# test_basemap.R — Step-by-step basemap diagnostic
# Run blocks one at a time in RStudio; each print() shows what that stage
# produces so you can pinpoint exactly where rendering breaks.
#
# Expected progression:
#   Step 1: gray county fill + hillshade terrain
#   Step 2: + water (lakes, rivers)
#   Step 3: + roads (gray casing + white fill)
#   Step 4: + transit (TRAX coloured dashes, commuter rail)
#   Step 5: + labels (street names, city names)
#   Step 6: full basemap from build_ugrc_map()
#   Step 7: + single focal tract outline (the critical compositing test)
#   Step 8: + coord_sf clipped to tract extent
#   Step 9: full make_tract_map() equivalent

# ── 0. Setup ──────────────────────────────────────────────────────────────────
source("src/ugrc_basemap.R")
ugrc_map_init()

# Fixed test coordinates — tract 49035102900, zoom 13, EPSG:3857
TEST_GEOID <- "49035102900"
TEST_BBOX  <- sf::st_bbox(
  c(xmin = -12458054, ymin = 4971567,
    xmax = -12454962, ymax = 4975862),
  crs = sf::st_crs(3857L)
)
TEST_ZOOM  <- 13L

cat(sprintf("bbox  xmin=%.0f  ymin=%.0f  xmax=%.0f  ymax=%.0f\n",
            TEST_BBOX["xmin"], TEST_BBOX["ymin"],
            TEST_BBOX["xmax"], TEST_BBOX["ymax"]))
cat(sprintf("zoom  %d\n", TEST_ZOOM))

# ── 1. Tile fetch ─────────────────────────────────────────────────────────────
centre <- .bbox_centre_wgs84(TEST_BBOX)
cat(sprintf("centre  lon=%.5f  lat=%.5f\n", centre$lon, centre$lat))
# EXPECTED: lon ≈ -111.9, lat ≈ 40.7  (Utah Valley)
# FAILURE:  lon ≈ -12456508, lat ≈ 4973714  → CRS bug still present

coords <- get_tile_xyz(centre$lon, centre$lat, TEST_ZOOM)
cat(sprintf("tile  Z=%d  X=%d  Y=%d\n", coords$z, coords$x, coords$y))
# EXPECTED: Z=13 X≈1549 Y≈3079

.url <- function(svc) sprintf(
  "MVT:https://tiles.arcgis.com/tiles/99lidPhWCzftIe9K/arcgis/rest/services/%s/VectorTileServer/tile/%d/%d/%d.pbf",
  svc, coords$z, coords$y, coords$x
)
bu <- .url("LiteBase")
lu <- .url("LiteLabels")
hu <- .url("VectorHillshade")

# ── 2. Fetch layer groups ─────────────────────────────────────────────────────
ground    <- .fetch_ground(bu)
hillshade <- .fetch_hillshade(hu, TEST_ZOOM)
water     <- .fetch_water(bu, TEST_ZOOM)
rec       <- .fetch_recreation(bu, TEST_ZOOM)
roads_raw <- .fetch_roads(bu, TEST_ZOOM)
muni      <- .fetch_muni(bu, TEST_ZOOM)
transit   <- .fetch_transit(bu, TEST_ZOOM)
buildings <- .fetch_buildings(bu, TEST_ZOOM)
lbl_data  <- .fetch_labels(lu, bu, TEST_ZOOM)

cat(sprintf("counties=%d  hillshade=%d  roads=%d  interstates=%d\n",
            nrow(ground$counties), nrow(hillshade),
            nrow(roads_raw$roads), nrow(roads_raw$interstates)))
# ── CRS diagnostic — confirm data is in EPSG:3857 after the safe_read_mvt fix ──
cat(sprintf("counties CRS epsg: %s\n", sf::st_crs(ground$counties)$epsg))
cat(sprintf("counties bbox: xmin=%.0f ymin=%.0f xmax=%.0f ymax=%.0f\n",
            sf::st_bbox(ground$counties)["xmin"], sf::st_bbox(ground$counties)["ymin"],
            sf::st_bbox(ground$counties)["xmax"], sf::st_bbox(ground$counties)["ymax"]))
# EXPECTED after fix: epsg=3857, bbox in metres (xmin ≈ -12.5M, ymin ≈ 4.97M)
# BEFORE fix:         epsg=4326, bbox in degrees (xmin ≈ -112, ymin ≈ 40)
cat(sprintf("lakes=%d  rivers=%d  trax=%d  commuter=%d  buildings=%d\n",
            nrow(water$lakes), nrow(water$rivers),
            nrow(transit$trax), nrow(transit$commuter_rail), nrow(buildings)))
cat(sprintf("lbl_city=%d  lbl_street=%d\n",
            nrow(lbl_data$city), nrow(lbl_data$street)))

# ── STEP 1: Ground + hillshade ────────────────────────────────────────────────
p <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = ground$counties, fill = .C_CTY_FILL, color = NA) +
  ggplot2::theme_void() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = .C_CTY_FILL, color = NA))

if (nrow(hillshade) > 0) {
  for (sv in c("0","1","2","3")) {
    hs <- hillshade[!is.na(hillshade$map_symbol) & hillshade$map_symbol == sv, ]
    if (nrow(hs) > 0)
      p <- p + ggplot2::geom_sf(data=hs, fill=.HS_COLORS[[sv]], color=NA,
                                alpha=.hs_alpha(TEST_ZOOM, as.integer(sv)))
  }
}

cat("\n--- STEP 1: ground + hillshade ---\n")
print(p)  # should show light-gray county with terrain shading

# ── STEP 2: + water ───────────────────────────────────────────────────────────
lake_gsl   <- if (nrow(water$lakes)>0) water$lakes[water$lakes$map_symbol=="0",] else water$lakes[0,]
lake_other <- if (nrow(water$lakes)>0) water$lakes[water$lakes$map_symbol!="0",] else water$lakes[0,]
if (nrow(lake_gsl)   > 0) p <- p + ggplot2::geom_sf(data=lake_gsl,   fill=.C_GSL_FILL,  color="#BBBFBF", linewidth=.lw(1))
if (nrow(lake_other) > 0) p <- p + ggplot2::geom_sf(data=lake_other, fill=.C_LAKE_FILL, color=.C_LAKE_BDR, linewidth=.lw(1))
if (nrow(water$rivers)>0) p <- p + ggplot2::geom_sf(data=water$rivers, fill=.C_RIVER_FILL, color=.C_RIVER_BDR, linewidth=.lw(1))

cat("\n--- STEP 2: + water ---\n")
print(p)

# ── STEP 3: + roads ───────────────────────────────────────────────────────────
roads       <- roads_raw$roads
interstates <- roads_raw$interstates
.rsym <- function(sym) {
  if (nrow(roads)==0) return(roads[0,])
  roads[!is.na(roads$map_symbol) & roads$map_symbol==as.character(sym),]
}
road_cas_order <- c(4L,2L,3L,6L,5L,7L,1L,0L)
road_fil_order <- c(7L,5L,6L,4L,3L,2L,1L,0L)

for (si in road_cas_order) {
  if (TEST_ZOOM < .ROAD_MIN_ZOOM[as.character(si)]) next
  rd <- .rsym(si)
  if (nrow(rd)==0) next
  cas_col <- if (si==5L && TEST_ZOOM<11L) "#BABABA" else if (si==6L && TEST_ZOOM<11L) "#B0B0B0" else .C_ROAD_CAS
  p <- p + ggplot2::geom_sf(data=rd, color=cas_col, linewidth=.road_lw(TEST_ZOOM, si, "cas"))
}
for (si in road_fil_order) {
  if (TEST_ZOOM < .ROAD_MIN_ZOOM[as.character(si)]) next
  rd <- .rsym(si); if (nrow(rd)==0) next
  lwf <- .road_lw(TEST_ZOOM, si, "fil"); if (lwf<=0) next
  p <- p + ggplot2::geom_sf(data=rd, color=.C_ROAD_FIL, linewidth=lwf,
                             linetype=if(si==5L)"32" else "solid")
}
if (nrow(interstates)>0) {
  p <- p + ggplot2::geom_sf(data=interstates, color=.C_ROAD_CAS, linewidth=.road_lw(TEST_ZOOM,"0ir","cas"))
  lf <- .road_lw(TEST_ZOOM,"0ir","fil")
  if (lf>0) p <- p + ggplot2::geom_sf(data=interstates, color=.C_ROAD_FIL, linewidth=lf)
}

cat("\n--- STEP 3: + roads ---\n")
print(p)

# ── STEP 4: + transit ─────────────────────────────────────────────────────────
trax <- .prepare_trax(transit$trax, TEST_ZOOM)
if (nrow(transit$commuter_rail)>0) {
  p <- p +
    ggplot2::geom_sf(data=transit$commuter_rail, color=.C_TR_CAS, linewidth=.lw(3.333)) +
    ggplot2::geom_sf(data=transit$commuter_rail, color=.C_TR_FIL, linewidth=.lw(1.667)) +
    ggplot2::geom_sf(data=transit$commuter_rail, color=.C_FRONTRUNNER, linewidth=.lw(1.667), linetype="42")
}
if (nrow(trax)>0) {
  lw_cas <- .trax_lw(TEST_ZOOM,"cas"); lw_fil <- .trax_lw(TEST_ZOOM,"fil")
  for (sym_v in unique(trax$map_symbol)) {
    tr <- trax[trax$map_symbol==sym_v,]; if (nrow(tr)==0) next
    cv <- tr$trax_color[1]
    p <- p + ggplot2::geom_sf(data=tr, color=.C_TR_CAS, linewidth=lw_cas)
    p <- p + ggplot2::geom_sf(data=tr, color=.C_TR_FIL, linewidth=lw_fil)
    if (cv != .C_TRAX_DEF)
      p <- p + ggplot2::geom_sf(data=tr, color=cv, linewidth=lw_fil, linetype=.trax_lty(TEST_ZOOM,sym_v))
    else
      p <- p + ggplot2::geom_sf(data=tr, color=cv, linewidth=lw_fil)
  }
}

cat("\n--- STEP 4: + transit ---\n")
print(p)

# ── STEP 5: + basic street labels ─────────────────────────────────────────────
str_lbl <- extract_label_coords(.filter_street_labels(lbl_data$street, TEST_ZOOM))
if (nrow(str_lbl)>0 && any(!is.na(str_lbl$map_label))) {
  cls_i <- as.integer(suppressWarnings(median(str_lbl$map_label_class, na.rm=TRUE)))
  sz    <- .sz_street(TEST_ZOOM, if(is.na(cls_i)) NA_integer_ else cls_i)
  p <- .lbl(p, str_lbl, col=.C_STR, halo=.C_STR_HALO, sz=sz, bgr=.bgr(1.333,sz), face="bold", fam=.F_STREET)
}
city_lbl <- extract_label_coords(.filter_city_labels(lbl_data$city, TEST_ZOOM))
if (nrow(city_lbl)>0 && any(!is.na(city_lbl$map_label))) {
  cls_i <- as.integer(suppressWarnings(median(city_lbl$map_label_class, na.rm=TRUE)))
  sz    <- .sz_city(TEST_ZOOM, if(is.na(cls_i)) 0L else cls_i)
  p <- .lbl(p, city_lbl, col=.C_CITY, halo=.C_CITY_HALO, sz=sz, bgr=.bgr(1.333,sz), face="bold", fam=.F_CITY)
}

cat("\n--- STEP 5: + labels ---\n")
print(p)

# ── STEP 6: full build_ugrc_map() ─────────────────────────────────────────────
cat("\n--- STEP 6: build_ugrc_map() standalone (no coord_sf) ---\n")
bm <- build_ugrc_map(TEST_BBOX, TEST_ZOOM, crs=3857L, verbose=TRUE)
cat(sprintf("bm has %d layer(s)\n", length(bm$layers)))
print(bm)
# EXPECTED: basemap visible over the full tile extent, NO coord crop

# ── STEP 7: + coord_sf only ───────────────────────────────────────────────────
cat("\n--- STEP 7: bm + coord_sf (crop to bbox, no OZ layers yet) ---\n")
p7 <- bm +
  ggplot2::coord_sf(
    xlim = c(TEST_BBOX["xmin"], TEST_BBOX["xmax"]),
    ylim = c(TEST_BBOX["ymin"], TEST_BBOX["ymax"]),
    crs  = sf::st_crs(3857L), expand = FALSE
  )
print(p7)
# EXPECTED: basemap cropped to bbox extent
# FAILURE:  blank white → coord_sf alone is the problem

# ── STEP 8: + one OZ layer then coord_sf ──────────────────────────────────────
cat("\n--- STEP 8: bm + one sf layer + coord_sf ---\n")
# Load the focal tract in 3857 for the test
focal_test <- sf::st_transform(
  dplyr::filter(oz_tracts, GEOID == TEST_GEOID),
  sf::st_crs(3857L)
)
p8 <- bm +
  ggplot2::geom_sf(data=focal_test, fill=NA, color="red", linewidth=1.5) +
  ggplot2::coord_sf(
    xlim = c(TEST_BBOX["xmin"], TEST_BBOX["xmax"]),
    ylim = c(TEST_BBOX["ymin"], TEST_BBOX["ymax"]),
    crs  = sf::st_crs(3857L), expand = FALSE
  )
print(p8)
# EXPECTED: basemap + red focal tract outline
# FAILURE:  just red outline on white → basemap lost after + geom_sf

# ── STEP 9: make_tract_map() ─────────────────────────────────────────────────
cat("\n--- STEP 9: make_tract_map() full ---\n")
p9 <- make_tract_map(TEST_GEOID)
print(p9)

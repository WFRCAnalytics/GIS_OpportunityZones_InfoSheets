# test_basemap.R — Step-by-step basemap diagnostic
# Prerequisite: run the libraries / prepare / load chunks in index.qmd so
# oz_tracts, wc_centers, sap_buffers, freeway_exits, uta_stops are available.
# Steps 0-7 are self-contained (only need src/ugrc_basemap.R).
# Steps 8-9 need the index.qmd data objects.

# ══════════════════════════════════════════════════════════════════════════════
# 0. Setup
# ══════════════════════════════════════════════════════════════════════════════
source("src/ugrc_basemap.R")
ugrc_map_init()

TEST_GEOID <- "49035102900"
TEST_ZOOM  <- 13L

# Tract 49035102900 in EPSG:3857 (pre-computed for the diagnostic)
TEST_BBOX <- sf::st_bbox(
  c(xmin = -12458054, ymin = 4971567,
    xmax = -12454962, ymax = 4975862),
  crs = sf::st_crs(3857L)
)

cat("── 0. Setup ──────────────────────────────────────────────────────────────\n")
cat(sprintf("bbox  xmin=%.0f  ymin=%.0f  xmax=%.0f  ymax=%.0f\n",
            TEST_BBOX["xmin"], TEST_BBOX["ymin"],
            TEST_BBOX["xmax"], TEST_BBOX["ymax"]))
cat(sprintf("zoom  %d\n\n", TEST_ZOOM))

# ══════════════════════════════════════════════════════════════════════════════
# 1. Centre + tile fetch
# ══════════════════════════════════════════════════════════════════════════════
cat("── 1. Tile fetch ─────────────────────────────────────────────────────────\n")
centre <- .bbox_centre_wgs84(TEST_BBOX)
cat(sprintf("centre  lon=%.5f  lat=%.5f\n", centre$lon, centre$lat))
stopifnot("centre lon must be ~ -111.9" = centre$lon > -113 && centre$lon < -110)
stopifnot("centre lat must be ~ 40.7"   = centre$lat >   39 && centre$lat <   42)

coords <- get_tile_xyz(centre$lon, centre$lat, TEST_ZOOM)
cat(sprintf("tile  Z=%d  X=%d  Y=%d\n\n", coords$z, coords$x, coords$y))

.mk_url <- function(svc) sprintf(
  "MVT:https://tiles.arcgis.com/tiles/99lidPhWCzftIe9K/arcgis/rest/services/%s/VectorTileServer/tile/%d/%d/%d.pbf",
  svc, coords$z, coords$y, coords$x
)
bu <- .mk_url("LiteBase")
lu <- .mk_url("LiteLabels")
hu <- .mk_url("VectorHillshade")

# ══════════════════════════════════════════════════════════════════════════════
# 2. Layer fetch + CRS verification
# ══════════════════════════════════════════════════════════════════════════════
cat("── 2. Layer fetch + CRS check ────────────────────────────────────────────\n")
ground    <- .fetch_ground(bu)
hillshade <- .fetch_hillshade(hu, TEST_ZOOM)
water     <- .fetch_water(bu, TEST_ZOOM)
roads_raw <- .fetch_roads(bu, TEST_ZOOM)
transit   <- .fetch_transit(bu, TEST_ZOOM)
lbl_data  <- .fetch_labels(lu, bu, TEST_ZOOM)

# CRS must be 3857 after the safe_read_mvt normalisation fix
ctys_crs  <- sf::st_crs(ground$counties)$epsg
ctys_bbox <- sf::st_bbox(ground$counties)
cat(sprintf("counties  CRS epsg=%s  xmin=%.0f  ymin=%.0f\n",
            ctys_crs, ctys_bbox["xmin"], ctys_bbox["ymin"]))
# PASS: epsg=3857, xmin ≈ -12460000 (metres)
# FAIL: epsg=4326, xmin ≈ -112 (degrees) → safe_read_mvt fix not sourced

stopifnot("counties CRS must be 3857 (re-source if failing)" =
          !is.na(ctys_crs) && ctys_crs == 3857L)

cat(sprintf("roads=%d  interstates=%d  hillshade=%d  trax=%d  commuter=%d\n",
            nrow(roads_raw$roads), nrow(roads_raw$interstates),
            nrow(hillshade), nrow(transit$trax), nrow(transit$commuter_rail)))
cat(sprintf("lbl_street=%d  lbl_city=%d\n\n", nrow(lbl_data$street), nrow(lbl_data$city)))

# ══════════════════════════════════════════════════════════════════════════════
# 3. Build basemap incrementally — print after each group
# ══════════════════════════════════════════════════════════════════════════════

# ── Step A: ground + hillshade ──────────────────────────────────────────────
p <- ggplot2::ggplot() +
  ggplot2::geom_sf(data=ground$counties, fill=.C_CTY_FILL, color=NA) +
  ggplot2::theme_void() +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill=.C_CTY_FILL, color=NA))

for (sv in c("0","1","2","3")) {
  hs <- hillshade[!is.na(hillshade$map_symbol) & hillshade$map_symbol==sv, ]
  if (nrow(hs)>0)
    p <- p + ggplot2::geom_sf(data=hs, fill=.HS_COLORS[[sv]], color=NA,
                               alpha=.hs_alpha(TEST_ZOOM, as.integer(sv)))
}
cat("── Step A: county fill + hillshade ──────────────────────────────────────\n")
print(p)   # light-gray + terrain shading

# ── Step B: + water ─────────────────────────────────────────────────────────
lake_gsl   <- if (nrow(water$lakes)>0) water$lakes[water$lakes$map_symbol=="0",] else water$lakes[0,]
lake_other <- if (nrow(water$lakes)>0) water$lakes[water$lakes$map_symbol!="0",] else water$lakes[0,]
if (nrow(lake_gsl)   >0) p <- p + ggplot2::geom_sf(data=lake_gsl,   fill=.C_GSL_FILL,  color="#BBBFBF", linewidth=.lw(1))
if (nrow(lake_other) >0) p <- p + ggplot2::geom_sf(data=lake_other, fill=.C_LAKE_FILL, color=.C_LAKE_BDR, linewidth=.lw(1))
if (nrow(water$rivers)>0) p <- p + ggplot2::geom_sf(data=water$rivers, fill=.C_RIVER_FILL, color=.C_RIVER_BDR, linewidth=.lw(1))
cat("\n── Step B: + water ──────────────────────────────────────────────────────\n")
print(p)

# ── Step C: + roads ─────────────────────────────────────────────────────────
roads       <- roads_raw$roads
interstates <- roads_raw$interstates
.rsym <- function(sym) {
  if (nrow(roads)==0) return(roads[0,])
  roads[!is.na(roads$map_symbol) & roads$map_symbol==as.character(sym), ]
}
for (si in c(4L,2L,3L,6L,5L,7L,1L,0L)) {   # casing pass
  if (TEST_ZOOM < .ROAD_MIN_ZOOM[as.character(si)]) next
  rd <- .rsym(si); if (nrow(rd)==0) next
  cas_col <- if (si==6L && TEST_ZOOM<11L) "#B0B0B0" else .C_ROAD_CAS
  p <- p + ggplot2::geom_sf(data=rd, color=cas_col, linewidth=.road_lw(TEST_ZOOM,si,"cas"))
}
for (si in c(7L,5L,6L,4L,3L,2L,1L,0L)) {   # fill pass
  if (TEST_ZOOM < .ROAD_MIN_ZOOM[as.character(si)]) next
  rd <- .rsym(si); if (nrow(rd)==0) next
  lwf <- .road_lw(TEST_ZOOM,si,"fil"); if (lwf<=0) next
  p <- p + ggplot2::geom_sf(data=rd, color=.C_ROAD_FIL, linewidth=lwf,
                              linetype=if(si==5L)"32" else "solid")
}
if (nrow(interstates)>0) {
  p <- p + ggplot2::geom_sf(data=interstates, color=.C_ROAD_CAS, linewidth=.road_lw(TEST_ZOOM,"0ir","cas"))
  lf <- .road_lw(TEST_ZOOM,"0ir","fil")
  if (lf>0) p <- p + ggplot2::geom_sf(data=interstates, color=.C_ROAD_FIL, linewidth=lf)
}
cat("\n── Step C: + roads ──────────────────────────────────────────────────────\n")
print(p)

# ── Step D: + transit ───────────────────────────────────────────────────────
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
    if (cv!=.C_TRAX_DEF) p <- p + ggplot2::geom_sf(data=tr, color=cv, linewidth=lw_fil, linetype=.trax_lty(TEST_ZOOM,sym_v))
    else                  p <- p + ggplot2::geom_sf(data=tr, color=cv, linewidth=lw_fil)
  }
}
cat("\n── Step D: + transit ────────────────────────────────────────────────────\n")
print(p)

# ── Step E: + street + city labels ──────────────────────────────────────────
str_lbl <- extract_label_coords(.filter_street_labels(lbl_data$street, TEST_ZOOM))
if (nrow(str_lbl)>0 && any(!is.na(str_lbl$map_label))) {
  cls_i <- as.integer(suppressWarnings(median(str_lbl$map_label_class, na.rm=TRUE)))
  sz <- .sz_street(TEST_ZOOM, if(is.na(cls_i)) NA_integer_ else cls_i)
  p <- .lbl(p, str_lbl, col=.C_STR, halo=.C_STR_HALO, sz=sz, bgr=.bgr(1.333,sz), face="bold", fam=.F_STREET)
}
city_lbl <- extract_label_coords(.filter_city_labels(lbl_data$city, TEST_ZOOM))
if (nrow(city_lbl)>0 && any(!is.na(city_lbl$map_label))) {
  cls_i <- as.integer(suppressWarnings(median(city_lbl$map_label_class, na.rm=TRUE)))
  sz <- .sz_city(TEST_ZOOM, if(is.na(cls_i)) 0L else cls_i)
  p <- .lbl(p, city_lbl, col=.C_CITY, halo=.C_CITY_HALO, sz=sz, bgr=.bgr(1.333,sz), face="bold", fam=.F_CITY)
}
cat("\n── Step E: + labels ─────────────────────────────────────────────────────\n")
print(p)

# ══════════════════════════════════════════════════════════════════════════════
# 4. build_ugrc_map() full
# ══════════════════════════════════════════════════════════════════════════════
cat("\n── Step F: build_ugrc_map() — no coord_sf yet ───────────────────────────\n")
bm <- build_ugrc_map(TEST_BBOX, TEST_ZOOM, crs=3857L, verbose=TRUE)
cat(sprintf("bm layers: %d\n", length(bm$layers)))
print(bm)   # full basemap, auto-extent from data

# ══════════════════════════════════════════════════════════════════════════════
# 5. coord_sf crop — the key compositing test
# ══════════════════════════════════════════════════════════════════════════════
cat("\n── Step G: bm + coord_sf crop only ─────────────────────────────────────\n")
p_g <- bm + ggplot2::coord_sf(
  xlim = c(TEST_BBOX[["xmin"]], TEST_BBOX[["xmax"]]),   # [[ avoids named-vector issues
  ylim = c(TEST_BBOX[["ymin"]], TEST_BBOX[["ymax"]]),
  crs  = sf::st_crs(3857L), expand = FALSE
)
print(p_g)
# PASS: basemap visible, cropped to tract bbox
# FAIL: blank/gray → data not yet in 3857 (re-source after safe_read_mvt fix)

# ══════════════════════════════════════════════════════════════════════════════
# 6. Add OZ layers (requires index.qmd data objects)
# ══════════════════════════════════════════════════════════════════════════════
if (!exists("oz_tracts")) {
  message("oz_tracts not found — run index.qmd load chunks first, then re-run from here")
  stop("missing oz_tracts", call. = FALSE)
}

cat("\n── Step H: bm + focal tract only + coord_sf ─────────────────────────────\n")
focal_3857 <- sf::st_transform(dplyr::filter(oz_tracts, GEOID==TEST_GEOID), sf::st_crs(3857L))
p_h <- bm +
  ggplot2::geom_sf(data=focal_3857, fill=NA, color="red", linewidth=2) +
  ggplot2::coord_sf(
    xlim = c(TEST_BBOX[["xmin"]], TEST_BBOX[["xmax"]]),
    ylim = c(TEST_BBOX[["ymin"]], TEST_BBOX[["ymax"]]),
    crs  = sf::st_crs(3857L), expand = FALSE
  )
print(p_h)
# PASS: basemap + red tract outline visible

cat("\n── Step I: full OZ overlay inline (make_tract_map equivalent) ───────────\n")
map_crs_sf <- sf::st_crs(3857L)
focal      <- sf::st_transform(dplyr::filter(oz_tracts, GEOID==TEST_GEOID), map_crs_sf)
other_oz   <- sf::st_transform(dplyr::filter(oz_tracts, GEOID!=TEST_GEOID), map_crs_sf)
wc_view    <- sf::st_filter(sf::st_transform(wc_centers,    map_crs_sf), sf::st_as_sfc(TEST_BBOX))
exits_view <- sf::st_filter(sf::st_transform(freeway_exits, map_crs_sf), sf::st_as_sfc(TEST_BBOX))
stops_view <- sf::st_filter(sf::st_transform(uta_stops,     map_crs_sf), sf::st_as_sfc(TEST_BBOX))
sap_view   <- sf::st_filter(sf::st_transform(sap_buffers,   map_crs_sf), sf::st_as_sfc(TEST_BBOX))
other_view <- sf::st_filter(other_oz, sf::st_as_sfc(TEST_BBOX))
sap_diss   <- if (nrow(sap_view)>0L) sf::st_union(sap_view) else sap_view
wc_colors  <- c(
  "Metropolitan Center"="#bc5f8c", "Urban Center"="#ec8369", "City Center"="#f5b86e",
  "Neighborhood Center"="#fae55c", "Education District"="#cbe3e1",
  "Employment District"="#f6c4da", "Industrial District"="#eadbf4",
  "Retail District"="#f8dddf",     "Special District"="#d3d3d3"
)
cat(sprintf("wc=%d  other_oz=%d  exits=%d  stops=%d  sap=%d\n",
            nrow(wc_view), nrow(other_view), nrow(exits_view),
            nrow(stops_view), nrow(sap_view)))

p_i <- bm +
  ggplot2::geom_sf(data=wc_view,    ggplot2::aes(fill=CenterType), color=NA, alpha=0.6) +
  ggplot2::scale_fill_manual(values=wc_colors, name=NULL, na.value="#C8C0C0", drop=TRUE) +
  ggplot2::geom_sf(data=other_view, fill=NA, color="#C4A43A",  linewidth=0.3) +
  ggplot2::geom_sf(data=sap_diss,   fill=NA, color="#0072b5",  linewidth=0.65, linetype="dashed") +
  ggplot2::geom_sf(data=focal, fill=NA, color="white",   linewidth=2.8) +
  ggplot2::geom_sf(data=focal, fill=NA, color="#003b4f", linewidth=1.6) +
  ggplot2::geom_sf(data=exits_view, shape=17, color="#3A4A5A", size=5) +
  ggplot2::geom_sf(data=stops_view, shape=21, fill="#943030", color="white", size=5, stroke=0.4) +
  ggplot2::coord_sf(
    xlim = c(TEST_BBOX[["xmin"]], TEST_BBOX[["xmax"]]),
    ylim = c(TEST_BBOX[["ymin"]], TEST_BBOX[["ymax"]]),
    crs  = map_crs_sf, expand = FALSE
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    plot.background = ggplot2::element_rect(fill="white", color=NA),
    plot.margin     = ggplot2::margin(0,0,0,0)
  )
print(p_i)
# PASS: basemap + all OZ overlays; this is what make_tract_map should produce

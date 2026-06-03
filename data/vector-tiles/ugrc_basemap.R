# ugrc_basemap.R — Full UGRC LiteBase / LiteLabels / VectorHillshade basemap
# Implements all visual layers from root-LightBase.json, root-LiteLabels.json,
# and root-VectorHillshade.json as a ggplot2 map for any lon/lat/zoom.
# Label halos via shadowtext::geom_shadowtext (bg.color = JSON text-halo-color).

# ══════════════════════════════════════════════════════════════════════════════
# 1. PACKAGES & FONTS
# ══════════════════════════════════════════════════════════════════════════════
library(sf)
library(ggplot2)
library(dplyr)
library(showtext)
library(shadowtext)

font_add_google("Poller One", "poller")
# System fonts (Windows) — fail silently if absent
tryCatch(font_add("Arial", "C:/Windows/Fonts/arial.ttf"), error = function(e) {
  NULL
})
tryCatch(
  font_add("Segoe UI", "C:/Windows/Fonts/segoeui.ttf"),
  error = function(e) NULL
)
tryCatch(
  font_add("Yu Gothic", "C:/Windows/Fonts/YuGothB.ttc"),
  error = function(e) NULL
)
showtext_auto()

.F_COUNTY <- "poller"
.F_CITY <- if ("Yu Gothic" %in% sysfonts::font_families()) {
  "Yu Gothic"
} else {
  "sans"
}
.F_HWY <- if ("Arial" %in% sysfonts::font_families()) "Arial" else "sans"
.F_STREET <- if ("Segoe UI" %in% sysfonts::font_families()) {
  "Segoe UI"
} else {
  "sans"
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. STYLE CONSTANTS
# ══════════════════════════════════════════════════════════════════════════════

# ── Unit conversion (96 DPI CSS pixels → ggplot2 units) ───────────────────────
# These are the ONLY conversion points in the file. Every linewidth and text
# size must call .lw() or .sz() — never embed raw numbers.
#
#   Linewidth: 1 CSS px @ 96 DPI = 25.4/96 = 0.26458 mm  (ggplot2 linewidth=)
#   Text size: 1 CSS px → size = px × 0.75 / 2.835 = px × 0.26455 (ggplot2 size=)
#              derivation: JSON px → pt (×0.75 at 96 DPI) → ggplot2 size units (÷2.835 pt/mm)
.PX_MM <- 25.4 / 96          # 0.26458  mm per CSS pixel
.PX_SZ <- 0.75  / 2.835      # 0.26455  ggplot2 size units per CSS pixel

.lw <- function(px) px * .PX_MM   # CSS px → ggplot2 linewidth (mm)
.sz <- function(px) px * .PX_SZ   # CSS px → ggplot2 text size

# Ground
.C_WEST_FILL <- "#FFFFFF"
.C_WEST_BDR <- "#B2B2B2"
.C_UTAH_HI <- rgb(217, 214, 210, 77, maxColorValue = 255) # z >= 8
.C_UTAH_LO <- rgb(230, 227, 223, 51, maxColorValue = 255) # z <  8
.C_CTY_FILL <- "#F7F7F7"

# Water
.C_GSL_FILL <- rgb(210, 217, 217, 191, maxColorValue = 255) # rgba(210,217,217,0.75)
.C_LAKE_FILL <- "#B7C7C7"
.C_LAKE_BDR <- "#95A3A6"
.C_RIVER_FILL <- "#ACBFBF"
.C_RIVER_BDR <- "#8DA2A6"
.C_STREAM <- "#A3BFBF"

# Parks & Recreation
.C_PARK_FILL <- rgb(206, 217, 184, 128, maxColorValue = 255) # 0.50 alpha
.C_PARK_BDR <- rgb(106, 128, 64, 102, maxColorValue = 255) # 0.40 alpha
.C_SKI_BDR <- "#999796"

# National Monuments / State Parks
.C_NM_OUTER <- rgb(204, 197, 194, 191, maxColorValue = 255) # 0.75
.C_NM_MID <- "#C7C0BD"
.C_NM_INNER <- "#999391" # National Monument inner / State Park z11+ inner
.C_SP_OUTER <- rgb(199, 199, 199, 191, maxColorValue = 255) # 0.75
.C_SP_MID <- "#B6BCBF"
.C_SP_BDR <- "#8C867E" # State Park z13+ inner border
.C_SP_INNER <- "#8C867E" # State Park z8-11 inner (JSON: #8C867E, sym 1 z8-11)

# Cemetery outline differs from golf/park: JSON rgba(104,128,57,0.4) vs golf rgba(106,128,64,0.4)
.C_CEMETERY_BDR <- rgb(104, 128, 57, 102, maxColorValue = 255) # rgba(104,128,57,0.4)

# Contours
.C_CTR_MINOR <- "#805E4D" # 50 / 100 / 200 / 250 ft
.C_CTR_500 <- "#664E42"
.C_CTR_1000 <- "#59443A"

# Hillshade — solid base colours; transparency applied per-symbol at draw time.
# JSON rgba() alpha × fill-opacity computed into .hs_alpha_tbl, NOT embedded here.
.HS_COLORS <- c(
  "0" = "#A6A4A2",
  "1" = "#BFBEBB",
  "2" = "#D9D8D4",
  "3" = "#E6E4E1"
)

# Effective alpha per (zoom_band × _symbol): rgba_alpha × fill-opacity
# z < 11  (90m):  JSON spec = solid (no fill-opacity). Using 0.70 in ggplot2 so
#                 county borders and fills remain legible underneath the hillshade.
# z 11-14 (30m / 10m z13-14): fill-opacity=0.8, rgba solid → 0.80
# z 14-15 (10m):  sym 0-2 rgba(x,0.5)×0.8=0.40; sym 3 rgba(x,0.7)×0.8=0.56
# z 15+   (10m):  sym 0-2 rgba(x,0.3)×0.8=0.24; sym 3 rgba(x,0.5)×0.8=0.40
.HS_ALPHA_TBL <- list(
  `0` = c(lt11 = 0.70, b11_14 = 0.80, b14_15 = 0.40, ge15 = 0.24),
  `1` = c(lt11 = 0.70, b11_14 = 0.80, b14_15 = 0.40, ge15 = 0.24),
  `2` = c(lt11 = 0.70, b11_14 = 0.80, b14_15 = 0.40, ge15 = 0.24),
  `3` = c(lt11 = 0.70, b11_14 = 0.80, b14_15 = 0.56, ge15 = 0.40)
)

.hs_alpha <- function(zoom, sym) {
  band <- if (zoom < 11) {
    "lt11"
  } else if (zoom < 14) {
    "b11_14"
  } else if (zoom < 15) {
    "b14_15"
  } else {
    "ge15"
  }
  tbl <- .HS_ALPHA_TBL[[as.character(sym)]]
  if (is.null(tbl)) {
    return(0.80)
  }
  tbl[[band]]
}

.hs_res_layer <- function(zoom) {
  if (zoom < 11) {
    "ShadePolygons_90Meter_dissolved"
  } else if (zoom < 13) {
    "ShadePolygons_30Meter"
  } else {
    "ShadePolygons_10Meter"
  }
}

# Municipalities
.C_MUNI1_BDR <- "#807B79"
.C_M2_FILL <- rgb(153, 148, 138, 26, maxColorValue = 255) # 0.10
.C_M2_OUT <- list(
  "0" = rgb(144, 120, 161, 38, maxColorValue = 255), # purple 0.15
  "1" = rgb(156, 179, 134, 38, maxColorValue = 255), # green
  "2" = rgb(134, 179, 179, 38, maxColorValue = 255), # teal
  "3" = rgb(179, 134, 149, 38, maxColorValue = 255) # mauve
)
.C_M2_IN <- list(
  "0" = rgb(153, 128, 171, 51, maxColorValue = 255), # purple 0.20
  "1" = rgb(156, 179, 134, 51, maxColorValue = 255),
  "2" = rgb(134, 179, 179, 51, maxColorValue = 255),
  "3" = rgb(179, 134, 149, 51, maxColorValue = 255)
)

# Buildings — JSON fill #F2F0ED; border = line layer rgba(140,133,133,0.35)
# (NOT the shadow fill rgba(128,123,121,0.25) which is a separate layer)
.C_BLDG_FILL <- "#F2F0ED"
.C_BLDG_BDR <- rgb(140, 133, 133, 89, maxColorValue = 255) # rgba(140,133,133,0.35)

# Roads
.C_ROAD_CAS <- "#B3B3B3"
.C_ROAD_FIL <- "#FFFFFF"
.C_RAIL <- "#B3AFAF"
.C_TRAIL_CAS <- "#ADA5A5"

# Transit
.C_TR_CAS <- "#B2B2B2"
.C_TR_FIL <- "#FFFFFF"
# TRAX dashed line colours: JSON uses 0.8 opacity on all coloured lines
# Blue (sym 0-2): rgba(89,126,179,0.8); Green (sym 3-4): rgba(96,191,77,0.8)
# Green alt (sym 5): rgba(72,191,48,0.8); Red (sym 6-8): rgba(204,102,111,0.8)
# S-Line (sym 9): rgba(121,222,242,0.8); Default (sym 10): #828282 solid, no alpha
.C_TRAX_BLU <- rgb(89, 126, 179, 204, maxColorValue = 255) # rgba(89,126,179,0.8)
.C_TRAX_GRN <- rgb(96, 191, 77, 204, maxColorValue = 255) # rgba(96,191,77,0.8) sym 3-4
.C_TRAX_GRN5 <- rgb(72, 191, 48, 204, maxColorValue = 255) # rgba(72,191,48,0.8) sym 5
.C_TRAX_RED <- rgb(204, 102, 111, 204, maxColorValue = 255) # rgba(204,102,111,0.8)
.C_TRAX_SLN <- rgb(121, 222, 242, 204, maxColorValue = 255) # rgba(121,222,242,0.8)
.C_TRAX_DEF <- "#828282" # sym 10: solid gray, no dashed overlay
.C_FRONTRUNNER <- "#D2ACE6"

# Labels — exact JSON values
.C_CNTY_HI <- "#FAF8F5" # county class 1-2
.C_CNTY_LO <- "#F2F1ED" # county class 3
.C_CNTY_HALO <- "#8C8885"
.C_CITY <- "#807D79"
.C_CITY_HALO <- rgb(230, 228, 225, 128, maxColorValue = 255) # rgba(230,228,225,0.5)
.C_HWY_INT <- "#737270" # Interstate
.C_HWY_US <- "#666461" # US / State Highway
.C_HWY_ROAD <- "#595757" # ramp / road name
.C_HWY_HALO <- rgb(217, 206, 206, 140, maxColorValue = 255) # rgba(217,206,206,0.55)
.C_STR <- "#8C8989"
.C_STR_HALO <- rgb(230, 228, 225, 153, maxColorValue = 255) # rgba(230,228,225,0.60)

# Feature labels — from JSON paint properties
.C_WATER_LBL <- "#829599" # streams, lakes (blue-gray)
.C_WATER_HALO <- rgb(194, 204, 204, 128, maxColorValue = 255) # rgba(194,204,204,0.5) from JSON
.C_PARK_LBL <- "#8C8C77" # parks, golf, cemeteries
.C_PARK_HALO <- rgb(235, 235, 220, 128, maxColorValue = 255) # warm-gray halo
.C_MON_LBL <- "#807E7D" # monuments, state parks
.C_MON_HALO <- rgb(230, 228, 225, 128, maxColorValue = 255) # same as city halo
.C_SKI_LBL <- "#8C8989" # SkiAreaLocations label (JSON #8C8989)
.C_SKI_HALO <- rgb(242, 239, 237, 140, maxColorValue = 255) # rgba(242,239,237,0.55)

# POI geometry colours — exact from JSON paint properties
.C_AIRPORT_LINE <- rgb(191, 134, 162, 128, maxColorValue = 255) # Airports rgba(191,134,162,0.5)
.C_SKI_LIFT_LINE <- "#BF8F8F" # SkiLifts line

# POI point-marker stroke colours (circle-stroke-color from JSON)
.C_SCHOOL_K12_PT <- "#808080" # Schools_PreKto12 circle stroke
.C_SCHOOL_HE_PT <- "#807979" # Schools_HigherEducation circle stroke
.C_HEALTH_PT <- "#BFA3A3" # LicensedHealthCareFacilities icon colour
.C_TRAIL_PT <- "#8C8B89" # Trailheads icon colour
.C_GNIS_PT <- "#807C7C" # PlaceNamesGNIS2010 icon colour
.C_OSM_PT <- "#8C8B89" # OpenSourcePlaces icon colour

# POI label colours — exact from JSON text-color / text-halo-color
.C_POI_LBL <- "#8C8989" # default (airport, school, ski loc, OSM, health)
.C_POI_HALO_STD <- rgb(230, 228, 225, 153, maxColorValue = 255) # rgba(230,228,225,0.60)
.C_POI_HALO_SCH <- rgb(242, 239, 237, 140, maxColorValue = 255) # rgba(242,239,237,0.55) schools / ski locs
.C_POI_HALO_HE <- rgb(230, 226, 218, 140, maxColorValue = 255) # rgba(230,226,218,0.55) higher ed
.C_TRAIL_LBL <- "#807D7D" # Trailheads
.C_TRAIL_HALO_PT <- rgb(242, 236, 233, 153, maxColorValue = 255) # rgba(242,236,233,0.60)
.C_GNIS_LBL <- "#807F7D" # PlaceNamesGNIS2010
.C_GNIS_HALO <- rgb(230, 228, 225, 140, maxColorValue = 255) # rgba(230,228,225,0.55)
.C_BAY_LBL <- "#829599" # PlaceNamesGNIS2010 - Bay Labels
.C_BAY_HALO <- rgb(194, 204, 204, 128, maxColorValue = 255) # rgba(194,204,204,0.50)
.C_SKLIFT_LBL <- "#FFFEFA" # SkiLifts/label text (near-white)
.C_SKLIFT_HALO <- "#B39898" # SkiLifts/label halo
.C_CTR_LBL <- "#8C8989" # Contours/label
.C_CTR_HALO <- rgb(230, 228, 225, 128, maxColorValue = 255) # rgba(230,228,225,0.50)

# ══════════════════════════════════════════════════════════════════════════════
# 3. ROAD LINEWIDTH LOOKUP TABLES
# ══════════════════════════════════════════════════════════════════════════════
# Raw JSON CSS pixel values extracted directly from root-LightBase.json.
# .road_lw() interpolates and then applies .lw() to convert to mm.
# sym 0 = Interstates (from Roads-Interstates layer)
# sym 1 = Ramps/Collectors, sym 2 = US Hwy, sym 3 = State Hwy
# sym 4 = Major Local Paved, sym 5 = Major Local Unpaved
# sym 6 = Other Federal Aid, sym 7 = Local Roads

.RSTOPS <- list(
  `0` = list(  # Interstates (Roads - Interstates and Ramps - white version)
    z   = c(  6,    8,   10,   12,   14,   16,   18),
    cas = c(3.00, 4.67, 6.67, 8.00, 9.33,20.00,26.67),
    fil = c(1.33, 2.67, 4.67, 6.00, 7.33,17.33,24.00)),
  `1` = list(  # Ramps and Collectors
    z   = c( 11,   12,   13,   14,   15,   16,   18),
    cas = c(2.67, 3.67, 4.67, 6.00, 7.67, 9.33,16.00),
    fil = c(1.33, 2.00, 3.33, 4.67, 6.00, 7.33,14.67)),
  `2` = list(  # US Highways
    z   = c(  6,    8,   10,   12,   14,   16,   18),
    cas = c(2.00, 3.00, 4.33, 5.33, 8.00,15.33,21.33),
    fil = c(1.00, 1.67, 2.67, 4.00, 6.00,13.33,18.67)),
  `3` = list(  # State Highways
    z   = c(  7,    9,   11,   13,   15,   17),
    cas = c(2.00, 3.00, 4.33, 6.67,11.33,20.00),
    fil = c(0.93, 1.67, 3.33, 4.67, 8.67,17.33)),
  `4` = list(  # Major Local Roads Paved
    z   = c(  9,   10,   11,   12,   13,   14,   15,   17),
    cas = c(2.33, 2.67, 3.33, 4.00, 5.00, 6.00, 8.00,20.00),
    fil = c(1.07, 1.33, 2.00, 2.67, 3.33, 4.67, 6.67,17.33)),
  `5` = list(  # Major Local Roads Not Paved
    z   = c( 11,   12,   13,   14,   15,   17),
    cas = c(2.67, 3.33, 4.00, 6.00, 6.67,13.33),
    fil = c(1.33, 2.00, 2.67, 4.67, 5.33,12.00)),
  `6` = list(  # Other Federal Aid Roads
    z   = c( 11,   12,   13,   14,   15,   17),
    cas = c(2.67, 3.33, 4.67, 6.00, 6.67,20.00),
    fil = c(1.33, 2.00, 3.33, 4.67, 5.33,17.33)),
  `7` = list(  # Local Roads
    z   = c( 12,   13,   14,   15,   16,   17),
    cas = c(2.67, 3.33, 4.00, 6.00, 9.33,14.67),
    fil = c(1.33, 2.00, 2.67, 4.67, 8.00,13.33))
)

.road_lw <- function(zoom, sym, type = "cas") {
  k <- as.character(sym)
  if (!k %in% names(.RSTOPS)) return(.lw(2.67))  # safe mid-zoom default
  st <- .RSTOPS[[k]]
  .lw(approx(st$z, st[[type]], xout = zoom, rule = 2)$y)
}

.ROAD_MIN_ZOOM <- c(
  `0` = 6,
  `1` = 11,
  `2` = 6,
  `3` = 7,
  `4` = 9,
  `5` = 9,
  `6` = 11,
  `7` = 12
)

# ══════════════════════════════════════════════════════════════════════════════
# 4. TILE COORDINATE HELPER
# ══════════════════════════════════════════════════════════════════════════════

get_tile_xyz <- function(lon, lat, zoom) {
  n <- 2^zoom
  x <- floor(n * (lon + 180) / 360)
  lat_rad <- lat * pi / 180
  y <- floor((n / 2) * (1 - log(tan(lat_rad) + 1 / cos(lat_rad)) / pi))
  list(x = x, y = y, z = zoom)
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. MVT FETCH HELPERS
# ══════════════════════════════════════════════════════════════════════════════

.empty_sf <- function() {
  sf::st_sf(
    map_label = character(0),
    map_label_class = numeric(0),
    map_symbol = character(0),
    geometry = sf::st_sfc(crs = 3857)
  )
}

safe_read_mvt <- function(url, layer_name) {
  avail <- tryCatch(sf::st_layers(url)$name, error = function(e) character(0))
  alt <- gsub("/", "_", layer_name)
  actual <- if (layer_name %in% avail) {
    layer_name
  } else if (alt %in% avail) {
    alt
  } else {
    NULL
  }
  if (is.null(actual)) {
    return(.empty_sf())
  }

  lyr <- tryCatch(
    sf::st_read(url, layer = actual, quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(lyr) || nrow(lyr) == 0) {
    return(.empty_sf())
  }
  cn <- names(lyr)

  # Standardise label — coalesce all name-like columns (no dplyr::coalesce needed)
  nm <- grep("name", cn, ignore.case = TRUE, value = TRUE)
  if (length(nm) > 0) {
    vecs <- lapply(nm, function(col) {
      v <- as.character(lyr[[col]])
      v[v == "" | is.na(v)] <- NA
      v
    })
    lyr$map_label <- Reduce(function(a, b) ifelse(is.na(a), b, a), vecs)
  } else {
    lyr$map_label <- NA_character_
  }

  # Standardise label class
  cl <- grep("label_class", cn, ignore.case = TRUE, value = TRUE)
  lyr$map_label_class <- if (length(cl) > 0) {
    as.numeric(lyr[[cl[1]]])
  } else {
    NA_real_
  }

  # Standardise symbol — try exact _symbol / symbol first; broad fallback for edge cases
  sy <- grep("^_?symbol$", cn, ignore.case = TRUE, value = TRUE)
  if (length(sy) == 0) {
    sy <- grep("symbol", cn, ignore.case = TRUE, value = TRUE)
  }
  lyr$map_symbol <- if (length(sy) > 0) as.character(lyr[[sy[1]]]) else "0"

  lyr
}

extract_label_coords <- function(sf_obj) {
  if (nrow(sf_obj) > 0) {
    co <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(
      sf_obj
    ))))
    sf_obj$X <- co[, 1]
    sf_obj$Y <- co[, 2]
  } else {
    sf_obj$X <- numeric(0)
    sf_obj$Y <- numeric(0)
  }
  sf_obj
}

# ══════════════════════════════════════════════════════════════════════════════
# 6. LAYER FETCH FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════

.fetch_ground <- function(bu) {
  list(
    western_states = safe_read_mvt(bu, "WesternStates"),
    utah = safe_read_mvt(bu, "Utah"),
    counties = safe_read_mvt(bu, "Counties")
  )
}

.fetch_water <- function(bu, zoom) {
  list(
    lakes = safe_read_mvt(bu, "LakesNHDHighRes_OLD"),
    gsl = safe_read_mvt(bu, "GSLWaterLevel2016_simplified50"),
    rivers = if (zoom >= 9) {
      safe_read_mvt(bu, "UtahMajorRiversPoly")
    } else {
      .empty_sf()
    },
    streams = if (zoom >= 11) {
      safe_read_mvt(bu, "StreamsNHDHighRes")
    } else {
      .empty_sf()
    }
  )
}

.fetch_recreation <- function(bu, zoom) {
  list(
    parks = if (zoom >= 10) safe_read_mvt(bu, "ParksLocal") else .empty_sf(),
    golf = if (zoom >= 10) safe_read_mvt(bu, "GolfCourses") else .empty_sf(),
    cemeteries = if (zoom >= 10) {
      safe_read_mvt(bu, "Cemeteries_Poly")
    } else {
      .empty_sf()
    },
    monuments = if (zoom >= 8) {
      safe_read_mvt(bu, "UtahParksAndMonuments")
    } else {
      .empty_sf()
    },
    ski = if (zoom >= 13) {
      safe_read_mvt(bu, "SkiAreaBoundaries")
    } else {
      .empty_sf()
    },
    trails = if (zoom >= 14) {
      safe_read_mvt(bu, "TrailsAndPathways")
    } else {
      .empty_sf()
    }
  )
}

.fetch_contours <- function(bu, zoom) {
  if (zoom < 9) {
    return(.empty_sf())
  }
  safe_read_mvt(bu, "Contours_10MeterDEM_50ft_generalized")
}

.fetch_roads <- function(bu, zoom) {
  list(
    roads = safe_read_mvt(bu, "Roads - white version"),
    interstates = safe_read_mvt(
      bu,
      "Roads - Interstates and Ramps - white version"
    ),
    railroads = if (zoom >= 11) safe_read_mvt(bu, "Railroads") else .empty_sf()
  )
}

.fetch_muni <- function(bu, zoom) {
  list(
    muni1 = if (zoom >= 11) {
      safe_read_mvt(bu, "Municipalities_Carto:1")
    } else {
      .empty_sf()
    },
    muni2 = if (zoom >= 12) {
      safe_read_mvt(bu, "Municipalities_Carto:2")
    } else {
      .empty_sf()
    }
  )
}

.fetch_transit <- function(bu, zoom) {
  list(
    trax = if (zoom >= 12) {
      safe_read_mvt(bu, "UTA_Trax_SingleLine")
    } else {
      .empty_sf()
    },
    commuter_rail = if (zoom >= 11) {
      safe_read_mvt(bu, "CommuterRailRoutes_UTA")
    } else {
      .empty_sf()
    },
    rail_stops = if (zoom >= 14) {
      safe_read_mvt(bu, "LightRailStations_UTA") # JSON minzoom: 14
    } else {
      .empty_sf()
    },
    cr_stops = if (zoom >= 14) {
      safe_read_mvt(bu, "CommuterRailStops_UTA") # JSON minzoom: 14
    } else {
      .empty_sf()
    }
  )
}

.fetch_buildings <- function(bu, zoom) {
  if (zoom >= 14) safe_read_mvt(bu, "Buildings") else .empty_sf()
}

.fetch_hillshade <- function(hu, zoom) safe_read_mvt(hu, .hs_res_layer(zoom))

# POI / location layers — all in base tile
.fetch_poi <- function(bu, zoom) {
  list(
    airports = if (zoom >= 10) safe_read_mvt(bu, "Airports") else .empty_sf(),
    airport_pts = if (zoom >= 9) {
      safe_read_mvt(bu, "AirportLocations")
    } else {
      .empty_sf()
    },
    schools_k12 = if (zoom >= 14) {
      safe_read_mvt(bu, "Schools_PreKto12")
    } else {
      .empty_sf()
    },
    schools_he = if (zoom >= 13) {
      safe_read_mvt(bu, "Schools_HigherEducation")
    } else {
      .empty_sf()
    },
    healthcare = if (zoom >= 14) {
      safe_read_mvt(bu, "LicensedHealthCareFacilities")
    } else {
      .empty_sf()
    },
    ski_locs = if (zoom >= 10) {
      safe_read_mvt(bu, "SkiAreaLocations")
    } else {
      .empty_sf()
    },
    ski_lifts = if (zoom >= 12) safe_read_mvt(bu, "SkiLifts") else .empty_sf(),
    trailheads = if (zoom >= 14) {
      safe_read_mvt(bu, "Trailheads")
    } else {
      .empty_sf()
    },
    gnis = if (zoom >= 12) {
      safe_read_mvt(bu, "PlaceNamesGNIS2010")
    } else {
      .empty_sf()
    },
    gnis_bay = if (zoom >= 11) {
      safe_read_mvt(bu, "PlaceNamesGNIS2010 - Bay Labels")
    } else {
      .empty_sf()
    },
    osm_places = if (zoom >= 13) {
      safe_read_mvt(bu, "OpenSourcePlaces")
    } else {
      .empty_sf()
    }
  )
}

.fetch_labels <- function(lu, bu, zoom) {
  list(
    # Administrative / road labels — from LiteLabels tile
    county = safe_read_mvt(lu, "Counties/label"),
    city = safe_read_mvt(lu, "CitiesTownsLocations_VT"),
    highway = safe_read_mvt(
      lu,
      "Roads - Interstates and Ramps - white version/label"
    ),
    street = if (zoom >= 8) {
      safe_read_mvt(lu, "Roads - white version/label")
    } else {
      .empty_sf()
    },

    # Feature labels — from LiteBase tile (these live in the base tile, not label tile)
    # Zoom gates match JSON minzoom for each label class
    streams = if (zoom >= 8) {
      safe_read_mvt(bu, "StreamsNHDHighRes/label")
    } else {
      .empty_sf()
    },
    lakes = if (zoom >= 8) {
      safe_read_mvt(bu, "LakesNHDHighRes_OLD/label")
    } else {
      .empty_sf()
    },
    gsl = if (zoom >= 7) {
      safe_read_mvt(bu, "GSLWaterLevel2016_simplified50/label")
    } else {
      .empty_sf()
    },
    monuments = if (zoom >= 8) {
      safe_read_mvt(bu, "UtahParksAndMonuments/label")
    } else {
      .empty_sf()
    },
    parks = if (zoom >= 13) {
      safe_read_mvt(bu, "ParksLocal/label")
    } else {
      .empty_sf()
    },
    golf = if (zoom >= 13) {
      safe_read_mvt(bu, "GolfCourses/label")
    } else {
      .empty_sf()
    },
    cemeteries = if (zoom >= 10) {
      safe_read_mvt(bu, "Cemeteries_Poly/label")
    } else {
      .empty_sf()
    },
    trails = if (zoom >= 14) {
      safe_read_mvt(bu, "TrailsAndPathways/label")
    } else {
      .empty_sf()
    },
    ski = if (zoom >= 13) {
      safe_read_mvt(bu, "SkiAreaBoundaries/label")
    } else {
      .empty_sf()
    },
    # Additional label layers (base tile, line-following — centroid placement used as fallback)
    ski_lift_lbl = if (zoom >= 12) {
      safe_read_mvt(bu, "SkiLifts/label")
    } else {
      .empty_sf()
    },
    ctr_lbl = if (zoom >= 13) {
      safe_read_mvt(bu, "Contours_10MeterDEM_50ft_generalized/label")
    } else {
      .empty_sf()
    }
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# 7. POST-FETCH PROCESSING
# ══════════════════════════════════════════════════════════════════════════════

.prepare_trax <- function(trax, zoom) {
  if (nrow(trax) == 0) {
    return(trax)
  }
  trax <- trax %>%
    dplyr::mutate(
      trax_color = dplyr::case_when(
        map_symbol %in% c("0", "1", "2") ~ .C_TRAX_BLU,
        map_symbol %in% c("3", "4") ~ .C_TRAX_GRN,
        map_symbol == "5" ~ .C_TRAX_GRN5, # distinct shade in JSON
        map_symbol %in% c("6", "7", "8") ~ .C_TRAX_RED,
        map_symbol == "9" ~ .C_TRAX_SLN,
        TRUE ~ .C_TRAX_DEF
      ),
      pixel_offset = dplyr::case_when(
        map_symbol == "1" ~ -2.33,
        map_symbol == "2" ~ -4.66,
        map_symbol %in% c("4", "7") ~ 2.33,
        map_symbol == "8" ~ 4.66,
        TRUE ~ 0
      ),
      meter_offset = pixel_offset * -1 * (156543.03 / (2^zoom))
    )
  trax <- suppressWarnings(sf::st_cast(trax, "LINESTRING"))
  shifted <- lapply(seq_len(nrow(trax)), function(i) {
    trax$geometry[[i]] + c(trax$meter_offset[i], trax$meter_offset[i])
  })
  trax$geometry <- sf::st_sfc(shifted, crs = sf::st_crs(trax))
  trax
}

# Stream helpers
.stream_sym <- function(streams, sym) {
  if (nrow(streams) == 0) {
    return(streams[0, ])
  }
  streams[
    !is.na(streams$map_symbol) & streams$map_symbol == as.character(sym),
  ]
}

.stream_lw <- function(zoom, sym) {
  # Raw JSON px values from StreamsNHDHighRes — converted via .lw()
  if (sym == 0L)       # Perennial Major: 1.0px z6-13, 1.33px z13-15, 2.0px z15+
    .lw(approx(c(6,9,11,13,15), c(1.0,1.0,1.0,1.333,2.0), xout=zoom, rule=2)$y)
  else if (sym == 1L)  # Perennial: 1.0px z11-14, 1.333px z14+
    .lw(approx(c(11,13,14), c(1.0,1.0,1.333), xout=zoom, rule=2)$y)
  else                 # Intermittent/Ephemeral: 0.667px z12-14, 1.0px z14+
    if (zoom >= 14L) .lw(1.0) else .lw(0.667)
}

# Stream dasharray → ggplot2 linetype (best available approximation)
# JSON: z12-13 "7 3 1 2 1 3", z13-14 longer, z14+ similar
.stream_lty <- function(zoom) if (zoom < 13) "longdash" else "dashed"

# Contour filter — JSON sym 3 (250ft) has a gap at z12-13 (not shown z12-13)
.filter_contours <- function(ctrs, zoom) {
  if (nrow(ctrs) == 0) {
    return(ctrs)
  }
  sym <- suppressWarnings(as.integer(ctrs$map_symbol))
  keep <- !is.na(sym) &
    ((sym == 0L & zoom >= 13) |
      (sym == 1L & zoom >= 12) |
      (sym == 2L & zoom >= 12) |
      (sym == 3L & (zoom >= 13 | (zoom >= 11 & zoom < 12))) | # gap at z12
      (sym == 4L & zoom >= 10) |
      (sym == 5L & zoom >= 9))
  ctrs[keep, ]
}

# Label filters
.filter_county_labels <- function(lbl, zoom) {
  if (nrow(lbl) == 0) {
    return(lbl)
  }
  cls <- lbl$map_label_class
  lbl[
    is.na(cls) |
      (cls == 1 & zoom == 9) |
      (cls == 2 & zoom == 10) |
      (cls == 3 & zoom >= 12 & zoom < 18),
  ]
}

.filter_highway_labels <- function(lbl, zoom) {
  if (nrow(lbl) == 0) {
    return(lbl)
  }
  cls <- lbl$map_label_class
  lbl[
    is.na(cls) |
      (cls == 0 & zoom >= 7) |
      (cls == 1 & zoom >= 7) |
      (cls == 2 & zoom >= 11) |
      (cls == 3 & zoom == 12) |
      (cls == 4 & zoom == 13) |
      (cls == 5 & zoom == 14) |
      (cls == 6 & zoom == 14) |
      (cls == 7 & zoom == 15) |
      (cls == 8 & zoom == 15) |
      (cls == 9 & zoom >= 16) |
      (cls == 10 & zoom >= 16) |
      (cls == 11 & zoom >= 7 & zoom <= 10),
  ]
}

.filter_street_labels <- function(lbl, zoom) {
  if (nrow(lbl) == 0) {
    return(lbl)
  }
  cls <- lbl$map_label_class
  lbl[
    is.na(cls) |
      (cls == 0 & zoom >= 8) |
      (cls == 1 & zoom >= 9) |
      (cls == 2 & zoom >= 11 & zoom <= 15) |
      (cls == 3 & zoom == 12) |
      (cls == 4 & zoom == 13) |
      (cls == 5 & zoom == 14) |
      (cls == 6 & zoom == 14) |
      (cls == 7 & zoom == 15) |
      (cls == 8 & zoom == 15) |
      (cls == 9 & zoom >= 16) |
      (cls == 10 & zoom >= 16),
  ]
}

.filter_city_labels <- function(lbl, zoom) {
  if (nrow(lbl) == 0) {
    return(lbl)
  }
  cls <- lbl$map_label_class
  lbl[
    is.na(cls) |
      (cls %in% c(0, 3, 6, 9, 12, 15) & zoom >= 7 & zoom < 12) |
      (cls %in% c(1, 4, 7, 10, 13, 16) & zoom >= 9 & zoom < 12) |
      (cls %in% c(2, 5, 8, 11, 14, 17) & zoom >= 9 & zoom < 12) |
      (cls >= 18 & zoom >= 12),
  ]
}

# ══════════════════════════════════════════════════════════════════════════════
# 8. LABEL SIZE HELPERS
# ══════════════════════════════════════════════════════════════════════════════
# JSON text-size values in CSS pixels; divide by ~4.5 → ggplot2 units.
# Zoom stops derived directly from each JSON label layer definition.

.sz_county <- function(cls) {
  if (cls <= 1L) {
    # JSON 24px    / 4.36 ≈ 5.5
    5.5
  } else if (cls == 2L) {
    # JSON 26.67px / 4.6  ≈ 5.8
    5.8
  } else {
    2.4
  } # JSON 10.67px / 4.5  ≈ 2.4  (corrected from 2.0)
}

# TRAX linewidths — raw JSON px → converted via .lw()
# JSON bands: z12-14 cas=2.67 fil=1.33; z14-16 cas=3.17 fil=1.83; z16+ cas=3.67 fil=2.33
.trax_lw <- function(zoom, type = "cas") {
  .lw(approx(
    c(12, 14, 16),
    if (type == "cas") c(2.667, 3.167, 3.667)
    else               c(1.333, 1.833, 2.333),
    xout = zoom,
    rule = 2
  )$y
}

# TRAX dashed linetype changes with zoom (JSON dasharray shrinks as zoom increases)
# z12-14: [5,3] ≈ "53"; z14-16: [3.64,2.18] ≈ "42"; z16+: [2.86,1.71] ≈ "32"
.trax_lty <- function(zoom) {
  if (zoom < 14) {
    "53"
  } else if (zoom < 16) {
    "42"
  } else {
    "32"
  }
}

.sz_city <- function(zoom, cls) {
  if (is.na(cls) || cls >= 18L) {
    return(3.5)
  }
  if (cls %in% c(0L, 3L, 6L, 9L, 12L, 15L)) {
    # major cities
    approx(
      c(6, 7, 8, 9, 10, 11),
      c(2.0, 2.1, 2.5, 2.8, 3.0, 3.3),
      xout = zoom,
      rule = 2
    )$y
  } else if (cls %in% c(1L, 4L, 7L, 10L, 13L, 16L)) {
    # medium
    approx(c(9, 10, 11), c(2.4, 2.6, 3.0), xout = zoom, rule = 2)$y
  } else {
    # minor
    approx(c(9, 10, 11), c(2.0, 2.3, 2.5), xout = zoom, rule = 2)$y
  }
}

.sz_hwy <- function(zoom, cls) {
  if (is.na(cls) || cls %in% c(0L, 1L, 11L)) {
    return(1.9)
  } # shield badges
  approx(
    c(7, 9, 11, 12, 13, 14, 15, 16),
    c(1.9, 1.9, 2.0, 2.1, 2.4, 2.8, 2.9, 3.1),
    xout = zoom,
    rule = 2
  )$y
}

.sz_street <- function(zoom, cls) {
  if (is.na(cls) || cls %in% c(0L, 1L, 2L)) {
    return(1.9)
  } # shields
  approx(
    c(8, 9, 11, 12, 13, 14, 15, 16),
    c(1.9, 1.9, 2.0, 2.3, 2.5, 2.8, 3.1, 3.5),
    xout = zoom,
    rule = 2
  )$y
}

# ── Feature label size helpers (derived from JSON text-size stops) ─────────────
# Stream / lake labels — size by prominence class and zoom
# JSON: Major ~9-13px at z8-14, Medium ~8-11px, Minor ~8px z12+; scaled ÷ 4.5
.sz_water_lbl <- function(zoom, cls) {
  if (is.na(cls) || cls == 0L) {
    # Major
    approx(
      c(8, 9, 10, 11, 12, 13, 14),
      c(1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6),
      xout = zoom,
      rule = 2
    )$y
  } else if (cls == 1L) {
    # Medium
    approx(
      c(9, 10, 11, 12, 13, 14),
      c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2),
      xout = zoom,
      rule = 2
    )$y
  } else {
    # Minor
    approx(c(12, 13, 14), c(1.0, 1.2, 1.4), xout = zoom, rule = 2)$y
  }
}

# Monument / state park labels — national parks largest, minor parks smallest
# JSON: Natl Parks ~14-18px z8-13+, State Parks ~10-14px z10-13+; scaled ÷ 4.5
.sz_monument_lbl <- function(zoom, cls) {
  if (is.na(cls) || cls == 0L) {
    # National parks / major monuments
    approx(
      c(8, 9, 10, 11, 12, 13),
      c(1.6, 1.9, 2.2, 2.5, 2.8, 3.0),
      xout = zoom,
      rule = 2
    )$y
  } else if (cls == 1L) {
    # Major state parks / minor monuments
    approx(
      c(9, 10, 11, 12, 13),
      c(1.4, 1.6, 1.8, 2.0, 2.2),
      xout = zoom,
      rule = 2
    )$y
  } else {
    # Remaining parks
    approx(c(11, 12, 13), c(1.2, 1.4, 1.6), xout = zoom, rule = 2)$y
  }
}

# Park / cemetery / golf / ski labels — small feature labels z10-13+
# JSON: ~9-11px at z13-15; scaled ÷ 4.5
.sz_park_lbl <- function(zoom) {
  approx(c(10, 13, 14, 15), c(1.3, 1.6, 1.9, 2.1), xout = zoom, rule = 2)$y
}

# ══════════════════════════════════════════════════════════════════════════════
# 9. LABEL DRAW HELPER
# ══════════════════════════════════════════════════════════════════════════════
# Uses shadowtext::geom_shadowtext() for clean single-pass halo rendering.
# bg.color = halo colour from JSON spec; bg.r controls halo spread.

.lbl <- function(p, data, col, halo, sz, face = "bold", fam = "") {
  if (nrow(data) == 0 || !any(!is.na(data$map_label))) {
    return(p)
  }
  dat <- dplyr::filter(data, !is.na(map_label))
  p +
    shadowtext::geom_shadowtext(
      data = dat,
      mapping = ggplot2::aes(x = X, y = Y, label = map_label),
      color = col,
      bg.color = halo,
      bg.r = 0.15,
      size = sz,
      fontface = face,
      family = fam,
      check_overlap = TRUE
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# 10. COUNTY BORDER PARAMETERS
# ══════════════════════════════════════════════════════════════════════════════
# All widths via .lw(json_px) — exact JSON pixel values from root-LightBase.json.
# JSON Counties layers: z6=1px, z7=1.67px, z8=2.33px,
#   z9-11: outer 4px + inner 0.67px (dasharray [12,6,6,6]),
#   z11+:  3-stroke 5.33px + 3.33px + 0.67px (dasharray [12,6,6,6])

.county_border_params <- function(zoom) {
  if (zoom < 6)  return(list(show=FALSE))
  if (zoom < 7)  return(list(show=TRUE, c1=NA,w1=0, c2=NA,w2=0,
                              c3="#D1D1D1",          w3=.lw(1.0),   lt="solid"))
  if (zoom < 8)  return(list(show=TRUE, c1=NA,w1=0, c2=NA,w2=0,
                              c3="#CCC9C6",          w3=.lw(1.667), lt="solid"))
  if (zoom < 9)  return(list(show=TRUE, c1=NA,w1=0, c2=NA,w2=0,
                              c3="#CCC7C2",          w3=.lw(2.333), lt="solid"))
  if (zoom < 11) return(list(show=TRUE, c1=NA,w1=0,
                              c2="#CCC7C2",          w2=.lw(4.0),
                              c3="#8F8D8C",          w3=.lw(0.667), lt="C663"))
  list(show=TRUE,
    c1=alpha("#736D67",0.1), w1=.lw(5.333),
    c2=alpha("#666361",0.1), w2=.lw(3.333),
    c3=alpha("#4D4B49",0.3), w3=.lw(0.667), lt="C663")
}

# ══════════════════════════════════════════════════════════════════════════════
# 11. MAIN MAP BUILDER
# ══════════════════════════════════════════════════════════════════════════════

build_ugrc_map <- function(lon, lat, zoom, verbose = FALSE) {
  .v <- function(...) if (verbose) message(...) # diagnostic helper

  # — Tile URLs —
  coords <- get_tile_xyz(lon, lat, zoom)
  .url <- function(svc) {
    sprintf(
      "MVT:https://tiles.arcgis.com/tiles/99lidPhWCzftIe9K/arcgis/rest/services/%s/VectorTileServer/tile/%d/%d/%d.pbf",
      svc,
      coords$z,
      coords$y,
      coords$x
    )
  }
  bu <- .url("LiteBase")
  lu <- .url("LiteLabels")
  hu <- .url("VectorHillshade")
  message(sprintf("Tile Z=%d  X=%d  Y=%d", coords$z, coords$x, coords$y))

  # — Fetch —
  ground <- .fetch_ground(bu)
  water <- .fetch_water(bu, zoom)
  rec <- .fetch_recreation(bu, zoom)
  ctrs_raw <- .fetch_contours(bu, zoom)
  roads_raw <- .fetch_roads(bu, zoom)
  muni <- .fetch_muni(bu, zoom)
  transit <- .fetch_transit(bu, zoom)
  buildings <- .fetch_buildings(bu, zoom)
  hillshade <- .fetch_hillshade(hu, zoom)
  lbl_data <- .fetch_labels(lu, bu, zoom)
  poi <- .fetch_poi(bu, zoom)

  .v(sprintf(
    "  counties=%d  hillshade=%d  roads=%d  interstates=%d",
    nrow(ground$counties),
    nrow(hillshade),
    nrow(roads_raw$roads),
    nrow(roads_raw$interstates)
  ))
  .v(sprintf(
    "  lakes=%d  rivers=%d  streams=%d  contours=%d",
    nrow(water$lakes),
    nrow(water$rivers),
    nrow(water$streams),
    nrow(ctrs_raw)
  ))
  .v(sprintf(
    "  lbl_county=%d  lbl_city=%d  lbl_hwy=%d  lbl_street=%d",
    nrow(lbl_data$county),
    nrow(lbl_data$city),
    nrow(lbl_data$highway),
    nrow(lbl_data$street)
  ))
  .v(sprintf(
    "  lbl_streams=%d  lbl_lakes=%d  lbl_monuments=%d  lbl_parks=%d  lbl_trails=%d",
    nrow(lbl_data$streams),
    nrow(lbl_data$lakes),
    nrow(lbl_data$monuments),
    nrow(lbl_data$parks),
    nrow(lbl_data$trails)
  ))
  .v(sprintf(
    "  poi airports=%d  schools_k12=%d  schools_he=%d  healthcare=%d",
    nrow(poi$airports),
    nrow(poi$schools_k12),
    nrow(poi$schools_he),
    nrow(poi$healthcare)
  ))
  .v(sprintf(
    "  poi ski_locs=%d  ski_lifts=%d  trailheads=%d  gnis=%d  osm=%d",
    nrow(poi$ski_locs),
    nrow(poi$ski_lifts),
    nrow(poi$trailheads),
    nrow(poi$gnis),
    nrow(poi$osm_places)
  ))

  # — Post-process —
  counties <- ground$counties
  cbs <- .county_border_params(zoom)
  ctrs <- .filter_contours(ctrs_raw, zoom)
  trax <- .prepare_trax(transit$trax, zoom)

  mon_natl <- if (nrow(rec$monuments) > 0) {
    rec$monuments[rec$monuments$map_symbol == "0", ]
  } else {
    rec$monuments[0, ]
  }
  mon_sp <- if (nrow(rec$monuments) > 0) {
    rec$monuments[rec$monuments$map_symbol == "1", ]
  } else {
    rec$monuments[0, ]
  }

  roads <- roads_raw$roads
  interstates <- roads_raw$interstates
  .rsym <- function(sym) {
    if (nrow(roads) == 0) {
      return(roads[0, ])
    }
    roads[!is.na(roads$map_symbol) & roads$map_symbol == as.character(sym), ]
  }
  road_draw_order <- c(2L, 3L, 4L, 5L, 6L, 7L, 1L)
  road_sym_known <- nrow(roads) > 0 &&
    any(roads$map_symbol %in% as.character(road_draw_order))
  road_fallback <- nrow(roads) > 0 && !road_sym_known

  # Carto:2 outer stroke width — raw JSON px via .lw()
  m2_ow <- if (zoom >= 15) .lw(8.0) else if (zoom >= 13) .lw(6.667) else .lw(4.667)
  m2_iw <- .lw(2.667)   # inner stroke constant at 2.67px across all zooms

  # Label data
  cnty_lbl <- extract_label_coords(.filter_county_labels(lbl_data$county, zoom))

  # County label fallback: if the LiteLabels tile returned no county labels
  # (common at low zoom for specific tiles), generate labels from the county
  # polygon centroids in the base tile. The Counties layer always has a
  # map_label column populated from the _name attribute.
  if (
    (nrow(cnty_lbl) == 0 || !any(!is.na(cnty_lbl$map_label))) &&
      nrow(counties) > 0 &&
      any(!is.na(counties$map_label))
  ) {
    .v("  county label fallback: using polygon centroids from base tile")
    cnty_lbl <- extract_label_coords(counties)
    cnty_lbl$map_label_class <- if (zoom <= 9) {
      1L
    } else if (zoom == 10) {
      2L
    } else {
      3L
    }
  }
  city_lbl <- extract_label_coords(.filter_city_labels(lbl_data$city, zoom))
  hwy_lbl <- extract_label_coords(.filter_highway_labels(
    lbl_data$highway,
    zoom
  ))
  str_lbl <- extract_label_coords(.filter_street_labels(lbl_data$street, zoom))

  # Bounding box
  bb <- sf::st_bbox(counties)
  xm <- (bb["xmax"] - bb["xmin"]) * 0.015
  ym <- (bb["ymax"] - bb["ymin"]) * 0.015
  xlim <- c(bb["xmin"] + xm, bb["xmax"] - xm)
  ylim <- c(bb["ymin"] + ym, bb["ymax"] - ym)

  # ─ Draw stack ─────────────────────────────────────────────────────────────
  p <- ggplot2::ggplot()

  # 1. Ground: WesternStates → Utah overlay → county base
  if (nrow(ground$western_states) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = ground$western_states,
        fill = .C_WEST_FILL,
        color = .C_WEST_BDR,
        linewidth = .lw(1.0)   # JSON: fill-outline 1px default
      )
  }

  utah_fill <- if (zoom >= 8) .C_UTAH_HI else .C_UTAH_LO
  p <- p +
    ggplot2::geom_sf(data = counties, fill = utah_fill, color = NA) +
    ggplot2::geom_sf(data = counties, fill = .C_CTY_FILL, color = NA)

  # 2. Hillshade — per-symbol alpha per JSON spec (no double-alpha bug)
  if (nrow(hillshade) > 0) {
    for (sv in c("0", "1", "2", "3")) {
      hs_sub <- hillshade[
        !is.na(hillshade$map_symbol) & hillshade$map_symbol == sv,
      ]
      if (nrow(hs_sub) == 0) {
        next
      }
      p <- p +
        ggplot2::geom_sf(
          data = hs_sub,
          fill = .HS_COLORS[[sv]],
          color = NA,
          alpha = .hs_alpha(zoom, as.integer(sv))
        )
    }
  }

  # 3. Water
  lake_gsl <- if (nrow(water$lakes) > 0) {
    water$lakes[water$lakes$map_symbol == "0", ]
  } else {
    water$lakes[0, ]
  }
  lake_other <- if (nrow(water$lakes) > 0) {
    water$lakes[water$lakes$map_symbol != "0", ]
  } else {
    water$lakes[0, ]
  }
  if (nrow(lake_gsl) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = lake_gsl,
        fill = .C_GSL_FILL,
        color = "#BBBFBF",
        linewidth = .lw(1.0)
      )
  }
  if (nrow(lake_other) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = lake_other,
        fill = .C_LAKE_FILL,
        color = .C_LAKE_BDR,
        linewidth = .lw(1.0)
      )
  }
  if (nrow(water$gsl) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = water$gsl,
        fill = .C_LAKE_FILL,
        color = .C_LAKE_BDR,
        linewidth = .lw(1.0)
      )
  }
  if (nrow(water$rivers) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = water$rivers,
        fill = .C_RIVER_FILL,
        color = .C_RIVER_BDR,
        linewidth = .lw(1.0)
      )
  }

  # Streams — solid (perennial), dashed (intermittent / ephemeral)
  for (sv in c("0", "1")) {
    ss <- .stream_sym(water$streams, as.integer(sv))
    if (nrow(ss) > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = ss,
          color = .C_STREAM,
          linewidth = .stream_lw(zoom, as.integer(sv))
        )
    }
  }
  dash_lty <- .stream_lty(zoom)
  for (sv in c("2", "3")) {
    ss <- .stream_sym(water$streams, as.integer(sv))
    if (nrow(ss) > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = ss,
          color = .C_STREAM,
          linewidth = .stream_lw(zoom, 2L),
          linetype = dash_lty
        )
    }
  }

  # 4. Contours (sym 3 gap at z12 respected)
  if (nrow(ctrs) > 0) {
    sym_i <- suppressWarnings(as.integer(ctrs$map_symbol))
    ctr_minor <- ctrs[!is.na(sym_i) & sym_i %in% 0:3, ]
    ctr_500 <- ctrs[!is.na(sym_i) & sym_i == 4, ]
    ctr_1000 <- ctrs[!is.na(sym_i) & sym_i == 5, ]
    if (nrow(ctr_minor) > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = ctr_minor,
          color = .C_CTR_MINOR,
          linewidth = .lw(0.667)  # JSON 0.667px,
          alpha = 0.20
        )
    }
    if (nrow(ctr_500) > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = ctr_500,
          color = .C_CTR_500,
          linewidth = .lw(1.0)    # JSON 1.0px,
          alpha = 0.20
        )
    }
    if (nrow(ctr_1000) > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = ctr_1000,
          color = .C_CTR_1000,
          linewidth = .lw(1.333)  # JSON 1.333px,
          alpha = 0.20
        )
    }
  }

  # 5. Parks & recreation fills
  if (nrow(rec$cemeteries) > 0) {
    # Cemeteries outline: JSON rgba(104,128,57,0.4) — distinct from golf rgba(106,128,64,0.4)
    p <- p +
      ggplot2::geom_sf(
        data = rec$cemeteries,
        fill = .C_PARK_FILL,
        color = .C_CEMETERY_BDR,
        linewidth = .lw(1.0)
      )
  }
  if (nrow(rec$golf) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = rec$golf,
        fill = .C_PARK_FILL,
        color = .C_PARK_BDR,
        linewidth = .lw(1.0)
      )
  }
  if (nrow(rec$parks) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = rec$parks,
        fill = .C_PARK_FILL,
        color = .C_PARK_BDR,
        linewidth = .lw(1.0)
      )
  }

  # National Monuments (sym 0) — 3-stroke at z13+, 2-stroke z10-13, 1-stroke z8-10
    if (nrow(mon_natl) > 0) {
    if (zoom >= 13) {
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = .C_NM_OUTER,
          linewidth = .lw(8.0)
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = .C_NM_MID,
          linewidth = .lw(4.0)
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = .C_NM_INNER,
          linewidth = .lw(0.667)
        )
    } else if (zoom >= 10) {
      # z10-13: outer 5.33→0.80, inner 0.67→0.10
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = "#C7C0BD",
          linewidth = .lw(5.333)
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = .C_NM_INNER,
          linewidth = .lw(0.667)
        )
    } else {
      # z8-10: outer 3.33→0.50
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = "#CCC5C2",
          linewidth = .lw(3.333)
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = .C_NM_INNER,
          linewidth = .lw(0.667)
        )
    }
  }
  # State Parks (sym 1) — 3-stroke at z13+, 2-stroke z8-13
  # JSON px: outer 8→1.20, mid 2.67→0.40, inner 1.0→0.15 at z13+
  if (nrow(mon_sp) > 0) {
    if (zoom >= 13) {
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_OUTER,
          linewidth = .lw(8.0)
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_MID,
          linewidth = .lw(2.667)
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_BDR,
          linewidth = .lw(1.0)
        )
    } else if (zoom >= 11) {
      # z11-13: outer 4→0.60, inner 0.67→0.10 (inner = #999391)
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_MID,
          linewidth = .lw(4.0)
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_NM_INNER,
          linewidth = .lw(0.667)
        )
    } else {
      # z8-11: outer 3.33→0.50, inner 0.67→0.10 (inner = #8C867E per JSON)
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_MID,
          linewidth = .lw(3.333)
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_INNER,
          linewidth = .lw(0.667)
        )
    }
  }
  if (nrow(rec$ski) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = rec$ski,
        fill = NA,
        color = .C_SKI_BDR,
        linewidth = .lw(1.0)
      )
  }

  # 5b. Airport runway/taxiway lines — rgba(191,134,162,0.5), z≥10
  if (nrow(poi$airports) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$airports,
        fill = NA,
        color = .C_AIRPORT_LINE,
        linewidth = .lw(1.0)
      )
  }

  # 6. County borders — 3-stroke, 0.15x scale from JSON px
  if (cbs$show) {
    if (!is.na(cbs$c1) && cbs$w1 > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = counties,
          fill = NA,
          color = cbs$c1,
          linewidth = cbs$w1
        )
    }
    if (!is.na(cbs$c2) && cbs$w2 > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = counties,
          fill = NA,
          color = cbs$c2,
          linewidth = cbs$w2
        )
    }
    if (!is.na(cbs$c3) && cbs$w3 > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = counties,
          fill = NA,
          color = cbs$c3,
          linewidth = cbs$w3,
          linetype = cbs$lt
        )
    }
  }

  # 7. Municipalities Carto:2 (type-coded fills + two-stroke borders)
  # sym 0 outer stroke: z<15 = rgba(160,134,179,0.15), z15+ = rgba(144,120,161,0.15)
  .m2_out0 <- if (zoom >= 15) {
    .C_M2_OUT[["0"]]
  } else {
    rgb(160, 134, 179, 38, maxColorValue = 255)
  } # rgba(160,134,179,0.15)
  if (nrow(muni$muni2) > 0) {
    for (sv in c("0", "1", "2", "3")) {
      msf <- muni$muni2[
        !is.na(muni$muni2$map_symbol) & muni$muni2$map_symbol == sv,
      ]
      if (nrow(msf) == 0) {
        next
      }
      out_col <- if (sv == "0") .m2_out0 else .C_M2_OUT[[sv]]
      p <- p + ggplot2::geom_sf(data = msf, fill = .C_M2_FILL, color = NA)
      p <- p +
        ggplot2::geom_sf(
          data = msf,
          fill = NA,
          color = out_col,
          linewidth = m2_ow
        )
      p <- p +
        ggplot2::geom_sf(
          data = msf,
          fill = NA,
          color = .C_M2_IN[[sv]],
          linewidth = m2_iw
        )
    }
  }

  # 8. Municipalities Carto:1
  if (nrow(muni$muni1) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = muni$muni1,
        fill = NA,
        color = .C_MUNI1_BDR,
        linewidth = .lw(0.667),  # JSON 0.667px
        linetype = "dotdash"
      )
  }

  # 9. Railroads
  if (nrow(roads_raw$railroads) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = roads_raw$railroads,
        color = .C_RAIL,
        linewidth = .lw(0.733)  # JSON 0.733px
      )
  }

  # 10. Roads — CASING PASS (widest class first)
  if (nrow(interstates) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = interstates,
        color = .C_ROAD_CAS,
        linewidth = .road_lw(zoom, 0, "cas")
      )
  }
  for (si in road_draw_order) {
    if (zoom < .ROAD_MIN_ZOOM[as.character(si)]) {
      next
    }
    rd <- .rsym(si)
    if (nrow(rd) == 0) {
      next
    }
    p <- p +
      ggplot2::geom_sf(
        data = rd,
        color = .C_ROAD_CAS,
        linewidth = .road_lw(zoom, si, "cas")
      )
  }

  # 11. Roads — FILL PASS (white centre lines)
  if (nrow(interstates) > 0) {
    lwf <- .road_lw(zoom, 0, "fil")
    if (lwf > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = interstates,
          color = .C_ROAD_FIL,
          linewidth = lwf
        )
    }
  }
  for (si in road_draw_order) {
    if (zoom < .ROAD_MIN_ZOOM[as.character(si)]) {
      next
    }
    rd <- .rsym(si)
    if (nrow(rd) == 0) {
      next
    }
    lwf <- .road_lw(zoom, si, "fil")
    if (lwf <= 0) {
      next
    }
    lty <- if (si == 5L) "22" else "solid"
    p <- p +
      ggplot2::geom_sf(
        data = rd,
        color = .C_ROAD_FIL,
        linewidth = lwf,
        linetype = lty
      )
  }
  if (road_fallback) {
    lw_c <- .road_lw(zoom, 4, "cas")
    lw_f <- .road_lw(zoom, 4, "fil")
    p <- p +
      ggplot2::geom_sf(data = roads, color = .C_ROAD_CAS, linewidth = lw_c)
    if (lw_f > 0) {
      p <- p +
        ggplot2::geom_sf(data = roads, color = .C_ROAD_FIL, linewidth = lw_f)
    }
  }

  # 12. Trails — JSON z14-15: outer 3px→0.45, fill 1.33px→0.20; z15+: 3.33→0.50, 1.67→0.25
  if (nrow(rec$trails) > 0) {
    tw <- if (zoom >= 15) .lw(3.333) else .lw(3.0)
    tf <- if (zoom >= 15) .lw(1.667) else .lw(1.333)
    p <- p +
      ggplot2::geom_sf(
        data = rec$trails,
        color = .C_TRAIL_CAS,
        linewidth = tw,
        linetype = "11"
      )
    p <- p +
      ggplot2::geom_sf(
        data = rec$trails,
        color = .C_ROAD_FIL,
        linewidth = tf,
        linetype = "11"
      )
  }

  # 12b. Ski lifts — JSON #BF8F8F dasharray [18,3,1,3], width 1.33px→0.20mm, z≥12
  # "F313" is the closest ggplot2 hex linetype encoding of [15,3,1,3] (18 capped at F=15)
  if (nrow(poi$ski_lifts) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$ski_lifts,
        color = .C_SKI_LIFT_LINE,
        linewidth = .lw(1.333),   # JSON 1.333px
        linetype = "F313"
      )
  }

  # 13. Transit — commuter rail
  # JSON: casing 3.333px, fill 1.667px, dashed #D2ACE6 1.667px dasharray [4,2.4]
  if (nrow(transit$commuter_rail) > 0) {
    p <- p + ggplot2::geom_sf(data=transit$commuter_rail, color=.C_TR_CAS,      linewidth=.lw(3.333))
    p <- p + ggplot2::geom_sf(data=transit$commuter_rail, color=.C_TR_FIL,      linewidth=.lw(1.667))
    p <- p + ggplot2::geom_sf(data=transit$commuter_rail, color=.C_FRONTRUNNER, linewidth=.lw(1.667), linetype="42")
  }

  # TRAX — zoom-interpolated widths; zoom-dependent dasharray; sym 10 solid only
  if (nrow(trax) > 0) {
    lw_cas <- .trax_lw(zoom, "cas")
    lw_fil <- .trax_lw(zoom, "fil")
    trax_lt <- .trax_lty(zoom)
    for (cv in unique(trax$trax_color)) {
      tr_sub <- trax[trax$trax_color == cv, ]
      p <- p +
        ggplot2::geom_sf(data = tr_sub, color = .C_TR_CAS, linewidth = lw_cas)
      p <- p +
        ggplot2::geom_sf(data = tr_sub, color = .C_TR_FIL, linewidth = lw_fil)
      if (cv != .C_TRAX_DEF) {
        # All named lines: coloured dashed overlay with 0.8 alpha embedded in colour
        p <- p +
          ggplot2::geom_sf(
            data = tr_sub,
            color = cv,
            linewidth = lw_fil,
            linetype = trax_lt
          )
      } else {
        # sym 10 (unknown/default): JSON shows single solid #828282 line, no dashed overlay
        p <- p + ggplot2::geom_sf(data = tr_sub, color = cv, linewidth = lw_cas)
      }
    }
  }

  # Rail/commuter stops — JSON: invisible icon + text label (minzoom 14)
  # Rendered as small points; labels come from their map_label field in the label draw section
  if (nrow(transit$rail_stops) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = transit$rail_stops,
        shape = 21,
        size = 1.2,
        fill = "white",
        color = alpha("#597EB3", 0.8),
        stroke = 0.4
      )
  }
  if (nrow(transit$cr_stops) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = transit$cr_stops,
        shape = 21,
        size = 1.2,
        fill = "white",
        color = alpha("#9B7BBD", 0.8),
        stroke = 0.4
      )
  }

  # 14. Buildings
  if (nrow(buildings) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = buildings,
        fill = .C_BLDG_FILL,
        color = .C_BLDG_BDR,
        linewidth = .lw(0.267)  # JSON 0.267px
      )
  }

  # 14b. POI point markers — rendered as circles/shapes (icons not available in ggplot2)
  # Zoom gates match JSON minzoom per layer.
  if (nrow(poi$ski_locs) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$ski_locs,
        shape = 23,
        size = 1.4,
        fill = "white",
        color = "#999796",
        stroke = 0.4
      )
  }
  if (nrow(poi$airport_pts) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$airport_pts,
        shape = 21,
        size = 1.8,
        fill = "#C8C0B8",
        color = "#8C8985",
        stroke = 0.4
      )
  }
  if (nrow(poi$gnis_bay) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$gnis_bay,
        shape = 21,
        size = 0.6,
        fill = .C_WATER_LBL,
        color = NA
      )
  }
  if (nrow(poi$gnis) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$gnis,
        shape = 24,
        size = 0.9,
        fill = .C_GNIS_PT,
        color = NA
      )
  }
  if (nrow(poi$osm_places) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$osm_places,
        shape = 21,
        size = 0.9,
        fill = .C_OSM_PT,
        color = NA
      )
  }
  if (nrow(poi$trailheads) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$trailheads,
        shape = 24,
        size = 1.2,
        fill = .C_TRAIL_PT,
        color = alpha(.C_TRAIL_PT, 0.7),
        stroke = 0.3
      )
  }
  if (nrow(poi$schools_he) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$schools_he,
        shape = 21,
        size = 1.5,
        fill = "transparent",
        color = .C_SCHOOL_HE_PT,
        stroke = 0.6
      )
  }
  if (nrow(poi$schools_k12) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$schools_k12,
        shape = 21,
        size = 1.2,
        fill = "transparent",
        color = .C_SCHOOL_K12_PT,
        stroke = 0.5
      )
  }
  if (nrow(poi$healthcare) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = poi$healthcare,
        shape = 21,
        size = 1.2,
        fill = .C_HEALTH_PT,
        color = alpha(.C_HEALTH_PT, 0.8),
        stroke = 0.3
      )
  }

  # ─ LABELS ────────────────────────────────────────────────────────────────
  # Feature labels drawn first (below city/county labels in z-order)

  # 15a. Great Salt Lake label — italic, water colour, z≥7
  if (nrow(lbl_data$gsl) > 0 && any(!is.na(lbl_data$gsl$map_label))) {
    gsl_lbl <- extract_label_coords(lbl_data$gsl)
    p <- .lbl(
      p,
      gsl_lbl,
      col = .C_WATER_LBL,
      halo = .C_WATER_HALO,
      sz = .sz_water_lbl(zoom, 0L),
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15b. Lake labels — italic, water colour, z≥8 for major; minor z≥11
  if (nrow(lbl_data$lakes) > 0 && any(!is.na(lbl_data$lakes$map_label))) {
    lake_lbl <- extract_label_coords(lbl_data$lakes)
    # Filter by label class for appropriate zoom
    cls_lake <- suppressWarnings(as.integer(lake_lbl$map_label_class))
    cls_lake[is.na(cls_lake)] <- 0L
    lake_lbl <- lake_lbl[
      (cls_lake == 0L & zoom >= 8) |
        (cls_lake == 1L & zoom >= 9) |
        (cls_lake >= 2L & zoom >= 11),
    ]
    if (nrow(lake_lbl) > 0 && any(!is.na(lake_lbl$map_label))) {
      sz_lake <- .sz_water_lbl(
        zoom,
        suppressWarnings(as.integer(median(
          cls_lake[cls_lake >= 0],
          na.rm = TRUE
        )))
      )
      p <- .lbl(
        p,
        lake_lbl,
        col = .C_WATER_LBL,
        halo = .C_WATER_HALO,
        sz = sz_lake,
        face = "italic",
        fam = .F_STREET
      )
    }
  }

  # 15c. Stream labels — italic, water colour, major z≥8 down to minor z≥12
  if (nrow(lbl_data$streams) > 0 && any(!is.na(lbl_data$streams$map_label))) {
    stm_lbl <- extract_label_coords(lbl_data$streams)
    cls_stm <- suppressWarnings(as.integer(stm_lbl$map_label_class))
    cls_stm[is.na(cls_stm)] <- 0L
    stm_lbl <- stm_lbl[
      (cls_stm == 0L & zoom >= 8) |
        (cls_stm == 1L & zoom >= 9) |
        (cls_stm >= 2L & zoom >= 12),
    ]
    if (nrow(stm_lbl) > 0 && any(!is.na(stm_lbl$map_label))) {
      sz_stm <- .sz_water_lbl(
        zoom,
        suppressWarnings(as.integer(median(cls_stm, na.rm = TRUE)))
      )
      p <- .lbl(
        p,
        stm_lbl,
        col = .C_WATER_LBL,
        halo = .C_WATER_HALO,
        sz = sz_stm,
        face = "italic",
        fam = .F_STREET
      )
    }
  }

  # 15d. Monument / state park labels — national parks z≥8, state parks z≥10, minor z≥11
  if (
    nrow(lbl_data$monuments) > 0 && any(!is.na(lbl_data$monuments$map_label))
  ) {
    mon_lbl <- extract_label_coords(lbl_data$monuments)
    cls_mon <- suppressWarnings(as.integer(mon_lbl$map_label_class))
    cls_mon[is.na(cls_mon)] <- 2L
    mon_lbl <- mon_lbl[
      (cls_mon == 0L & zoom >= 8) |
        (cls_mon == 1L & zoom >= 10) |
        (cls_mon >= 2L & zoom >= 11),
    ]
    if (nrow(mon_lbl) > 0 && any(!is.na(mon_lbl$map_label))) {
      sz_mon <- .sz_monument_lbl(
        zoom,
        suppressWarnings(as.integer(median(cls_mon, na.rm = TRUE)))
      )
      p <- .lbl(
        p,
        mon_lbl,
        col = .C_MON_LBL,
        halo = .C_MON_HALO,
        sz = sz_mon,
        face = "bold",
        fam = .F_STREET
      )
    }
  }

  # 15e. Park labels — z≥13
  if (nrow(lbl_data$parks) > 0 && any(!is.na(lbl_data$parks$map_label))) {
    park_lbl <- extract_label_coords(lbl_data$parks)
    p <- .lbl(
      p,
      park_lbl,
      col = .C_PARK_LBL,
      halo = .C_PARK_HALO,
      sz = .sz_park_lbl(zoom),
      face = "bold",
      fam = .F_STREET
    )
  }

  # 15f. Golf course labels — z≥13
  if (nrow(lbl_data$golf) > 0 && any(!is.na(lbl_data$golf$map_label))) {
    golf_lbl <- extract_label_coords(lbl_data$golf)
    p <- .lbl(
      p,
      golf_lbl,
      col = .C_PARK_LBL,
      halo = .C_PARK_HALO,
      sz = .sz_park_lbl(zoom),
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15g. Cemetery labels — z≥10
  if (
    nrow(lbl_data$cemeteries) > 0 && any(!is.na(lbl_data$cemeteries$map_label))
  ) {
    cem_lbl <- extract_label_coords(lbl_data$cemeteries)
    p <- .lbl(
      p,
      cem_lbl,
      col = .C_PARK_LBL,
      halo = .C_PARK_HALO,
      sz = .sz_park_lbl(zoom),
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15h. Trail labels — z≥14 (line-placement not supported; use centroid)
  if (nrow(lbl_data$trails) > 0 && any(!is.na(lbl_data$trails$map_label))) {
    trail_lbl <- extract_label_coords(lbl_data$trails)
    p <- .lbl(
      p,
      trail_lbl,
      col = .C_PARK_LBL,
      halo = .C_PARK_HALO,
      sz = .sz_park_lbl(zoom),
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15i. Ski area labels — z≥13
  if (nrow(lbl_data$ski) > 0 && any(!is.na(lbl_data$ski$map_label))) {
    ski_lbl <- extract_label_coords(lbl_data$ski)
    p <- .lbl(
      p,
      ski_lbl,
      col = .C_SKI_LBL,
      halo = .C_SKI_HALO,
      sz = .sz_park_lbl(zoom),
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15j. Contour elevation labels — Arial Bold Italic, z≥13; centroid approx (line-placement N/A)
  if (nrow(lbl_data$ctr_lbl) > 0 && any(!is.na(lbl_data$ctr_lbl$map_label))) {
    ctr_lbl_c <- extract_label_coords(lbl_data$ctr_lbl)
    p <- .lbl(
      p,
      ctr_lbl_c,
      col = .C_CTR_LBL,
      halo = .C_CTR_HALO,
      sz = 1.5,
      face = "italic",
      fam = .F_HWY
    )
  }

  # 15k. Ski lift labels — #FFFEFA on #B39898 halo, z≥12
  if (
    nrow(lbl_data$ski_lift_lbl) > 0 &&
      any(!is.na(lbl_data$ski_lift_lbl$map_label))
  ) {
    sklift_lbl_c <- extract_label_coords(lbl_data$ski_lift_lbl)
    p <- .lbl(
      p,
      sklift_lbl_c,
      col = .C_SKLIFT_LBL,
      halo = .C_SKLIFT_HALO,
      sz = 1.5,
      face = "italic",
      fam = .F_HWY
    )
  }

  # 15l. GNIS bay labels — water colour italic, z≥11
  if (nrow(poi$gnis_bay) > 0 && any(!is.na(poi$gnis_bay$map_label))) {
    bay_lbl <- extract_label_coords(poi$gnis_bay)
    p <- .lbl(
      p,
      bay_lbl,
      col = .C_BAY_LBL,
      halo = .C_BAY_HALO,
      sz = 1.5,
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15m. GNIS place labels (summits, geographic names) — #807F7D italic, z≥12
  if (nrow(poi$gnis) > 0 && any(!is.na(poi$gnis$map_label))) {
    gnis_lbl <- extract_label_coords(poi$gnis)
    p <- .lbl(
      p,
      gnis_lbl,
      col = .C_GNIS_LBL,
      halo = .C_GNIS_HALO,
      sz = 1.5,
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15n. OpenSourcePlaces labels — #8C8B89 italic, z≥14
  if (nrow(poi$osm_places) > 0 && any(!is.na(poi$osm_places$map_label))) {
    osm_lbl <- extract_label_coords(poi$osm_places)
    p <- .lbl(
      p,
      osm_lbl,
      col = .C_POI_LBL,
      halo = .C_POI_HALO_STD,
      sz = 1.7,
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15o. Ski area location labels — #8C8989 italic, z≥11
  if (nrow(poi$ski_locs) > 0 && any(!is.na(poi$ski_locs$map_label))) {
    ski_loc_lbl <- extract_label_coords(poi$ski_locs)
    p <- .lbl(
      p,
      ski_loc_lbl,
      col = .C_SKI_LBL,
      halo = .C_POI_HALO_SCH,
      sz = 1.8,
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15p. Trailhead labels — #807D7D italic, z≥14
  if (nrow(poi$trailheads) > 0 && any(!is.na(poi$trailheads$map_label))) {
    trail_lbl <- extract_label_coords(poi$trailheads)
    p <- .lbl(
      p,
      trail_lbl,
      col = .C_TRAIL_LBL,
      halo = .C_TRAIL_HALO_PT,
      sz = 1.7,
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15q. Higher education labels — #8C8989 italic bold, z≥13
  if (nrow(poi$schools_he) > 0 && any(!is.na(poi$schools_he$map_label))) {
    she_lbl <- extract_label_coords(poi$schools_he)
    p <- .lbl(
      p,
      she_lbl,
      col = .C_POI_LBL,
      halo = .C_POI_HALO_HE,
      sz = 1.9,
      face = "bold.italic",
      fam = .F_STREET
    )
  }

  # 15r. K-12 school labels — #8C8989 italic, z≥14
  if (nrow(poi$schools_k12) > 0 && any(!is.na(poi$schools_k12$map_label))) {
    sk12_lbl <- extract_label_coords(poi$schools_k12)
    p <- .lbl(
      p,
      sk12_lbl,
      col = .C_POI_LBL,
      halo = .C_POI_HALO_SCH,
      sz = 1.7,
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15s. Healthcare labels — #8C8989 italic, z≥14
  if (nrow(poi$healthcare) > 0 && any(!is.na(poi$healthcare$map_label))) {
    hc_lbl <- extract_label_coords(poi$healthcare)
    p <- .lbl(
      p,
      hc_lbl,
      col = .C_POI_LBL,
      halo = .C_POI_HALO_STD,
      sz = 1.7,
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15t. Airport labels — #8C8989 italic, z≥10
  if (nrow(poi$airport_pts) > 0 && any(!is.na(poi$airport_pts$map_label))) {
    apt_lbl <- extract_label_coords(poi$airport_pts)
    p <- .lbl(
      p,
      apt_lbl,
      col = .C_POI_LBL,
      halo = .C_POI_HALO_STD,
      sz = 1.8,
      face = "italic",
      fam = .F_STREET
    )
  }

  # 15. County labels — Poller One, light text on gray halo
  # Word-wrap to match Mapbox GL text-max-width; draw per label_class so each
  # county gets its own correct text size and colour (not a batch median).
  if (nrow(cnty_lbl) > 0 && any(!is.na(cnty_lbl$map_label))) {
    cnty_lbl$map_label <- vapply(
      cnty_lbl$map_label,
      function(x) {
        if (is.na(x)) {
          return(NA_character_)
        }
        paste(strwrap(x, width = 8L), collapse = "\n")
      },
      character(1L)
    )
    cnty_lbl$cls_i <- as.integer(
      suppressWarnings(cnty_lbl$map_label_class)
    )
    cnty_lbl$cls_i[is.na(cnty_lbl$cls_i)] <- 3L
    for (ci in unique(cnty_lbl$cls_i)) {
      sub_c <- cnty_lbl[!is.na(cnty_lbl$cls_i) & cnty_lbl$cls_i == ci, ]
      if (nrow(sub_c) == 0 || !any(!is.na(sub_c$map_label))) {
        next
      }
      lbl_col <- if (ci >= 3L) .C_CNTY_LO else .C_CNTY_HI
      p <- .lbl(
        p,
        sub_c,
        col = lbl_col,
        halo = .C_CNTY_HALO,
        sz = .sz_county(ci),
        face = "bold",
        fam = .F_COUNTY
      )
    }
  }

  # 16. City / town labels
  if (nrow(city_lbl) > 0 && any(!is.na(city_lbl$map_label))) {
    cls_i <- as.integer(suppressWarnings(median(
      city_lbl$map_label_class,
      na.rm = TRUE
    )))
    if (is.na(cls_i)) {
      cls_i <- 0L
    }
    p <- .lbl(
      p,
      city_lbl,
      col = .C_CITY,
      halo = .C_CITY_HALO,
      sz = .sz_city(zoom, cls_i),
      face = "bold",
      fam = .F_CITY
    )
  }

  # 17. Highway labels — per-class colour, zoom-interpolated size
  if (nrow(hwy_lbl) > 0 && any(!is.na(hwy_lbl$map_label))) {
    hwy_lbl <- hwy_lbl %>%
      dplyr::mutate(
        lbl_col = dplyr::case_when(
          map_label_class == 0 ~ .C_HWY_INT,
          map_label_class %in% c(1L, 2L, 11L) ~ .C_HWY_US,
          TRUE ~ .C_HWY_ROAD
        )
      )
    cls_i <- as.integer(suppressWarnings(median(
      hwy_lbl$map_label_class,
      na.rm = TRUE
    )))
    sz <- .sz_hwy(zoom, if (is.na(cls_i)) NA_integer_ else cls_i)
    for (cv in unique(hwy_lbl$lbl_col)) {
      sub_lbl <- dplyr::filter(hwy_lbl, lbl_col == cv, !is.na(map_label))
      if (nrow(sub_lbl) == 0) {
        next
      }
      p <- .lbl(
        p,
        sub_lbl,
        col = cv,
        halo = .C_HWY_HALO,
        sz = sz,
        face = "bold",
        fam = .F_HWY
      )
    }
  }

  # 18. Street labels
  if (nrow(str_lbl) > 0 && any(!is.na(str_lbl$map_label))) {
    cls_i <- as.integer(suppressWarnings(median(
      str_lbl$map_label_class,
      na.rm = TRUE
    )))
    sz <- .sz_street(zoom, if (is.na(cls_i)) NA_integer_ else cls_i)
    p <- .lbl(
      p,
      str_lbl,
      col = .C_STR,
      halo = .C_STR_HALO,
      sz = sz,
      face = "bold",
      fam = .F_STREET
    )
  }

  # ─ Coordinate system & theme ─────────────────────────────────────────────
  p +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, crs = 3857) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = .C_CTY_FILL, color = NA)
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# 12. EXAMPLE CALL
# ══════════════════════════════════════════════════════════════════════════════
downtown_map <- build_ugrc_map(lon = -111.89, lat = 40.76, zoom = 14)
print(downtown_map)

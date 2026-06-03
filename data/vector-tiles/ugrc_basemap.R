# ugrc_basemap.R — Full UGRC LiteBase / LiteLabels / VectorHillshade basemap
# Implements all visual layers from root-LightBase.json, root-LiteLabels.json,
# and root-VectorHillshade.json as a ggplot2 map for any lon/lat/zoom.
# No shadowtext dependency — text halos drawn with double geom_text pass.

# ══════════════════════════════════════════════════════════════════════════════
# 1. PACKAGES & FONTS
# ══════════════════════════════════════════════════════════════════════════════
library(sf)
library(ggplot2)
library(dplyr)
library(showtext)

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
.C_NM_INNER <- "#999391"
.C_SP_OUTER <- rgb(199, 199, 199, 191, maxColorValue = 255) # 0.75
.C_SP_MID <- "#B6BCBF"
.C_SP_BDR <- "#8C867E"

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

# Buildings — JSON: fill #F2F0ED; border rgba(128,123,121,0.25)
.C_BLDG_FILL <- "#F2F0ED"
.C_BLDG_BDR <- rgb(128, 123, 121, 64, maxColorValue = 255) # 0.25

# Roads
.C_ROAD_CAS <- "#B3B3B3"
.C_ROAD_FIL <- "#FFFFFF"
.C_RAIL <- "#B3AFAF"
.C_TRAIL_CAS <- "#ADA5A5"

# Transit
.C_TR_CAS <- "#B2B2B2"
.C_TR_FIL <- "#FFFFFF"
.C_TRAX_BLU <- "#597EB3"
.C_TRAX_GRN <- "#60BF4D"
.C_TRAX_RED <- "#CC666F"
.C_TRAX_SLN <- "#79DEF2"
.C_TRAX_DEF <- "#828282"
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

# ══════════════════════════════════════════════════════════════════════════════
# 3. ROAD LINEWIDTH LOOKUP TABLES
# ══════════════════════════════════════════════════════════════════════════════
# JSON CSS pixel values × 0.15 ≈ ggplot2 linewidth (mm at ~96 DPI output).
# Entries: z = zoom stops, cas = casing (outer gray), fil = fill (inner white).

.RSTOPS <- list(
  `0` = list(
    z = c(6, 8, 10, 12, 14, 16, 18),
    cas = c(.38, .57, .82, 1.01, 1.52, 2.92, 4.05),
    fil = c(0, .19, .44, .63, 1.14, 2.16, 3.05)
  ),
  `1` = list(
    z = c(11, 12, 13, 14, 15, 16, 18),
    cas = c(.40, .55, .70, .90, 1.15, 1.40, 2.40),
    fil = c(.13, .20, .33, .57, .83, 1.07, 1.60)
  ),
  `2` = list(
    z = c(6, 8, 10, 12, 14, 16, 18),
    cas = c(.30, .45, .65, .80, 1.20, 2.30, 4.05),
    fil = c(0, .15, .35, .50, .90, 1.70, 3.05)
  ),
  `3` = list(
    z = c(7, 9, 11, 13, 15, 17),
    cas = c(.30, .45, .65, 1.00, 1.70, 3.80),
    fil = c(0, .15, .35, .70, 1.30, 3.05)
  ),
  `4` = list(
    z = c(9, 10, 11, 12, 13, 14, 15, 17),
    cas = c(.35, .40, .50, .60, .75, .90, 1.20, 3.80),
    fil = c(.16, .20, .30, .40, .50, .70, .90, 3.05)
  ),
  `5` = list(
    z = c(9, 11, 12, 13, 14, 15, 17),
    cas = c(.40, .40, .50, .60, .90, 1.00, 2.50),
    fil = c(.20, .20, .30, .40, .70, .80, 2.30)
  ),
  `6` = list(
    z = c(11, 12, 13, 14, 15, 17),
    cas = c(.40, .50, .70, .90, 1.00, 3.80),
    fil = c(.20, .30, .50, .70, .80, 3.30)
  ),
  `7` = list(
    z = c(12, 13, 14, 15, 16, 17),
    cas = c(.40, .50, .60, .90, 1.40, 2.80),
    fil = c(.20, .30, .40, .70, 1.20, 2.50)
  )
)

.road_lw <- function(zoom, sym, type = "cas") {
  k <- as.character(sym)
  if (!k %in% names(.RSTOPS)) {
    return(0.4)
  }
  st <- .RSTOPS[[k]]
  approx(st$z, st[[type]], xout = zoom, rule = 2)$y
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

  # Standardise symbol (broad match for _symbol, symbolID, etc.)
  sy <- grep("symbol", cn, ignore.case = TRUE, value = TRUE)
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
    rail_stops = if (zoom >= 12) {
      safe_read_mvt(bu, "LightRailStations_UTA")
    } else {
      .empty_sf()
    },
    cr_stops = if (zoom >= 12) {
      safe_read_mvt(bu, "CommuterRailStops_UTA")
    } else {
      .empty_sf()
    }
  )
}

.fetch_buildings <- function(bu, zoom) {
  if (zoom >= 14) safe_read_mvt(bu, "Buildings") else .empty_sf()
}

.fetch_hillshade <- function(hu, zoom) safe_read_mvt(hu, .hs_res_layer(zoom))

.fetch_labels <- function(lu, zoom) {
  list(
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
        map_symbol %in% c("3", "4", "5") ~ .C_TRAX_GRN,
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
  # JSON: sym 0 (Perennial Major) 1.0-2.0px; sym 1 (Perennial) 1.0-1.33px; sym 2-3 0.67px
  # Scale × 0.15
  if (sym == 0L) {
    approx(
      c(6, 9, 11, 13, 15),
      c(1.0, 1.0, 1.0, 1.33, 2.0),
      xout = zoom,
      rule = 2
    )$y *
      0.15
  } else if (sym == 1L) {
    approx(c(11, 13, 14), c(1.0, 1.0, 1.33), xout = zoom, rule = 2)$y * 0.15
  } else {
    0.67 * 0.15
  }
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
    # JSON 24px   → ~5.5 units
    5.5
  } else if (cls == 2L) {
    # JSON 26.67px → ~5.8
    5.8
  } else {
    2.0
  } # JSON 10.67px → ~2.4 (using 2.0 for Poller weight)
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

# ══════════════════════════════════════════════════════════════════════════════
# 9. LABEL DRAW HELPER (no shadowtext dependency)
# ══════════════════════════════════════════════════════════════════════════════
# Approximates JSON text-halo-color by drawing text twice:
#   pass 1 — halo colour, check_overlap=FALSE (always draw all halos)
#   pass 2 — label colour, check_overlap=TRUE  (suppress true overlaps at actual size)
# Keeping both passes independent ensures labels are never silently dropped by
# halo-size inflation (1.25×) interfering with the label-size overlap check.

.lbl <- function(p, data, col, halo, sz, face = "bold", fam = "") {
  if (nrow(data) == 0 || !any(!is.na(data$map_label))) return(p)
  dat <- dplyr::filter(data, !is.na(map_label))
  mp  <- ggplot2::aes(x = X, y = Y, label = map_label)
  # Halo: no overlap check — every label gets a halo background
  p <- p + ggplot2::geom_text(data=dat, mapping=mp,
             color=halo, size=sz*1.25, fontface=face, family=fam,
             check_overlap=FALSE)
  # Label text: overlap check at actual size to suppress truly crowded labels
  p + ggplot2::geom_text(data=dat, mapping=mp,
             color=col, size=sz, fontface=face, family=fam,
             check_overlap=TRUE)
}

# ══════════════════════════════════════════════════════════════════════════════
# 10. COUNTY BORDER PARAMETERS
# ══════════════════════════════════════════════════════════════════════════════
# Widths are empirically calibrated for ggplot2 legibility, not mathematically
# derived from JSON pixels. County borders are administrative lines with no
# geographic width; they must be visible at any output size. Values roughly
# proportional to JSON spec and confirmed visible in ggplot2 output.
# JSON px: z6=1.0, z7=1.67, z8=2.33, z9-11=[4.0,0.67], z11+=[5.33,3.33,0.67]

.county_border_params <- function(zoom) {
  if (zoom < 6)  return(list(show = FALSE))
  if (zoom < 7)  return(list(show=TRUE, c1=NA, w1=0, c2=NA, w2=0,
                              c3="#D1D1D1",         w3=0.6,  lt="solid"))
  if (zoom < 8)  return(list(show=TRUE, c1=NA, w1=0, c2=NA, w2=0,
                              c3="#CCC9C6",         w3=1.0,  lt="solid"))
  if (zoom < 9)  return(list(show=TRUE, c1=NA, w1=0, c2=NA, w2=0,
                              c3="#CCC7C2",         w3=1.4,  lt="solid"))
  if (zoom < 11) return(list(show=TRUE, c1=NA, w1=0,
                              c2="#CCC7C2",         w2=2.5,
                              c3="#8F8D8C",         w3=0.5,  lt="dashed"))
  list(show=TRUE,
    c1 = alpha("#736D67", 0.1), w1 = 3.5,
    c2 = alpha("#666361", 0.1), w2 = 2.2,
    c3 = alpha("#4D4B49", 0.3), w3 = 0.5,  lt = "dashed")
}

# ══════════════════════════════════════════════════════════════════════════════
# 11. MAIN MAP BUILDER
# ══════════════════════════════════════════════════════════════════════════════

build_ugrc_map <- function(lon, lat, zoom, verbose = FALSE) {
  .v <- function(...) if (verbose) message(...)   # diagnostic helper

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
  ground    <- .fetch_ground(bu)
  water     <- .fetch_water(bu, zoom)
  rec       <- .fetch_recreation(bu, zoom)
  ctrs_raw  <- .fetch_contours(bu, zoom)
  roads_raw <- .fetch_roads(bu, zoom)
  muni      <- .fetch_muni(bu, zoom)
  transit   <- .fetch_transit(bu, zoom)
  buildings <- .fetch_buildings(bu, zoom)
  hillshade <- .fetch_hillshade(hu, zoom)
  lbl_data  <- .fetch_labels(lu, zoom)

  .v(sprintf("  counties=%d  hillshade=%d  roads=%d  interstates=%d",
     nrow(ground$counties), nrow(hillshade),
     nrow(roads_raw$roads), nrow(roads_raw$interstates)))
  .v(sprintf("  lakes=%d  rivers=%d  streams=%d  contours=%d",
     nrow(water$lakes), nrow(water$rivers),
     nrow(water$streams), nrow(ctrs_raw)))
  .v(sprintf("  lbl_county=%d  lbl_city=%d  lbl_hwy=%d  lbl_street=%d",
     nrow(lbl_data$county), nrow(lbl_data$city),
     nrow(lbl_data$highway), nrow(lbl_data$street)))

  # — Post-process —
  counties <- ground$counties
  cbs      <- .county_border_params(zoom)
  ctrs     <- .filter_contours(ctrs_raw, zoom)
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

  m2_ow <- if (zoom >= 15) {
    0.80
  } else if (zoom >= 13) {
    0.67
  } else {
    0.47
  }

  # Label data
  cnty_lbl <- extract_label_coords(.filter_county_labels(lbl_data$county, zoom))

  # County label fallback: if the LiteLabels tile returned no county labels
  # (common at low zoom for specific tiles), generate labels from the county
  # polygon centroids in the base tile. The Counties layer always has a
  # map_label column populated from the _name attribute.
  if ((nrow(cnty_lbl) == 0 || !any(!is.na(cnty_lbl$map_label))) &&
      nrow(counties) > 0 && any(!is.na(counties$map_label))) {
    .v("  county label fallback: using polygon centroids from base tile")
    cnty_lbl <- extract_label_coords(counties)
    cnty_lbl$map_label_class <- if (zoom <= 9) 1L else if (zoom == 10) 2L else 3L
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
        linewidth = 0.15
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
        linewidth = 0.10
      )
  }
  if (nrow(lake_other) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = lake_other,
        fill = .C_LAKE_FILL,
        color = .C_LAKE_BDR,
        linewidth = 0.10
      )
  }
  if (nrow(water$gsl) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = water$gsl,
        fill = .C_LAKE_FILL,
        color = .C_LAKE_BDR,
        linewidth = 0.10
      )
  }
  if (nrow(water$rivers) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = water$rivers,
        fill = .C_RIVER_FILL,
        color = .C_RIVER_BDR,
        linewidth = 0.10
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
          linewidth = 0.10,
          alpha = 0.20
        )
    }
    if (nrow(ctr_500) > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = ctr_500,
          color = .C_CTR_500,
          linewidth = 0.15,
          alpha = 0.20
        )
    }
    if (nrow(ctr_1000) > 0) {
      p <- p +
        ggplot2::geom_sf(
          data = ctr_1000,
          color = .C_CTR_1000,
          linewidth = 0.20,
          alpha = 0.20
        )
    }
  }

  # 5. Parks & recreation fills
  if (nrow(rec$cemeteries) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = rec$cemeteries,
        fill = .C_PARK_FILL,
        color = .C_PARK_BDR,
        linewidth = 0.10
      )
  }
  if (nrow(rec$golf) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = rec$golf,
        fill = .C_PARK_FILL,
        color = .C_PARK_BDR,
        linewidth = 0.10
      )
  }
  if (nrow(rec$parks) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = rec$parks,
        fill = .C_PARK_FILL,
        color = .C_PARK_BDR,
        linewidth = 0.10
      )
  }

  # National monuments — 3-stroke at z13+, 2-stroke below
  if (nrow(mon_natl) > 0) {
    if (zoom >= 13) {
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = .C_NM_OUTER,
          linewidth = 0.80
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = .C_NM_MID,
          linewidth = 0.50
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = .C_NM_INNER,
          linewidth = 0.10
        )
    } else if (zoom >= 10) {
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = "#C7C0BD",
          linewidth = 0.50
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = .C_NM_INNER,
          linewidth = 0.10
        )
    } else {
      p <- p +
        ggplot2::geom_sf(
          data = mon_natl,
          fill = NA,
          color = "#CCC5C2",
          linewidth = 0.35
        )
    }
  }
  if (nrow(mon_sp) > 0) {
    if (zoom >= 13) {
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_OUTER,
          linewidth = 0.80
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_MID,
          linewidth = 0.35
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_NM_INNER,
          linewidth = 0.13
        )
    } else {
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_MID,
          linewidth = 0.35
        )
      p <- p +
        ggplot2::geom_sf(
          data = mon_sp,
          fill = NA,
          color = .C_SP_BDR,
          linewidth = 0.10
        )
    }
  }
  if (nrow(rec$ski) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = rec$ski,
        fill = NA,
        color = .C_SKI_BDR,
        linewidth = 0.10
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
  if (nrow(muni$muni2) > 0) {
    for (sv in c("0", "1", "2", "3")) {
      msf <- muni$muni2[
        !is.na(muni$muni2$map_symbol) & muni$muni2$map_symbol == sv,
      ]
      if (nrow(msf) == 0) {
        next
      }
      p <- p + ggplot2::geom_sf(data = msf, fill = .C_M2_FILL, color = NA)
      p <- p +
        ggplot2::geom_sf(
          data = msf,
          fill = NA,
          color = .C_M2_OUT[[sv]],
          linewidth = m2_ow
        )
      p <- p +
        ggplot2::geom_sf(
          data = msf,
          fill = NA,
          color = .C_M2_IN[[sv]],
          linewidth = 0.27
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
        linewidth = 0.10,
        linetype = "dotdash"
      )
  }

  # 9. Railroads
  if (nrow(roads_raw$railroads) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = roads_raw$railroads,
        color = .C_RAIL,
        linewidth = 0.11
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

  # 12. Trails
  if (nrow(rec$trails) > 0) {
    tw <- if (zoom >= 15) 0.33 else 0.30
    tf <- if (zoom >= 15) 0.17 else 0.13
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

  # 13. Transit — commuter rail
  if (nrow(transit$commuter_rail) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = transit$commuter_rail,
        color = .C_TR_CAS,
        linewidth = 1.2
      )
    p <- p +
      ggplot2::geom_sf(
        data = transit$commuter_rail,
        color = .C_TR_FIL,
        linewidth = 0.6
      )
    p <- p +
      ggplot2::geom_sf(
        data = transit$commuter_rail,
        color = .C_FRONTRUNNER,
        linewidth = 0.6,
        linetype = "42"
      )
  }

  # TRAX — loop per colour to avoid scale conflicts
  if (nrow(trax) > 0) {
    for (cv in unique(trax$trax_color)) {
      tr_sub <- trax[trax$trax_color == cv, ]
      p <- p +
        ggplot2::geom_sf(data = tr_sub, color = .C_TR_CAS, linewidth = 1.0)
      p <- p +
        ggplot2::geom_sf(data = tr_sub, color = .C_TR_FIL, linewidth = 0.5)
      p <- p +
        ggplot2::geom_sf(
          data = tr_sub,
          color = cv,
          linewidth = 0.5,
          linetype = "53"
        )
    }
  }
  if (nrow(transit$rail_stops) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = transit$rail_stops,
        shape = 21,
        size = 1.8,
        fill = "white",
        color = .C_TRAX_BLU,
        stroke = 0.5
      )
  }
  if (nrow(transit$cr_stops) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = transit$cr_stops,
        shape = 21,
        size = 1.8,
        fill = "white",
        color = "#9B7BBD",
        stroke = 0.5
      )
  }

  # 14. Buildings
  if (nrow(buildings) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = buildings,
        fill = .C_BLDG_FILL,
        color = .C_BLDG_BDR,
        linewidth = 0.04
      )
  }

  # ─ LABELS ────────────────────────────────────────────────────────────────

  # 15. County labels — Poller One, light text on gray halo
  if (nrow(cnty_lbl) > 0 && any(!is.na(cnty_lbl$map_label))) {
    cls_i <- as.integer(suppressWarnings(median(
      cnty_lbl$map_label_class,
      na.rm = TRUE
    )))
    if (is.na(cls_i)) {
      cls_i <- 3L
    }
    lbl_col <- if (cls_i >= 3L) .C_CNTY_LO else .C_CNTY_HI
    p <- .lbl(
      p,
      cnty_lbl,
      col = lbl_col,
      halo = .C_CNTY_HALO,
      sz = .sz_county(cls_i),
      face = "bold",
      fam = .F_COUNTY
    )
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
downtown_map <- build_ugrc_map(lon = -111.89, lat = 40.76, zoom = 8)
print(downtown_map)

# export_excel.R — Excel backup export for OZ 2.0 Info Sheets
# Requires: openxlsx2 (install.packages("openxlsx2") then renv::snapshot())
# Called from index.qmd `report` chunk after report_data and tract_maps are built.

# ── Brand palette (hex strings — converted to wbColour via .xl_col()) ─────────
.XL_TEAL      <- "#003B4F"
.XL_TEAL_MID  <- "#0E5C72"
.XL_TEAL_SOFT <- "#3E7E91"
.XL_SKY       <- "#7EC8E3"
.XL_AMBER     <- "#C77B30"
.XL_KEY_TINT  <- "#F1F7F8"
.XL_HAIR      <- "#EEF3F4"
.XL_INK       <- "#16272D"
.XL_BODY      <- "#3C4A4F"
.XL_MUTED     <- "#7A848A"
.XL_PANEL     <- "#F7FAFB"
.XL_LINE      <- "#CADDE2"
.XL_WHITE     <- "#FFFFFF"
.XL_OZ_LIKELY <- "#15705A"
.XL_OZ_MORE   <- "#2F8FB0"
.XL_OZ_LESS   <- "#D98A3D"
.XL_OZ_UNLKLY <- "#B4532E"
.XL_OZ_UNK    <- "#9AA3A8"
.XL_YELLOW_TINT <- "#FFFDE0"   # light yellow for key metric value cells

# ── Convert "#RRGGBB" hex string → wbColour (strips # prefix) ─────────────────
.xl_col <- function(hex) {
  openxlsx2::wb_color(hex = sub("^#", "", as.character(hex)))
}


# ── Small helpers ──────────────────────────────────────────────────────────────

.xl_oz_color <- function(cls) {
  if (is.null(cls) || length(cls) == 0 || is.na(cls)) return(.XL_OZ_UNK)
  cls <- as.character(cls)
  if (grepl("Likely to attract", cls, ignore.case = TRUE) &&
      !grepl("More|Less|Un", cls, ignore.case = TRUE)) return(.XL_OZ_LIKELY)
  if (grepl("More likely",         cls, ignore.case = TRUE)) return(.XL_OZ_MORE)
  if (grepl("Less likely",         cls, ignore.case = TRUE)) return(.XL_OZ_LESS)
  if (grepl("Unlikely|not likely", cls, ignore.case = TRUE)) return(.XL_OZ_UNLKLY)
  .XL_OZ_UNK
}

.xl_oz_short <- function(cls) {
  if (is.null(cls) || length(cls) == 0 || is.na(cls)) return("—")
  cls <- as.character(cls)
  if (grepl("Likely to attract", cls, ignore.case = TRUE) &&
      !grepl("More|Less|Un", cls, ignore.case = TRUE)) return("Likely")
  if (grepl("More likely",         cls, ignore.case = TRUE)) return("More Likely")
  if (grepl("Less likely",         cls, ignore.case = TRUE)) return("Less Likely")
  if (grepl("Unlikely|not likely", cls, ignore.case = TRUE)) return("Unlikely")
  cls
}

.xl_cv <- function(data, col) {
  if (!col %in% names(data)) return(NA_real_)
  v <- data[[col]]
  if (length(v) == 0) NA_real_ else v[[1]]
}

.xl_fmt <- function(x, digits = 1, suffix = "", big_mark = "") {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("—")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) || !is.finite(x)) return("—")
  rounded <- round(x, digits)
  if (digits == 0 && nchar(big_mark) > 0) {
    return(paste0(format(as.integer(rounded), big.mark = big_mark, scientific = FALSE), suffix))
  }
  paste0(format(rounded, nsmall = digits, scientific = FALSE), suffix)
}

.xl_tract_label <- function(geoid) {
  geoid <- as.character(geoid)
  sfx  <- substr(geoid, 6, 11)
  base <- as.integer(substr(sfx, 1, 4))
  dec  <- substr(sfx, 5, 6)
  if (is.na(dec) || nchar(trimws(dec)) == 0 || dec == "00") paste0("Tract ", base)
  else paste0("Tract ", base, ".", dec)
}

# ── XLOOKUP helpers ───────────────────────────────────────────────────────────

.col_letter <- function(n) {
  result <- character(0)
  while (n > 0L) {
    result <- c(LETTERS[(n - 1L) %% 26L + 1L], result)
    n      <- (n - 1L) %/% 26L
  }
  paste(result, collapse = "")
}

# Column positions in the All Data sheet (one row per tract, 44 cols)
.AD <- c(
  GEOID                      =  1L, county                     =  2L,
  tract_label                =  3L, cities                     =  4L,
  pop                        =  5L, hh                         =  6L,
  total_acres                =  7L, dev_acres                  =  8L,
  oz1_acres                  =  9L, oz1_pct                    = 10L,
  wc_muc_pct                 = 11L, wc_pct_metropolitan_center = 12L,
  wc_pct_urban_center        = 13L, wc_pct_city_center         = 14L,
  wc_pct_neighborhood_center = 15L, wc_pct_employment_district = 16L,
  wc_pct_industrial_district = 17L, wc_pct_retail_district     = 18L,
  units_total                = 19L, res_acres_total            = 20L,
  res_acres_sfd              = 21L, res_acres_mf_sfa           = 22L,
  transit_stop_count         = 23L, sap_acres                  = 24L,
  sap_pct                    = 25L, ato_jobtransit             = 26L,
  ato_hhtransit              = 27L, sap_planned_hh_per_acre    = 28L,
  freeway_exit_count         = 29L, exit_dist_edge_mi          = 30L,
  ato_jobauto                = 31L, ato_hhauto                 = 32L,
  ato_composite              = 33L, ato_pct_county_avg         = 34L,
  hh_added                   = 35L, pop_added                  = 36L,
  jobs_added                 = 37L, eru_added                  = 38L,
  eru_per_acre               = 39L, pvrty_r                    = 40L,
  unmpl_r                    = 41L, mfi                        = 42L,
  oztoolclassification       = 43L, oz_short                   = 44L
)

# Excel number formats per metric (used in All Data sheet and formula cells)
.AD_FMT <- c(
  GEOID                      = "@",         county                     = "@",
  tract_label                = "@",         pop                        = "#,##0",
  hh                         = "#,##0",     total_acres                = "0.0",
  dev_acres                  = "0.0",       oz1_acres                  = "0.0",
  oz1_pct                    = '0.0"%"',    wc_muc_pct                 = '0.0"%"',
  wc_pct_metropolitan_center = '0.0"%"',    wc_pct_urban_center        = '0.0"%"',
  wc_pct_city_center         = '0.0"%"',    wc_pct_neighborhood_center = '0.0"%"',
  wc_pct_employment_district = '0.0"%"',    wc_pct_industrial_district = '0.0"%"',
  wc_pct_retail_district     = '0.0"%"',    units_total                = "#,##0",
  res_acres_total            = "0.0",       res_acres_sfd              = "0.0",
  res_acres_mf_sfa           = "0.0",       transit_stop_count         = "#,##0",
  sap_acres                  = "0.0",       sap_pct                    = '0.0"%"',
  ato_jobtransit             = "#,##0",     ato_hhtransit              = "#,##0",
  sap_planned_hh_per_acre    = "0.0",       freeway_exit_count         = "#,##0",
  exit_dist_edge_mi          = "0.00",      ato_jobauto                = "#,##0",
  ato_hhauto                 = "#,##0",     ato_composite              = "0.0",
  hh_added                   = "#,##0",     pop_added                  = "#,##0",
  jobs_added                 = "#,##0",     eru_added                  = "0.0",
  eru_per_acre               = "0.00",      pvrty_r                    = '0.0"%"',
  unmpl_r                    = '0.0"%"',    mfi                        = '"$"#,##0',
  oztoolclassification       = "@",         oz_short                   = "@",
  cities                     = "@",        ato_pct_county_avg         = '0.0"%"'
)

# Build an INDEX/MATCH formula: looks up geoid_ref in All Data col A,
# returns the column for `metric`. Uses INDEX/MATCH (all Excel versions).
# The &"" coerces the lookup value to text, preventing type-mismatch failures
# when Excel silently converts 11-digit GEOIDs from text to numbers.
# Numeric fields: blank All Data cells (NA in pipeline) show "Not Available/Applicable"
# rather than 0, so non-MPO metrics (SAP, ATO) are honest about missing coverage.
.xlup <- function(geoid_ref, metric, ad_sheet = "'All Data'") {
  if (!metric %in% names(.AD)) return('""')
  cl  <- .col_letter(.AD[[metric]])
  fmt <- .AD_FMT[[metric]]
  idx <- paste0('INDEX(', ad_sheet, '!$', cl, ':$', cl,
                ',MATCH(', geoid_ref, '&"",', ad_sheet, '!$A:$A,0))')
  if (!is.na(fmt) && fmt != "@") {
    paste0('=IFERROR(IF(', idx, '="","Not Available/Applicable",', idx, '),"-")')
  } else {
    paste0('=IFERROR(', idx, ',"-")')
  }
}

# Raw INDEX/MATCH expression for embedding inside larger CONCAT formulas (no leading =).
# Returns: IFERROR(INDEX(ad_sheet!$col:$col,MATCH(geoid_ref&"",ad_sheet!$A:$A,0)),fallback)
.xlmatch <- function(geoid_ref, metric, fallback = '""', ad_sheet = "'All Data'") {
  if (!metric %in% names(.AD)) return('""')
  cl <- .col_letter(.AD[[metric]])
  paste0('IFERROR(INDEX(', ad_sheet, '!$', cl, ':$', cl,
         ',MATCH(', geoid_ref, '&"",', ad_sheet, '!$A:$A,0)),', fallback, ')')
}

# Cell dims shortcuts
.d1 <- function(row, col) openxlsx2::wb_dims(rows = row, cols = col)
.dr <- function(rows, cols) openxlsx2::wb_dims(rows = rows, cols = cols)

# Write a single scalar to one cell
.wc <- function(wb, sheet, row, col, value) {
  openxlsx2::wb_add_data(wb, sheet, x = value,
    dims = .d1(row, col), col_names = FALSE)
}

# Merge + fill + font a range — the most repeated pattern.
# fill, font_color: plain "#RRGGBB" strings (converted internally to wbColour).
.mff <- function(wb, sheet, rows, cols, value = NULL,
                 fill = NULL, font_color = .XL_WHITE, font_size = 9,
                 bold = FALSE, h_align = "left", v_align = "center",
                 wrap = FALSE) {
  dims <- .dr(rows, cols)
  if (!is.null(value)) {
    wb <- .wc(wb, sheet, min(rows), min(cols), value)
  }
  if (length(cols) > 1 || length(rows) > 1) {
    wb <- openxlsx2::wb_merge_cells(wb, sheet, dims = dims)
  }
  if (!is.null(fill)) {
    wb <- openxlsx2::wb_add_fill(wb, sheet, dims = dims, color = .xl_col(fill))
  }
  wb <- openxlsx2::wb_add_font(wb, sheet, dims = dims,
    name = "Poppins", size = font_size, bold = bold,
    color = .xl_col(font_color))
  wb <- openxlsx2::wb_add_cell_style(wb, sheet, dims = dims,
    horizontal = h_align, vertical = v_align, wrap_text = wrap)
  wb
}

# ── Save tract maps as PNGs for Excel embedding ───────────────────────────────

.save_map_pngs_xl <- function(tract_maps, geoids, map_dir,
                               width = 2.27, height = 3.18, dpi = 300) {
  dir.create(map_dir, showWarnings = FALSE, recursive = TRUE)
  purrr::set_names(
    purrr::map_chr(geoids, function(geoid) {
      path <- file.path(map_dir, paste0(geoid, "_xl.png"))
      if (!file.exists(path) && geoid %in% names(tract_maps)) {
        suppressWarnings(
          ggplot2::ggsave(
            filename = path,
            plot     = tract_maps[[geoid]],
            width    = width,
            height   = height,
            dpi      = dpi,
            bg       = "#F7F4EF"
          )
        )
      }
      path
    }),
    geoids
  )
}

# ── All Data sheet (flat database, one row per tract, 42 cols) ───────────────

.write_alldata_sheet <- function(wb, report_data) {
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = "All Data",
    tab_color = .xl_col(.XL_TEAL_SOFT))

  n_tracts <- nrow(report_data)

  ad <- data.frame(
    "GEOID"                  = as.character(report_data$GEOID),
    "County"                 = as.character(report_data$county),
    "Tract"                  = vapply(as.character(report_data$GEOID),
                                      .xl_tract_label, character(1L)),
    "Cities"                 = as.character(report_data$cities),
    "Population"             = report_data$pop,
    "Households"             = report_data$hh,
    "Total Acres"            = report_data$total_acres,
    "Dev Acres"              = report_data$dev_acres,
    "OZ 1.0 Acres"           = report_data$oz1_acres,
    "OZ 1.0 %"               = report_data$oz1_pct,
    "WC MUorC %"             = report_data$wc_muc_pct,
    "Metropolitan Center %"  = report_data$wc_pct_metropolitan_center,
    "Urban Center %"         = report_data$wc_pct_urban_center,
    "City Center %"          = report_data$wc_pct_city_center,
    "Neighborhood Center %"  = report_data$wc_pct_neighborhood_center,
    "Employment District %"  = report_data$wc_pct_employment_district,
    "Industrial District %"  = report_data$wc_pct_industrial_district,
    "Retail District %"      = report_data$wc_pct_retail_district,
    "Total Units"            = report_data$units_total,
    "Residential Acres"      = report_data$res_acres_total,
    "SFD Acres"              = report_data$res_acres_sfd,
    "MF/SFA Acres"           = report_data$res_acres_mf_sfa,
    "Rail/BRT Stops"         = report_data$transit_stop_count,
    "SAP Acres"              = report_data$sap_acres,
    "SAP %"                  = report_data$sap_pct,
    "ATO Jobs Transit"       = report_data$ato_jobtransit,
    "ATO HH Transit"         = report_data$ato_hhtransit,
    "SAP HH/ac Planned"      = report_data$sap_planned_hh_per_acre,
    "Freeway Exits"          = report_data$freeway_exit_count,
    "Exit Distance (mi)"     = report_data$exit_dist_edge_mi,
    "ATO Jobs Auto"          = report_data$ato_jobauto,
    "ATO HH Auto"            = report_data$ato_hhauto,
    "ATO Composite /100"     = report_data$ato_composite,
    "ATO % of County Avg"    = rep(NA_real_, nrow(report_data)),
    "HH Added"               = report_data$hh_added,
    "Pop Added"              = report_data$pop_added,
    "Jobs Added"             = report_data$jobs_added,
    "ERU Added"              = report_data$eru_added,
    "ERU/ac"                 = report_data$eru_per_acre,
    "Poverty Rate"           = report_data$pvrty_r,
    "Unemployment Rate"      = report_data$unmpl_r,
    "Median Family Income"   = report_data$mfi,
    "OZ Tool Classification" = as.character(report_data$oztoolclassification),
    "Likelihood (Short)"     = vapply(as.character(report_data$oztoolclassification),
                                      .xl_oz_short, character(1L)),
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )

  n_cols <- 44L

  # Row 1: title
  wb <- .mff(wb, "All Data", 1L, 1:n_cols,
    value = paste0("OZ 2.0 Candidate Tracts — All Data  |  ",
                   format(Sys.Date(), "%B %d, %Y")),
    fill = .XL_TEAL, font_size = 13, bold = TRUE)
  wb <- openxlsx2::wb_set_row_heights(wb, "All Data", rows = 1L, heights = 26)

  # Row 2: column headers — styled manually.
  # wb_add_data_table is intentionally avoided: applying wb_add_formula /
  # wb_add_numfmt to cells inside an Excel Table range produces conflicting
  # XML that Excel flags as a repair error on open.
  col_labels <- names(ad)
  for (j in seq_len(n_cols)) {
    wb <- .wc(wb, "All Data", 2L, j, col_labels[j])
    wb <- openxlsx2::wb_add_fill(wb, "All Data",
      dims = .d1(2L, j), color = .xl_col(.XL_TEAL))
    wb <- openxlsx2::wb_add_font(wb, "All Data", dims = .d1(2L, j),
      name = "Poppins", size = 8, bold = TRUE, color = .xl_col(.XL_WHITE))
    wb <- openxlsx2::wb_add_cell_style(wb, "All Data", dims = .d1(2L, j),
      horizontal = "center", vertical = "center", wrap_text = TRUE)
  }
  wb <- openxlsx2::wb_set_row_heights(wb, "All Data", rows = 2L, heights = 20)
  wb <- openxlsx2::wb_add_filter(wb, "All Data", rows = 2L, cols = 1:n_cols)

  # Data rows (3 to n_tracts+2)
  wb <- openxlsx2::wb_add_data(wb, "All Data", x = ad,
    dims = "A3", col_names = FALSE)

  # openxlsx2 writes NA_real_ as Excel's #N/A error, not an empty cell.
  # #N/A in numeric ranges breaks MIN/MAX (error propagates → all ATO formulas
  # return ""), and IF(cell="","") comparisons propagate the error instead of
  # returning blank. Overwrite each NA cell with "" so it is blank to Excel.
  for (.na_col in c("oz1_acres", "sap_acres", "sap_planned_hh_per_acre",
                    "exit_dist_edge_mi", "ato_jobauto", "ato_hhauto",
                    "ato_jobtransit", "ato_hhtransit")) {
    .na_rows <- which(is.na(report_data[[.na_col]])) + 2L
    for (.na_r in .na_rows) {
      wb <- openxlsx2::wb_add_data(wb, "All Data", x = "",
                                   dims = .d1(.na_r, .AD[[.na_col]]), col_names = FALSE)
    }
  }
  rm(.na_col, .na_rows)

  wb <- openxlsx2::wb_set_col_widths(wb, "All Data", cols = 1:n_cols,
    widths = c(14, 12, 12, 28, rep(10, 29), 14, rep(10, 10)))

  # Number formats on data cells (apply @ to text cols so Excel keeps them as text)
  data_rows <- 3L:(n_tracts + 2L)
  for (metric in names(.AD)) {
    fmt <- .AD_FMT[[metric]]
    if (!is.na(fmt)) {
      wb <- openxlsx2::wb_add_numfmt(wb, "All Data",
        dims = .dr(data_rows, .AD[[metric]]), numfmt = fmt)
    }
  }

  # Derived columns: overwrite values from wb_add_data with Excel formulas
  # so the sheet self-updates if raw inputs are edited.
  .cl <- function(m) .col_letter(.AD[[m]])
  end_row <- n_tracts + 2L   # last data row; used for ATO min-max ranges
  for (row_i in seq_len(n_tracts)) {
    r <- row_i + 2L

    # Residential acres total  =  SFD acres + MF/SFA acres
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["res_acres_total"]]),
      x = paste0("=", .cl("res_acres_sfd"), r, "+", .cl("res_acres_mf_sfa"), r))

    # OZ 1.0 %  =  OZ 1.0 Acres / Total Acres × 100
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["oz1_pct"]]),
      x = paste0("=IFERROR(", .cl("oz1_acres"), r,
                 "/", .cl("total_acres"), r, "*100,0)"))

    # WC MUorC %  =  Metropolitan + Urban + City center percentages
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["wc_muc_pct"]]),
      x = paste0("=", .cl("wc_pct_metropolitan_center"), r,
                 "+", .cl("wc_pct_urban_center"), r,
                 "+", .cl("wc_pct_city_center"), r))

    # SAP %  =  SAP Acres / Total Acres × 100 (blank when SAP acres unavailable)
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["sap_pct"]]),
      x = paste0('=IF(', .cl("sap_acres"), r, '="","",',
                 'IFERROR(', .cl("sap_acres"), r,
                 '/', .cl("total_acres"), r, '*100,0))'))

    # ERU Added  =  HH Added + Jobs Added × 0.4
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["eru_added"]]),
      x = paste0("=", .cl("hh_added"), r, "+", .cl("jobs_added"), r, "*0.4"))

    # ERU/ac  =  ERU Added / Dev Acres
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["eru_per_acre"]]),
      x = paste0("=IFERROR(", .cl("eru_added"), r,
                 "/", .cl("dev_acres"), r, ",0)"))

    # ATO Composite  =  average of 4 min-max-normalised subscore components (0-100)
    # Blank when subscores are unavailable (non-MPO tracts).
    .norm <- function(m) {
      cl  <- .cl(m)
      rng <- paste0("$", cl, "$3:$", cl, "$", end_row)
      paste0("(", cl, r, "-MIN(", rng, "))/(MAX(", rng, ")-MIN(", rng, "))*100")
    }
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["ato_composite"]]),
      x = paste0(
        '=IFERROR(IF(', .cl("ato_jobauto"), r, '="","",(',
        .norm("ato_jobauto"),   "+",
        .norm("ato_hhauto"),    "+",
        .norm("ato_jobtransit"), "+",
        .norm("ato_hhtransit"),
        ')/4),"")'
      ))

    # ATO % of county avg  =  tract composite / AVERAGEIF(same county) × 100
    # AVERAGEIF ignores blank cells, so non-MPO tracts are excluded from county avg.
    ato_cl <- .cl("ato_composite")
    cty_cl <- .cl("county")
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["ato_pct_county_avg"]]),
      x = paste0(
        '=IFERROR(IF(', ato_cl, r, '="","",',
        ato_cl, r,
        '/AVERAGEIF($', cty_cl, ':$', cty_cl, ',$', cty_cl, r,
        ',$', ato_cl, ':$', ato_cl, ')*100),"")'
      ))

    # oz_short (col AP): static fill + white font keyed to classification
    oz_cls_i <- if (is.na(report_data$oztoolclassification[row_i])) NA_character_
                else as.character(report_data$oztoolclassification[row_i])
    wb <- openxlsx2::wb_add_fill(wb, "All Data",
      dims = .d1(r, .AD[["oz_short"]]),
      color = .xl_col(.xl_oz_color(oz_cls_i)))
    wb <- openxlsx2::wb_add_font(wb, "All Data",
      dims = .d1(r, .AD[["oz_short"]]),
      name = "Poppins", size = 9, bold = TRUE, color = .xl_col(.XL_WHITE))
  }

  wb <- openxlsx2::wb_freeze_pane(wb, "All Data",
    first_active_row = 3L, first_active_col = 2L)

  wb
}

# ── README sheet ──────────────────────────────────────────────────────────────

.write_readme_sheet <- function(wb, county_names) {
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = "README",
    tab_color = .xl_col(.XL_TEAL_MID))
  wb <- openxlsx2::wb_set_col_widths(wb, "README", cols = 1:4,
    widths = c(28, 55, 28, 28))

  # Row 1: Title
  wb <- .mff(wb, "README", 1, 1:4,
    value = "OZ 2.0 Candidate Tracts — Excel Export",
    fill = .XL_TEAL, font_size = 16, bold = TRUE)
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = 1, heights = 30)

  # Row 2: Subtitle
  wb <- .mff(wb, "README", 2, 1:4,
    value = paste0("WFRC & MAG Region · Wasatch Front, Utah · Generated ",
                   format(Sys.Date(), "%B %d, %Y")),
    fill = .XL_TEAL_MID, font_size = 9, font_color = .XL_SKY)
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = 2, heights = 18)

  # Row 3: blank
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = 3, heights = 8)

  # Row 4: ABOUT section header
  wb <- .mff(wb, "README", 4, 1:4,
    value = "ABOUT THIS WORKBOOK",
    fill = .XL_KEY_TINT, font_size = 8, bold = TRUE, font_color = .XL_AMBER)
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = 4, heights = 16)

  # Rows 5-9: Methodology text
  method_text <- paste0(
    "This workbook presents the WFRC/MAG Opportunity Zone 2.0 candidate tract analysis ",
    "for the nine-county Wasatch Front region. It covers every census tract on the ",
    "federal OZ 2.0 eligible list, with metrics spanning land use, transit access, ",
    "housing inventory, projected growth, and Access to Opportunities indicators. ",
    "The Summary sheet provides a regional overview of all eligible tracts. ",
    "Per-county sheets offer a detailed profile for each tract, including an embedded ",
    "map and key metrics organized by theme. The All Data sheet contains the complete ",
    "set of metrics in a flat, filterable table for cross-tract comparison and analysis."
  )
  wb <- .wc(wb, "README", 5, 1, method_text)
  wb <- openxlsx2::wb_merge_cells(wb, "README", dims = .dr(5:9, 1:4))
  wb <- openxlsx2::wb_add_fill(wb, "README", dims = .dr(5:9, 1:4), color = .xl_col(.XL_PANEL))
  wb <- openxlsx2::wb_add_font(wb, "README", dims = .dr(5:9, 1:4),
    name = "Poppins", size = 9, color = .xl_col(.XL_BODY))
  wb <- openxlsx2::wb_add_cell_style(wb, "README", dims = .dr(5:9, 1:4),
    horizontal = "left", vertical = "top", wrap_text = TRUE)
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = 5:9, heights = 15)

  # Row 10: blank
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = 10, heights = 8)

  # Row 11: SHEETS section header
  wb <- .mff(wb, "README", 11, 1:4,
    value = "SHEETS IN THIS WORKBOOK",
    fill = .XL_KEY_TINT, font_size = 8, bold = TRUE, font_color = .XL_AMBER)
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = 11, heights = 16)

  # Row 12: sheet table column headers
  for (j in 1:2) {
    lbl <- c("Sheet Name", "Description")[j]
    wb <- .wc(wb, "README", 12, j, lbl)
    wb <- openxlsx2::wb_add_fill(wb, "README", dims = .d1(12, j), color = .xl_col(.XL_TEAL))
    wb <- openxlsx2::wb_add_font(wb, "README", dims = .d1(12, j),
      name = "Poppins", size = 8, bold = TRUE, color = .xl_col(.XL_WHITE))
    wb <- openxlsx2::wb_add_cell_style(wb, "README", dims = .d1(12, j),
      horizontal = "left", vertical = "center")
  }
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = 12, heights = 16)

  # Rows 13+: sheet guide (All Data last)
  sheet_guide <- c("README", "Summary", paste0(county_names, " County"), "All Data")
  sheet_desc  <- c(
    "This sheet: about this workbook, abbreviation glossary, and data sources.",
    "Regional overview: one row per tract with key profile and access metrics for all eligible tracts across the Wasatch Front.",
    paste0(county_names, " County: per-tract detail blocks with embedded map and metrics organized by theme."),
    "Complete dataset: all metrics for every tract in one filterable table. Use this sheet for cross-tract comparison and custom analysis."
  )
  for (k in seq_along(sheet_guide)) {
    row_r  <- 12 + k
    fill_c <- if (k %% 2 == 0) .XL_WHITE else .XL_HAIR
    wb <- .wc(wb, "README", row_r, 1, sheet_guide[k])
    wb <- .wc(wb, "README", row_r, 2, sheet_desc[k])
    wb <- openxlsx2::wb_add_fill(wb, "README", dims = .dr(row_r, 1:2), color = .xl_col(fill_c))
    wb <- openxlsx2::wb_add_font(wb, "README", dims = .d1(row_r, 1),
      name = "Poppins", size = 8, bold = TRUE, color = .xl_col(.XL_TEAL))
    wb <- openxlsx2::wb_add_font(wb, "README", dims = .d1(row_r, 2),
      name = "Poppins", size = 8, color = .xl_col(.XL_BODY))
    wb <- openxlsx2::wb_add_cell_style(wb, "README", dims = .dr(row_r, 1:2),
      horizontal = "left", vertical = "center", wrap_text = TRUE)
  }
  wb <- openxlsx2::wb_set_row_heights(wb, "README",
    rows = 13:(12 + length(sheet_guide)), heights = 16)

  # Abbreviation glossary
  abbr_start <- 12 + length(sheet_guide) + 2
  wb <- .mff(wb, "README", abbr_start, 1:4,
    value = "ABBREVIATIONS & GLOSSARY",
    fill = .XL_KEY_TINT, font_size = 8, bold = TRUE, font_color = .XL_AMBER)
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = abbr_start, heights = 16)

  abbrevs <- list(
    c("ATO",        "Access to Opportunities",
      "Composite 0-100 index combining driving and transit access to jobs and households."),
    c("ERU",        "Equivalent Residential Unit",
      "Growth density metric: 1 household = 1 ERU, 1 job = 0.4 ERU."),
    c("SAP",        "Station Area Plan",
      "Adopted housing plan within 1/2-mile of a rail or BRT stop."),
    c("WC / MUorC", "Wasatch Choice / Metro-Urban-City Center",
      "WFRC/MAG regional growth center framework. MUorC = Metropolitan, Urban, or City center types."),
    c("OZ 1.0",     "Opportunity Zones (2018 round)",
      "Prior federal OZ designations; overlap shown for context."),
    c("OZ 2.0",     "Opportunity Zones 2.0 (current)",
      "Current federal eligible tract list under evaluation."),
    c("TAZ",        "Traffic Analysis Zone",
      "WFRC/MAG forecast unit; growth projections are area-proportioned from TAZ to tract."),
    c("DEVACRES",   "Developable Acres",
      "Developable land within a TAZ from the ATO layer; used as intersection weight for ATO scores."),
    c("GEOID",      "Census Tract Geographic ID",
      "11-digit FIPS code: 2-digit state + 3-digit county + 6-digit tract.")
  )

  hdr_row <- abbr_start + 1
  for (j in 1:3) {
    lbl <- c("Term", "Full Name", "Description")[j]
    wb <- .wc(wb, "README", hdr_row, j, lbl)
    wb <- openxlsx2::wb_add_fill(wb, "README", dims = .d1(hdr_row, j), color = .xl_col(.XL_TEAL))
    wb <- openxlsx2::wb_add_font(wb, "README", dims = .d1(hdr_row, j),
      name = "Poppins", size = 8, bold = TRUE, color = .xl_col(.XL_WHITE))
    wb <- openxlsx2::wb_add_cell_style(wb, "README", dims = .d1(hdr_row, j),
      horizontal = "left", vertical = "center")
  }
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = hdr_row, heights = 16)

  for (k in seq_along(abbrevs)) {
    row_r  <- hdr_row + k
    fill_c <- if (k %% 2 == 0) .XL_WHITE else .XL_HAIR
    for (j in 1:3) {
      wb <- .wc(wb, "README", row_r, j, abbrevs[[k]][j])
      wb <- openxlsx2::wb_add_fill(wb, "README", dims = .d1(row_r, j), color = .xl_col(fill_c))
      wb <- openxlsx2::wb_add_font(wb, "README", dims = .d1(row_r, j),
        name = "Poppins", size = 8, bold = (j == 1),
        color = .xl_col(if (j == 1) .XL_TEAL else .XL_BODY))
      wb <- openxlsx2::wb_add_cell_style(wb, "README", dims = .d1(row_r, j),
        horizontal = "left", vertical = "top", wrap_text = TRUE)
    }
    wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = row_r, heights = 28)
  }

  # Data sources
  src_row <- hdr_row + length(abbrevs) + 2
  wb <- .mff(wb, "README", src_row, 1:4,
    value = "DATA SOURCES",
    fill = .XL_KEY_TINT, font_size = 8, bold = TRUE, font_color = .XL_AMBER)
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = src_row, heights = 16)

  sources <- c(
    "Federal OZ 2.0 Eligibility List — U.S. Treasury / HUD",
    "Urban Institute OZ Tool Classification — Urban Institute",
    "Wasatch Choice for 2050 Centers & Districts — WFRC/MAG",
    "ACS Population, Households, Income, Poverty, Unemployment — U.S. Census Bureau",
    "UTA Rail/BRT Stops — Utah Transit Authority",
    "Station Area Plans (SAP) — WFRC/MAG",
    "Freeway Interchanges — WFRC/MAG",
    "Housing Inventory — WFRC/MAG",
    "Access to Opportunities (ATO) Scores — WFRC/MAG",
    "TAZ Growth Projections 2027-2037 — WFRC/MAG"
  )
  for (k in seq_along(sources)) {
    row_r  <- src_row + k
    fill_c <- if (k %% 2 == 0) .XL_WHITE else .XL_HAIR
    wb <- .wc(wb, "README", row_r, 1, sources[k])
    wb <- openxlsx2::wb_merge_cells(wb, "README", dims = .dr(row_r, 1:4))
    wb <- openxlsx2::wb_add_fill(wb, "README", dims = .dr(row_r, 1:4), color = .xl_col(fill_c))
    wb <- openxlsx2::wb_add_font(wb, "README", dims = .d1(row_r, 1),
      name = "Poppins", size = 8, color = .xl_col(.XL_BODY))
    wb <- openxlsx2::wb_add_cell_style(wb, "README", dims = .d1(row_r, 1),
      horizontal = "left", vertical = "center")
    wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = row_r, heights = 15)
  }

  # ── Methodology notes ─────────────────────────────────────────────────────
  meth_start <- src_row + length(sources) + 2
  wb <- .mff(wb, "README", meth_start, 1:4,
    value = "METHODOLOGY NOTES",
    fill = .XL_KEY_TINT, font_size = 8, bold = TRUE, font_color = .XL_AMBER)
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = meth_start, heights = 16)

  meth_hdr <- meth_start + 1
  for (j in 1:2) {
    wb <- .wc(wb, "README", meth_hdr, j, c("Metric", "How It Is Calculated")[j])
    wb <- openxlsx2::wb_add_fill(wb, "README", dims = .d1(meth_hdr, j), color = .xl_col(.XL_TEAL))
    wb <- openxlsx2::wb_add_font(wb, "README", dims = .d1(meth_hdr, j),
      name = "Poppins", size = 8, bold = TRUE, color = .xl_col(.XL_WHITE))
    wb <- openxlsx2::wb_add_cell_style(wb, "README", dims = .d1(meth_hdr, j),
      horizontal = "left", vertical = "center")
  }
  wb <- openxlsx2::wb_merge_cells(wb, "README", dims = .dr(meth_hdr, 2:4))
  wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = meth_hdr, heights = 16)

  meth_entries <- list(
    list(
      metric = "ATO Composite Score",
      text   = paste0(
        "Each of the four raw ATO subscores — Jobs (Auto), Households (Auto), Jobs (Transit), ",
        "Households (Transit) — is rescaled 0–100 using min-max normalization across all ",
        "MPO-region tracts in this export. The four normalized values are then averaged to produce ",
        "a single 0–100 composite index (higher = greater access). ",
        "Scores are relative to the tracts included in this export: adding or removing tracts ",
        "would shift individual values."
      )
    ),
    list(
      metric = "ATO % of County Avg",
      text   = paste0(
        "Tract ATO Composite Score ÷ average ATO Composite Score for all tracts in the same ",
        "county × 100. The county average is computed via AVERAGEIF in the All Data sheet and ",
        "excludes non-MPO tracts (Tooele County), which have no ATO data. ",
        "A value of 100 means the tract matches its county average; values above 100 indicate ",
        "above-average access for that county."
      )
    ),
    list(
      metric = "ERU Added / ERU per Acre",
      text   = paste0(
        "ERU (Equivalent Residential Unit) standardizes growth across land use types. ",
        "ERU Added = Households Added + (Jobs Added × 0.4). ",
        "ERU per Acre = ERU Added ÷ Developable Acres. ",
        "The 0.4 weight reflects that employment uses are less land-intensive than housing. ",
        "Growth figures are sourced from WFRC/MAG TAZ projections (2027–2037), ",
        "area-proportioned to census tract boundaries using developable acres as the weight."
      )
    ),
    list(
      metric = "Cities",
      text   = paste0(
        "Determined by spatial intersection of the census tract boundary with U.S. Census ",
        "TIGER/Line Place boundaries (incorporated cities, towns, and census-designated places). ",
        "Municipalities covering less than 2% of the tract area are excluded to avoid listing ",
        "artifacts from boundary slivers. Where multiple cities qualify, they are listed in ",
        "descending order of intersection area."
      )
    ),
    list(
      metric = "SAP & ATO Availability",
      text   = paste0(
        "Station Area Plan (SAP) and Access to Opportunities (ATO) metrics are derived from ",
        "WFRC/MAG datasets that cover only the MPO planning boundary. Tooele County tracts ",
        "fall outside this boundary and therefore have no SAP or ATO data. These cells show ",
        "'Not Available/Applicable' in the per-county detail sheets and are left blank in ",
        "the All Data sheet."
      )
    )
  )

  for (k in seq_along(meth_entries)) {
    row_r  <- meth_hdr + k
    fill_c <- if (k %% 2 == 0) .XL_WHITE else .XL_HAIR
    wb <- .wc(wb, "README", row_r, 1, meth_entries[[k]]$metric)
    wb <- .wc(wb, "README", row_r, 2, meth_entries[[k]]$text)
    wb <- openxlsx2::wb_merge_cells(wb, "README", dims = .dr(row_r, 2:4))
    wb <- openxlsx2::wb_add_fill(wb, "README", dims = .dr(row_r, 1:4), color = .xl_col(fill_c))
    wb <- openxlsx2::wb_add_font(wb, "README", dims = .d1(row_r, 1),
      name = "Poppins", size = 8, bold = TRUE, color = .xl_col(.XL_TEAL))
    wb <- openxlsx2::wb_add_font(wb, "README", dims = .d1(row_r, 2),
      name = "Poppins", size = 8, color = .xl_col(.XL_BODY))
    wb <- openxlsx2::wb_add_cell_style(wb, "README", dims = .d1(row_r, 1),
      horizontal = "left", vertical = "top")
    wb <- openxlsx2::wb_add_cell_style(wb, "README", dims = .d1(row_r, 2),
      horizontal = "left", vertical = "top", wrap_text = TRUE)
    wb <- openxlsx2::wb_set_row_heights(wb, "README", rows = row_r, heights = 52)
  }

  wb
}

# ── Summary sheet ─────────────────────────────────────────────────────────────

.write_summary_sheet <- function(wb, report_data) {
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = "Summary",
    tab_color = .xl_col(.XL_TEAL))
  # Cols: #, County, Tract, GEOID, Pop, HH, TotalAc, DevAc, OZ1%, MUorC%, ATO, SAP, ERU, Likelihood
  wb <- openxlsx2::wb_set_col_widths(wb, "Summary", cols = 1:14,
    widths = c(4, 12, 14, 13, 9, 9, 9, 9, 8, 8, 7, 7, 7, 22))

  # Row 1: document title
  wb <- .mff(wb, "Summary", 1, 1:14,
    value = paste0("OZ 2.0 Eligible Census Tracts — Wasatch Front Region  |  ",
                   format(Sys.Date(), "%B %d, %Y")),
    fill = .XL_TEAL, font_size = 14, bold = TRUE)
  wb <- openxlsx2::wb_set_row_heights(wb, "Summary", rows = 1, heights = 28)

  # Row 2: super-group headers
  super <- list(
    list(label = "#",                 cols = 1,     fill = .XL_TEAL,     fc = .XL_SKY),
    list(label = "IDENTITY",          cols = 2:4,   fill = .XL_TEAL,     fc = .XL_WHITE),
    list(label = "PROFILE",           cols = 5:9,   fill = .XL_TEAL,     fc = .XL_WHITE),
    list(label = "FOUR KEY MEASURES", cols = 10:13, fill = .XL_KEY_TINT, fc = .XL_AMBER),
    list(label = "INVESTMENT",        cols = 14,    fill = .XL_TEAL,     fc = .XL_WHITE)
  )
  for (sg in super) {
    wb <- .mff(wb, "Summary", 2, sg$cols,
      value = sg$label,
      fill = sg$fill, font_size = 7, bold = TRUE,
      font_color = sg$fc, h_align = "center", v_align = "bottom")
  }
  wb <- openxlsx2::wb_set_row_heights(wb, "Summary", rows = 2, heights = 14)

  # Row 3: column headers
  col_hdrs <- c(
    "#", "County", "Tract", "GEOID",
    "Population", "Households", "Total Acres", "Dev Acres", "OZ 1.0 %",
    "WC MUorC %", "ATO /100", "SAP HH/ac", "ERU/ac",
    "Investment Likelihood"
  )
  for (j in seq_along(col_hdrs)) {
    is_key <- j %in% 10:13
    bg     <- if (is_key) .XL_KEY_TINT else .XL_TEAL
    fg     <- if (is_key) .XL_INK      else .XL_WHITE
    wb <- .wc(wb, "Summary", 3, j, col_hdrs[j])
    wb <- openxlsx2::wb_add_fill(wb, "Summary", dims = .d1(3, j), color = .xl_col(bg))
    wb <- openxlsx2::wb_add_font(wb, "Summary", dims = .d1(3, j),
      name = "Poppins", size = 8, bold = TRUE, color = .xl_col(fg))
    wb <- openxlsx2::wb_add_cell_style(wb, "Summary", dims = .d1(3, j),
      horizontal = "center", vertical = "bottom", wrap_text = TRUE)
  }
  wb <- openxlsx2::wb_set_row_heights(wb, "Summary", rows = 3, heights = 30)

  # Freeze rows 1-3
  wb <- openxlsx2::wb_freeze_pane(wb, "Summary",
    first_active_row = 4, first_active_col = 3)

  # Data rows — values pulled via XLOOKUP from All Data (GEOID anchor in col D)
  current_row  <- 4L
  county_names <- levels(droplevels(report_data$county))

  for (county_name in county_names) {
    cty_data <- dplyr::filter(report_data, county == county_name) |>
      dplyr::arrange(GEOID)

    # County separator row
    wb <- .mff(wb, "Summary", current_row, 1:14,
      value = paste0(county_name, " County  —  ", nrow(cty_data), " Eligible Tracts"),
      fill = .XL_TEAL_MID, font_size = 9, bold = TRUE)
    wb <- openxlsx2::wb_set_row_heights(wb, "Summary", rows = current_row, heights = 18)
    current_row <- current_row + 1L
    data_start  <- current_row

    for (i in seq_len(nrow(cty_data))) {
      tract     <- cty_data[i, ]
      row_r     <- current_row
      g         <- function(col) .xl_cv(tract, col)
      geoid     <- as.character(g("GEOID"))
      geoid_ref <- paste0("$D", row_r)   # GEOID lives in col D

      row_fill <- if (i %% 2 == 0) .XL_WHITE else .XL_HAIR
      key_fill <- if (i %% 2 == 0) .XL_WHITE else .XL_KEY_TINT

      # Cols 1-4: static identity
      wb <- .wc(wb, "Summary", row_r, 1L, sprintf("%02d", i))
      wb <- .wc(wb, "Summary", row_r, 2L, as.character(g("county")))
      wb <- .wc(wb, "Summary", row_r, 3L, .xl_tract_label(geoid))
      wb <- .wc(wb, "Summary", row_r, 4L, geoid)
      # Force text storage — GEOIDs look numeric and Excel auto-converts them
      wb <- openxlsx2::wb_add_numfmt(wb, "Summary",
        dims = .d1(row_r, 4L), numfmt = "@")

      # Cols 5-13: XLOOKUP numeric formulas
      num_cols <- list(
        list(j = 5L,  m = "pop",                    f = "#,##0"),
        list(j = 6L,  m = "hh",                     f = "#,##0"),
        list(j = 7L,  m = "total_acres",             f = "0.0"),
        list(j = 8L,  m = "dev_acres",               f = "0.0"),
        list(j = 9L,  m = "oz1_pct",                 f = '0.0"%"'),
        list(j = 10L, m = "wc_muc_pct",              f = '0.0"%"'),
        list(j = 11L, m = "ato_composite",            f = "0.0"),
        list(j = 12L, m = "sap_planned_hh_per_acre", f = "0.0"),
        list(j = 13L, m = "eru_per_acre",             f = "0.00")
      )
      for (nc in num_cols) {
        wb <- openxlsx2::wb_add_formula(wb, "Summary",
          dims = .d1(row_r, nc$j), x = .xlup(geoid_ref, nc$m))
        wb <- openxlsx2::wb_add_numfmt(wb, "Summary",
          dims = .d1(row_r, nc$j), numfmt = nc$f)
      }

      # Col 14: INDEX/MATCH formula for text; static fill from R data (no CF)
      oz_cls <- if (is.na(g("oztoolclassification"))) NA_character_
                else as.character(g("oztoolclassification"))
      wb <- openxlsx2::wb_add_formula(wb, "Summary",
        dims = .d1(row_r, 14L), x = .xlup(geoid_ref, "oz_short"))

      # Fill
      wb <- openxlsx2::wb_add_fill(wb, "Summary",
        dims = .dr(row_r, 1:9),  color = .xl_col(row_fill))
      wb <- openxlsx2::wb_add_fill(wb, "Summary",
        dims = .dr(row_r, 10:13), color = .xl_col(key_fill))
      wb <- openxlsx2::wb_add_fill(wb, "Summary",
        dims = .d1(row_r, 14L), color = .xl_col(.xl_oz_color(oz_cls)))

      # Font
      wb <- openxlsx2::wb_add_font(wb, "Summary", dims = .dr(row_r, 1:13),
        name = "Poppins", size = 9, color = .xl_col(.XL_INK))
      wb <- openxlsx2::wb_add_font(wb, "Summary", dims = .d1(row_r, 14L),
        name = "Poppins", size = 8, bold = TRUE, color = .xl_col(.XL_WHITE))

      # Alignment
      wb <- openxlsx2::wb_add_cell_style(wb, "Summary", dims = .dr(row_r, 1:4),
        horizontal = "left", vertical = "center")
      wb <- openxlsx2::wb_add_cell_style(wb, "Summary", dims = .dr(row_r, 5:13),
        horizontal = "right", vertical = "center")
      wb <- openxlsx2::wb_add_cell_style(wb, "Summary", dims = .d1(row_r, 14L),
        horizontal = "center", vertical = "center")
      wb <- openxlsx2::wb_set_row_heights(wb, "Summary", rows = row_r, heights = 16)

      current_row <- current_row + 1L
    }

    # Data bars on the four key measure columns
    for (col_j in 10:13) {
      tryCatch(
        openxlsx2::wb_add_conditional_formatting(
          wb, "Summary",
          dims = .dr(data_start:(current_row - 1L), col_j),
          type = "dataBar"
        ),
        error = function(e) invisible(NULL)
      )
    }
  }

  wb
}

# ── Per-tract block (25 rows) ─────────────────────────────────────────────────

.write_tract_block <- function(wb, sheet, tract, tract_num, r_start, map_paths) {
  g       <- function(col) .xl_cv(tract, col)
  r       <- function(offset) r_start + offset
  geoid   <- as.character(g("GEOID"))
  oz_cls  <- if (is.na(g("oztoolclassification"))) NA_character_
              else as.character(g("oztoolclassification"))
  oz_fill <- .xl_oz_color(oz_cls)
  # GEOID embedded directly in INDEX/MATCH formulas — no anchor cell needed
  h_ref   <- paste0('"', geoid, '"')

  # ── r+0: Tract header — merged A:J, oz-color fill ─────────────────────────
  wb <- .mff(wb, sheet, r(0), 1:12, fill = oz_fill, font_size = 10, bold = TRUE)
  wb <- openxlsx2::wb_add_formula(wb, sheet, dims = .d1(r(0), 1L),
    x = paste0(
      '="', sprintf("%02d", tract_num), '  —  "',
      '&', .xlmatch(h_ref, "tract_label"),
      '&"  |  GEOID "&', h_ref,
      '&"  |  "&TEXT(', .xlmatch(h_ref, "total_acres", "0"), ',"0")',
      '&" ac"'
    ))
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(0), heights = 22)

  # ── r+1: spacer ───────────────────────────────────────────────────────────
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(1), heights = 6)

  # ── r+2: Four panel section headers ───────────────────────────────────────
  for (cfg in list(
    list(cols = 2:3,   label = "TRACT PROFILE"),
    list(cols = 5:6,   label = "CENTERS & DISTRICTS"),
    list(cols = 8:9,   label = "STATION AREA PLANNING (SAP)"),
    list(cols = 11:12, label = "WORKPLACE ACCESS TO OPPORTUNITIES (ATO)")
  )) {
    wb <- .mff(wb, sheet, r(2), cfg$cols,
      value = cfg$label, fill = .XL_KEY_TINT, font_size = 7, bold = TRUE,
      font_color = .XL_AMBER, h_align = "left")
  }
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(2), heights = 14)

  # Merge col A from TRACT PROFILE header down to MF/SFA Acres — map image sits on top
  wb <- openxlsx2::wb_merge_cells(wb, sheet, dims = .dr(r(2):r(16), 1L))
  wb <- openxlsx2::wb_add_border(wb, sheet, dims = .dr(r(2):r(16), 1L),
    left_border = "", top_border = "", bottom_border = "",
    right_border = "thin", right_color = .xl_col(.XL_LINE))

  # ── KV row helper ─────────────────────────────────────────────────────────
  # Each panel: list(lc, vc, label, [metric | formula | val], [numfmt],
  #                  [vsize], [vcolor], [vfill], [lfill])
  # lfill: background for the entire lc:vc range (default = default_fill arg)
  # vfill: override fill for the value cell only (applied after lfill)
  .kv <- function(row_offset, panels, default_fill = .XL_PANEL) {
    row_r <- r(row_offset)
    for (p in panels) {
      if (is.null(p)) next
      bg <- if (!is.null(p$lfill)) p$lfill else default_fill
      wb <<- openxlsx2::wb_add_fill(wb, sheet,
        dims = .dr(row_r, p$lc:p$vc), color = .xl_col(bg))
      wb <<- .wc(wb, sheet, row_r, p$lc, p$label)
      lc_color <- if (!is.null(p$lcolor)) p$lcolor else .XL_BODY
      lc_size  <- if (!is.null(p$lsize))  p$lsize  else 8L
      lc_bold  <- if (!is.null(p$lbold))  p$lbold  else FALSE
      wb <<- openxlsx2::wb_add_font(wb, sheet, dims = .d1(row_r, p$lc),
        name = "Poppins", size = lc_size, bold = lc_bold, color = .xl_col(lc_color))
      wb <<- openxlsx2::wb_add_cell_style(wb, sheet, dims = .d1(row_r, p$lc),
        horizontal = "left", vertical = "center")
      if (!is.null(p$vfill)) {
        wb <<- openxlsx2::wb_add_fill(wb, sheet,
          dims = .d1(row_r, p$vc), color = .xl_col(p$vfill))
      }
      if (!is.null(p$metric)) {
        wb <<- openxlsx2::wb_add_formula(wb, sheet,
          dims = .d1(row_r, p$vc), x = .xlup(h_ref, p$metric))
        fmt <- .AD_FMT[[p$metric]]
        if (!is.na(fmt) && fmt != "@") {
          wb <<- openxlsx2::wb_add_numfmt(wb, sheet,
            dims = .d1(row_r, p$vc), numfmt = fmt)
        }
      } else if (!is.null(p$formula)) {
        wb <<- openxlsx2::wb_add_formula(wb, sheet,
          dims = .d1(row_r, p$vc), x = p$formula)
        if (!is.null(p$numfmt)) {
          wb <<- openxlsx2::wb_add_numfmt(wb, sheet,
            dims = .d1(row_r, p$vc), numfmt = p$numfmt)
        }
      } else if (!is.null(p$val)) {
        wb <<- .wc(wb, sheet, row_r, p$vc, p$val)
      }
      vc_color <- if (!is.null(p$vcolor)) p$vcolor else .XL_INK
      vc_size  <- if (!is.null(p$vsize))  p$vsize  else 9L
      wb <<- openxlsx2::wb_add_font(wb, sheet, dims = .d1(row_r, p$vc),
        name = "Poppins", size = vc_size, bold = TRUE, color = .xl_col(vc_color))
      wb <<- openxlsx2::wb_add_cell_style(wb, sheet, dims = .d1(row_r, p$vc),
        horizontal = "right", vertical = "center")
    }
    wb <<- openxlsx2::wb_set_row_heights(wb, sheet, rows = row_r, heights = 15)
  }

  # Alternating fill for data rows (offsets 3-17)
  .af <- function(off) if (((off - 3L) %% 2L) == 0L) .XL_PANEL else .XL_WHITE

  # ── r+3: Cities | WC Center Overlap % | Transit Stations | COMPOSITE SCORE
  # Value cells for WC Overlap %, Transit Stations, and Composite score get light-yellow fill.
  # COMPOSITE SCORE label is styled as a section sub-header (KEY_TINT, blue, bold 7pt).
  .kv(3, list(
    list(lc = 2L, vc = 3L,  label = "Cities",                           metric = "cities"),
    list(lc = 5L, vc = 6L,  label = "WC Center Overlap %",               metric = "wc_muc_pct",
         vfill = .XL_YELLOW_TINT),
    list(lc = 8L, vc = 9L,  label = "Transit Stations (Fixed Guideway)", metric = "transit_stop_count",
         vfill = .XL_YELLOW_TINT),
    list(lc = 11L, vc = 12L, label = "Composite ATO Score",
         metric = "ato_composite", vfill = .XL_YELLOW_TINT)
  ), default_fill = .XL_WHITE)
  # Cities is text — left-align (override the .kv default of right-align)
  wb <- openxlsx2::wb_add_cell_style(wb, sheet, dims = .d1(r(3), 3L),
    horizontal = "left", vertical = "center")

  # ── r+4: Population | Metropolitan Center | Station Area Acres | ATO % of County Avg
  .kv(4, list(
    list(lc = 2L, vc = 3L,  label = "Population",          metric = "pop"),
    list(lc = 5L, vc = 6L,  label = "Metropolitan Center", metric = "wc_pct_metropolitan_center"),
    list(lc = 8L, vc = 9L,  label = "Station Area Acres",  metric = "sap_acres"),
    list(lc = 11L, vc = 12L, label = "ATO % of County Avg", metric = "ato_pct_county_avg",
         vfill = .XL_YELLOW_TINT)
  ), default_fill = .XL_WHITE)

  # ── r+5: Households | Urban Center | Station Area % | AUTO ACCESS (sub-hdr)
  .kv(5, list(
    list(lc = 2L, vc = 3L,  label = "Households",              metric = "hh"),
    list(lc = 5L, vc = 6L,  label = "Urban Center",            metric = "wc_pct_urban_center"),
    list(lc = 8L, vc = 9L,  label = "Station Area % of Tract", metric = "sap_pct")
  ), default_fill = .XL_WHITE)
  wb <- .mff(wb, sheet, r(5), 11:12, value = "AUTO ACCESS",
    fill = .XL_KEY_TINT, font_size = 7, bold = TRUE, font_color = .XL_TEAL, h_align = "left")

  # ── r+6: Total Acres | City Center | SAP Planned HH/ac | Freeway Exits
  .kv(6, list(
    list(lc = 2L, vc = 3L,  label = "Total Acres",                         metric = "total_acres"),
    list(lc = 5L, vc = 6L,  label = "City Center",                         metric = "wc_pct_city_center"),
    list(lc = 8L, vc = 9L,  label = "Additional Planned Housing w/in SAP", metric = "sap_planned_hh_per_acre"),
    list(lc = 11L, vc = 12L, label = "Freeway Exits",                       metric = "freeway_exit_count")
  ), default_fill = .XL_WHITE)

  # ── r+7: Developable Acres | Neighborhood Center | (blank) | ATO Jobs (Auto)
  .kv(7, list(
    list(lc = 2L, vc = 3L,  label = "Developable Acres",      metric = "dev_acres"),
    list(lc = 5L, vc = 6L,  label = "Neighborhood Center",    metric = "wc_pct_neighborhood_center"),
    list(lc = 8L, vc = 9L,  label = "",                       val = ""),
    list(lc = 11L, vc = 12L, label = "ATO Jobs (Auto)",        metric = "ato_jobauto")
  ), default_fill = .XL_WHITE)

  # ── r+8: Poverty Rate | Employment District | PROJECTED GROWTH (sub-hdr) | ATO HH (Auto)
  .kv(8, list(
    list(lc = 2L, vc = 3L,  label = "Poverty Rate",            metric = "pvrty_r"),
    list(lc = 5L, vc = 6L,  label = "Employment District",     metric = "wc_pct_employment_district"),
    list(lc = 11L, vc = 12L, label = "ATO Households (Auto)",  metric = "ato_hhauto")
  ), default_fill = .XL_WHITE)
  wb <- .mff(wb, sheet, r(8), 8:9, value = "PROJECTED TRACT GROWTH 2027-37 (RTP Forecast)",
    fill = .XL_KEY_TINT, font_size = 7, bold = TRUE, font_color = .XL_AMBER, h_align = "left")

  # ── r+9: Unemployment Rate | Industrial District | Households Added | TRANSIT ACCESS (sub-hdr)
  .kv(9, list(
    list(lc = 2L, vc = 3L,  label = "Unemployment Rate",   metric = "unmpl_r"),
    list(lc = 5L, vc = 6L,  label = "Industrial District", metric = "wc_pct_industrial_district"),
    list(lc = 8L, vc = 9L,  label = "Households Added",    metric = "hh_added")
  ), default_fill = .XL_WHITE)
  wb <- .mff(wb, sheet, r(9), 11:12, value = "TRANSIT ACCESS",
    fill = .XL_KEY_TINT, font_size = 7, bold = TRUE, font_color = .XL_TEAL, h_align = "left")

  # ── r+10: Median Family Income | Retail District | Population Added | ATO Jobs (Transit)
  .kv(10, list(
    list(lc = 2L, vc = 3L,  label = "Median Family Income",  metric = "mfi"),
    list(lc = 5L, vc = 6L,  label = "Retail District",       metric = "wc_pct_retail_district"),
    list(lc = 8L, vc = 9L,  label = "Population Added",      metric = "pop_added"),
    list(lc = 11L, vc = 12L, label = "ATO Jobs (Transit)",   metric = "ato_jobtransit")
  ), default_fill = .XL_WHITE)

  # ── r+11: (blank) | (blank) | Jobs Added | ATO HH (Transit)
  .kv(11, list(
    list(lc = 2L, vc = 3L,  label = "", val = ""),
    list(lc = 5L, vc = 6L,  label = "", val = ""),
    list(lc = 8L, vc = 9L,  label = "Jobs Added",              metric = "jobs_added"),
    list(lc = 11L, vc = 12L, label = "ATO Households (Transit)", metric = "ato_hhtransit")
  ), default_fill = .XL_WHITE)

  # ── r+12: OZ 1.0 sub-hdr | Housing sub-hdr | ERU Added (data) | Urban Institute sub-hdr
  wb <- .mff(wb, sheet, r(12), 2:3, value = "OVERLAP W/ OPPORTUNITY ZONES 1.0",
    fill = .XL_KEY_TINT, font_size = 7, bold = TRUE, font_color = .XL_AMBER, h_align = "left")
  wb <- .mff(wb, sheet, r(12), 5:6, value = "2025 HOUSING UNIT INVENTORY",
    fill = .XL_KEY_TINT, font_size = 7, bold = TRUE, font_color = .XL_AMBER, h_align = "left")
  wb <- openxlsx2::wb_add_fill(wb, sheet,
    dims = .dr(r(12), 8:9), color = .xl_col(.XL_WHITE))
  wb <- .wc(wb, sheet, r(12), 8L, "ERU Added")
  wb <- openxlsx2::wb_add_font(wb, sheet, dims = .d1(r(12), 8L),
    name = "Poppins", size = 8, color = .xl_col(.XL_BODY))
  wb <- openxlsx2::wb_add_cell_style(wb, sheet, dims = .d1(r(12), 8L),
    horizontal = "left", vertical = "center")
  wb <- openxlsx2::wb_add_formula(wb, sheet,
    dims = .d1(r(12), 9L), x = .xlup(h_ref, "eru_added"))
  wb <- openxlsx2::wb_add_numfmt(wb, sheet,
    dims = .d1(r(12), 9L), numfmt = .AD_FMT[["eru_added"]])
  wb <- openxlsx2::wb_add_font(wb, sheet, dims = .d1(r(12), 9L),
    name = "Poppins", size = 9, bold = TRUE, color = .xl_col(.XL_INK))
  wb <- openxlsx2::wb_add_cell_style(wb, sheet, dims = .d1(r(12), 9L),
    horizontal = "right", vertical = "center")
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(12), heights = 13)

  # ── r+13: OZ 1.0 Acres | Total Units | ERU per Acre | Urban Institute sub-hdr
  .kv(13, list(
    list(lc = 2L, vc = 3L, label = "OZ 1.0 Acres", metric = "oz1_acres"),
    list(lc = 5L, vc = 6L, label = "Total Units",   metric = "units_total"),
    list(lc = 8L, vc = 9L, label = "ERU per Acre",  metric = "eru_per_acre")
  ), default_fill = .XL_WHITE)
  wb <- .mff(wb, sheet, r(13), 11:12, value = "URBAN INSTITUTE ANALYSIS",
    fill = .XL_KEY_TINT, font_size = 7, bold = TRUE, font_color = .XL_AMBER, h_align = "left")

  # ── r+14: OZ 1.0 % | Residential Acres | (blank) | Investment Likelihood
  .kv(14, list(
    list(lc = 2L, vc = 3L,  label = "OZ 1.0 % of Tract",    metric = "oz1_pct"),
    list(lc = 5L, vc = 6L,  label = "Residential Acres",     metric = "res_acres_total"),
    list(lc = 8L, vc = 9L,  label = "", val = ""),
    list(lc = 11L, vc = 12L, label = "Investment Likelihood", metric = "oz_short")
  ), default_fill = .XL_WHITE)

  # ── r+15: (blank) | SFD Acres | (blank) | (blank)
  .kv(15, list(
    list(lc = 2L, vc = 3L,  label = "", val = ""),
    list(lc = 5L, vc = 6L,  label = "SFD Acres",      metric = "res_acres_sfd"),
    list(lc = 8L, vc = 9L,  label = "", val = ""),
    list(lc = 11L, vc = 12L, label = "", val = "")
  ), default_fill = .XL_WHITE)

  # ── r+16: (blank) | MF / SFA Acres | (blank) | (blank)
  .kv(16, list(
    list(lc = 2L, vc = 3L,  label = "", val = ""),
    list(lc = 5L, vc = 6L,  label = "MF / SFA Acres", metric = "res_acres_mf_sfa"),
    list(lc = 8L, vc = 9L,  label = "", val = ""),
    list(lc = 11L, vc = 12L, label = "", val = "")
  ), default_fill = .XL_WHITE)

  # ── r+17 to r+22: spacers between tract blocks ────────────────────────────
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(17):r(22), heights = 8)

  # ── Map image anchored at col A, tract header row ─────────────────────────
  if (!is.null(map_paths) && geoid %in% names(map_paths) &&
      !is.na(map_paths[[geoid]]) && file.exists(map_paths[[geoid]])) {
    wb <- openxlsx2::wb_add_image(
      wb, sheet = sheet,
      dims  = paste0("A", r(2)),
      file  = map_paths[[geoid]],
      width = 2.27, height = 3.18, units = "in"
    )
  }

  wb
}

# ── Per-county detail sheet ───────────────────────────────────────────────────

.write_county_sheet <- function(wb, county_data, county_name, map_paths) {
  sheet_name <- paste0(county_name, " County")
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet_name,
    tab_color = .xl_col(.XL_TEAL))
  # A=map(28) | B-C=Tract Profile(22+22) | D=buf(3) | E-F=Centers(22+12) |
  # G=buf(3) | H-I=SAP/Growth(26+12) | J=buf(3) | K-L=ATO(24+20)
  wb <- openxlsx2::wb_set_col_widths(wb, sheet_name, cols = 1:12,
    widths = c(28, 22, 22, 3, 22, 12, 3, 26, 12, 3, 24, 20))

  wb <- .mff(wb, sheet_name, 1, 1:12,
    value = paste0(county_name, " County — OZ 2.0 Candidate Tracts — Detail"),
    fill = .XL_TEAL, font_size = 14, bold = TRUE)
  wb <- openxlsx2::wb_set_row_heights(wb, sheet_name, rows = 1, heights = 28)

  wb <- .mff(wb, sheet_name, 2, 1:12,
    value = paste0("WFRC & MAG Region  |  Generated ",
                   format(Sys.Date(), "%B %d, %Y")),
    fill = .XL_TEAL_MID, font_size = 9, font_color = .XL_SKY)
  wb <- openxlsx2::wb_set_row_heights(wb, sheet_name, rows = 2, heights = 18)

  wb <- openxlsx2::wb_set_row_heights(wb, sheet_name, rows = 3, heights = 8)

  # Tract blocks: 25 rows each, starting at row 4
  current_row <- 4L
  for (i in seq_len(nrow(county_data))) {
    wb <- .write_tract_block(wb, sheet_name, county_data[i, ], i,
                             current_row, map_paths)
    current_row <- current_row + 23L
  }

  wb
}

# ── Main export function ──────────────────────────────────────────────────────

#' Export all OZ 2.0 tract data to a styled Excel workbook.
#'
#' @param report_data  Master data frame (one row per tract) from index.qmd.
#' @param tract_maps   Named list of ggplot objects keyed by GEOID.
#' @param settings     Named list from config/settings.yml.
#' @param map_cache_dir  Path to map PNG cache directory (data/map_cache).
#' @param output_path    Overrides default output/OZ_Tracts_Export.xlsx.
export_excel <- function(report_data, tract_maps, settings,
                         map_cache_dir = "data/map_cache",
                         output_path = NULL) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("openxlsx2 is required. Run: install.packages('openxlsx2')", call. = FALSE)
  }
  if (is.null(output_path)) {
    output_path <- file.path(settings$output_dir, "OZ_Tracts_Export.xlsx")
  }
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  message("Saving tract map PNGs for Excel...")
  map_paths <- .save_map_pngs_xl(tract_maps, report_data$GEOID, map_cache_dir)

  county_names <- levels(droplevels(report_data$county))

  message("Building Excel workbook...")
  wb <- openxlsx2::wb_workbook(
    creator = "WFRC / MAG",
    title   = "OZ 2.0 Candidate Tracts — Wasatch Front Region"
  )
  wb <- openxlsx2::wb_set_base_font(wb,
    font_name  = "Poppins",
    font_size  = 9,
    font_color = .xl_col(.XL_INK)
  )

  message("  Writing README sheet...")
  wb <- .write_readme_sheet(wb, county_names)

  message("  Writing Summary sheet...")
  wb <- .write_summary_sheet(wb, report_data)

  for (county_name in county_names) {
    message("  Writing ", county_name, " County sheet...")
    cty_data <- dplyr::filter(report_data, county == county_name) |>
      dplyr::arrange(GEOID)
    wb <- .write_county_sheet(wb, cty_data, county_name, map_paths)
  }

  message("  Writing All Data sheet...")
  wb <- .write_alldata_sheet(wb, report_data)

  openxlsx2::wb_save(wb, file = output_path, overwrite = TRUE)
  message("Excel export written to: ", output_path)
  invisible(output_path)
}

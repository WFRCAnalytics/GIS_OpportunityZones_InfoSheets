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
      !grepl("More|Less|Un", cls, ignore.case = TRUE)) return("Likely to Attract Investment")
  if (grepl("More likely",       cls, ignore.case = TRUE)) return("More Likely")
  if (grepl("Less likely",       cls, ignore.case = TRUE)) return("Less Likely")
  if (grepl("Unlikely|not likely", cls, ignore.case = TRUE)) return("Unlikely / Not Likely")
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

# Column positions in the All Data sheet (one row per tract, 42 cols)
.AD <- c(
  GEOID                      =  1L, county                     =  2L,
  tract_label                =  3L, pop                        =  4L,
  hh                         =  5L, total_acres                =  6L,
  dev_acres                  =  7L, oz1_acres                  =  8L,
  oz1_pct                    =  9L, wc_muc_pct                 = 10L,
  wc_pct_metropolitan_center = 11L, wc_pct_urban_center        = 12L,
  wc_pct_city_center         = 13L, wc_pct_neighborhood_center = 14L,
  wc_pct_employment_district = 15L, wc_pct_industrial_district = 16L,
  wc_pct_retail_district     = 17L, units_total                = 18L,
  res_acres_total            = 19L, res_acres_sfd              = 20L,
  res_acres_mf_sfa           = 21L, transit_stop_count         = 22L,
  sap_acres                  = 23L, sap_pct                    = 24L,
  ato_jobtransit             = 25L, ato_hhtransit              = 26L,
  sap_planned_hh_per_acre    = 27L, freeway_exit_count         = 28L,
  exit_dist_edge_mi          = 29L, ato_jobauto                = 30L,
  ato_hhauto                 = 31L, ato_composite              = 32L,
  hh_added                   = 33L, pop_added                  = 34L,
  jobs_added                 = 35L, eru_added                  = 36L,
  eru_per_acre               = 37L, pvrty_r                    = 38L,
  unmpl_r                    = 39L, mfi                        = 40L,
  oztoolclassification       = 41L, oz_short                   = 42L
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
  oztoolclassification       = "@",         oz_short                   = "@"
)

# Build an INDEX/MATCH formula: looks up geoid_ref in All Data col A,
# returns the column for `metric`. Uses INDEX/MATCH (all Excel versions).
# The &"" coerces the lookup value to text, preventing type-mismatch failures
# when Excel silently converts 11-digit GEOIDs from text to numbers.
.xlup <- function(geoid_ref, metric, ad_sheet = "'All Data'") {
  if (!metric %in% names(.AD)) return('""')
  cl <- .col_letter(.AD[[metric]])
  paste0('=IFERROR(INDEX(', ad_sheet, '!$', cl, ':$', cl,
         ',MATCH(', geoid_ref, '&"",', ad_sheet, '!$A:$A,0)),"-")')
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
                               width = 2.5, height = 3.5, dpi = 300) {
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

  n_cols <- 42L

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

  wb <- openxlsx2::wb_set_col_widths(wb, "All Data", cols = 1:n_cols,
    widths = c(14, 12, 12, rep(10, 39)))

  # Number formats on data cells (apply @ to text cols so Excel keeps them as text)
  data_rows <- 3L:(n_tracts + 2L)
  for (metric in names(.AD)) {
    fmt <- .AD_FMT[[metric]]
    if (!is.na(fmt)) {
      wb <- openxlsx2::wb_add_numfmt(wb, "All Data",
        dims = .dr(data_rows, .AD[[metric]]), numfmt = fmt)
    }
  }

  # Derived columns: overwrite values from wb_add_data_table with Excel formulas
  # so the sheet self-updates if raw inputs are edited.
  .cl <- function(m) .col_letter(.AD[[m]])
  for (row_i in seq_len(n_tracts)) {
    r <- row_i + 2L

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

    # SAP %  =  SAP Acres / Total Acres × 100
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["sap_pct"]]),
      x = paste0("=IFERROR(", .cl("sap_acres"), r,
                 "/", .cl("total_acres"), r, "*100,0)"))

    # ERU Added  =  HH Added + Jobs Added × 0.4
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["eru_added"]]),
      x = paste0("=", .cl("hh_added"), r, "+", .cl("jobs_added"), r, "*0.4"))

    # ERU/ac  =  ERU Added / Dev Acres
    wb <- openxlsx2::wb_add_formula(wb, "All Data",
      dims = .d1(r, .AD[["eru_per_acre"]]),
      x = paste0("=IFERROR(", .cl("eru_added"), r,
                 "/", .cl("dev_acres"), r, ",0)"))

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
    "This workbook is a machine-generated backup of the WFRC/MAG Opportunity Zone 2.0 ",
    "candidate tract analysis. It contains all metrics calculated in the R/GIS pipeline ",
    "for every census tract on the federal OZ 2.0 eligible list within the nine-county ",
    "Wasatch Front region. The Summary sheet mirrors the one-page PDF summary table. ",
    "Per-county sheets mirror the detail PDF pages and include an embedded tract map. ",
    "All values update automatically when the pipeline is re-run with new data."
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
    "This sheet: methodology, column glossary, and abbreviation key.",
    "All-tract summary table: one row per tract, 14 columns matching the PDF summary. Values cross-referenced from All Data.",
    paste0(county_names, " County: per-tract detail blocks with embedded map images. Values cross-referenced from All Data."),
    "Flat database: all 42 metrics for every tract in one filterable Excel Table. Derived columns use in-sheet formulas."
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
  g      <- function(col) .xl_cv(tract, col)
  r      <- function(offset) r_start + offset
  geoid  <- as.character(g("GEOID"))
  oz_cls <- if (is.na(g("oztoolclassification"))) NA_character_
             else as.character(g("oztoolclassification"))
  oz_fill <- .xl_oz_color(oz_cls)
  h_ref   <- paste0("$H$", r_start)   # hidden GEOID anchor for INDEX/MATCH formulas

  # Col H (hidden): write GEOID as the INDEX/MATCH key for all formula cells
  wb <- .wc(wb, sheet, r_start, 8L, geoid)
  # Force text storage — GEOIDs look numeric and Excel auto-converts them
  wb <- openxlsx2::wb_add_numfmt(wb, sheet, dims = .d1(r_start, 8L), numfmt = "@")

  # Row +0: Tract header — merged A:G, oz-color fill; text built from CONCAT+INDEX/MATCH
  wb <- .mff(wb, sheet, r(0), 1:7, fill = oz_fill, font_size = 10, bold = TRUE)
  wb <- openxlsx2::wb_add_formula(wb, sheet, dims = .d1(r(0), 1L),
    x = paste0(
      '="', sprintf("%02d", tract_num), "  —  \"",
      '&', .xlmatch(h_ref, "tract_label"),
      '&"  |  GEOID "&', h_ref,
      '&"  |  "&TEXT(', .xlmatch(h_ref, "total_acres", "0"), ',"0")',
      '&" ac  |  "&',
      .xlmatch(h_ref, "oz_short", '"—"')
    ))
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(0), heights = 22)

  # Section sub-header helper
  sec_hdr <- function(row_offset, labels, fg = .XL_AMBER, bg = .XL_KEY_TINT) {
    for (j in seq_along(labels)) {
      col_start <- (j - 1L) * 2L + 2L
      wb <<- .mff(wb, sheet, r(row_offset), col_start:(col_start + 1L),
        value = labels[[j]], fill = bg, font_size = 7, bold = TRUE,
        font_color = fg, h_align = "left")
    }
    wb <<- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(row_offset), heights = 14)
  }

  # Row +1: Section 1 sub-headers
  sec_hdr(1, list("TRACT PROFILE", "CENTERS & DISTRICTS", "HOUSING & DEMOGRAPHICS"))

  # KV block helper — each KV is list(label=, metric=); metric=NULL → blank value
  kv_rows <- function(row_offset_start, s1, s2, s3, n_rows) {
    for (off in seq_len(n_rows) - 1L) {
      row_r  <- r(row_offset_start + off)
      fill_c <- if (off %% 2 == 0) .XL_PANEL else .XL_WHITE
      grps   <- list(
        list(lc = 2L, vc = 3L, kv = s1[[off + 1L]]),
        list(lc = 4L, vc = 5L, kv = s2[[off + 1L]]),
        list(lc = 6L, vc = 7L, kv = s3[[off + 1L]])
      )
      for (grp in grps) {
        wb <<- .wc(wb, sheet, row_r, grp$lc, grp$kv$label)
        wb <<- openxlsx2::wb_add_fill(wb, sheet,
          dims = .dr(row_r, grp$lc:grp$vc), color = .xl_col(fill_c))
        wb <<- openxlsx2::wb_add_font(wb, sheet, dims = .d1(row_r, grp$lc),
          name = "Poppins", size = 8, color = .xl_col(.XL_BODY))
        wb <<- openxlsx2::wb_add_cell_style(wb, sheet, dims = .d1(row_r, grp$lc),
          horizontal = "left", vertical = "center")
        if (!is.null(grp$kv$metric)) {
          wb <<- openxlsx2::wb_add_formula(wb, sheet,
            dims = .d1(row_r, grp$vc), x = .xlup(h_ref, grp$kv$metric))
          fmt <- .AD_FMT[[grp$kv$metric]]
          if (!is.na(fmt) && fmt != "@") {
            wb <<- openxlsx2::wb_add_numfmt(wb, sheet,
              dims = .d1(row_r, grp$vc), numfmt = fmt)
          }
        } else {
          wb <<- .wc(wb, sheet, row_r, grp$vc, "")
        }
        wb <<- openxlsx2::wb_add_font(wb, sheet, dims = .d1(row_r, grp$vc),
          name = "Poppins", size = 9, bold = TRUE, color = .xl_col(.XL_INK))
        wb <<- openxlsx2::wb_add_cell_style(wb, sheet, dims = .d1(row_r, grp$vc),
          horizontal = "right", vertical = "center")
      }
    }
    wb <<- openxlsx2::wb_set_row_heights(wb, sheet,
      rows = r(row_offset_start):r(row_offset_start + n_rows - 1L), heights = 15)
  }

  # Rows +2 to +9: Section 1 KV (8 rows)
  blank <- list(label = "", metric = NULL)
  profile_kvs <- list(
    list(label = "Population",           metric = "pop"),
    list(label = "Households",           metric = "hh"),
    list(label = "Total Acres",          metric = "total_acres"),
    list(label = "Developable Acres",    metric = "dev_acres"),
    list(label = "OZ 1.0 Acres",         metric = "oz1_acres"),
    list(label = "OZ 1.0 % of Tract",    metric = "oz1_pct"),
    blank, blank
  )
  centers_kvs <- list(
    list(label = "WC MUorC % Total",    metric = "wc_muc_pct"),
    list(label = "Metropolitan Center", metric = "wc_pct_metropolitan_center"),
    list(label = "Urban Center",        metric = "wc_pct_urban_center"),
    list(label = "City Center",         metric = "wc_pct_city_center"),
    list(label = "Neighborhood Center", metric = "wc_pct_neighborhood_center"),
    list(label = "Employment District", metric = "wc_pct_employment_district"),
    list(label = "Industrial District", metric = "wc_pct_industrial_district"),
    list(label = "Retail District",     metric = "wc_pct_retail_district")
  )
  housing_kvs <- list(
    list(label = "Total Units",          metric = "units_total"),
    list(label = "Residential Acres",    metric = "res_acres_total"),
    list(label = "SFD Acres",            metric = "res_acres_sfd"),
    list(label = "MF / SFA Acres",       metric = "res_acres_mf_sfa"),
    list(label = "Poverty Rate",         metric = "pvrty_r"),
    list(label = "Unemployment Rate",    metric = "unmpl_r"),
    list(label = "Median Family Income", metric = "mfi"),
    blank
  )
  kv_rows(2, profile_kvs, centers_kvs, housing_kvs, 8)

  # Row +10: Section 2 sub-headers
  sec_hdr(10, list("TRANSIT ACCESS", "AUTO ACCESS", "PROJECTED GROWTH 2027-37"),
    fg = .XL_TEAL_MID)

  # Rows +11 to +17: Section 2 KV (7 rows)
  transit_kvs <- list(
    list(label = "Rail / BRT Stops",          metric = "transit_stop_count"),
    list(label = "SAP Acres",                 metric = "sap_acres"),
    list(label = "SAP % of Tract",            metric = "sap_pct"),
    list(label = "ATO Jobs (Transit)",        metric = "ato_jobtransit"),
    list(label = "ATO Households (Transit)",  metric = "ato_hhtransit"),
    list(label = "SAP Planned HH/ac",         metric = "sap_planned_hh_per_acre"),
    blank
  )
  auto_kvs <- list(
    list(label = "Freeway Exits",             metric = "freeway_exit_count"),
    list(label = "Exit Distance (mi)",        metric = "exit_dist_edge_mi"),
    list(label = "ATO Jobs (Auto)",           metric = "ato_jobauto"),
    list(label = "ATO Households (Auto)",     metric = "ato_hhauto"),
    blank, blank, blank
  )
  growth_kvs <- list(
    list(label = "Households Added",          metric = "hh_added"),
    list(label = "Population Added",          metric = "pop_added"),
    list(label = "Jobs Added",                metric = "jobs_added"),
    list(label = "ERU Added",                 metric = "eru_added"),
    list(label = "ERU per Acre",              metric = "eru_per_acre"),
    blank, blank
  )
  kv_rows(11, transit_kvs, auto_kvs, growth_kvs, 7)

  # Row +18: Socioeconomic sub-headers
  sec_hdr(18, list("INCOME", "POVERTY RATE", "UNEMPLOYMENT"), fg = .XL_TEAL_SOFT)

  # Row +19: Socioeconomic values (XLOOKUP formulas)
  for (ss in list(list(vc = 3L, m = "mfi"), list(vc = 5L, m = "pvrty_r"),
                  list(vc = 7L, m = "unmpl_r"))) {
    wb <- openxlsx2::wb_add_formula(wb, sheet,
      dims = .d1(r(19), ss$vc), x = .xlup(h_ref, ss$m))
    wb <- openxlsx2::wb_add_numfmt(wb, sheet,
      dims = .d1(r(19), ss$vc), numfmt = .AD_FMT[[ss$m]])
    wb <- openxlsx2::wb_add_fill(wb, sheet,
      dims = .dr(r(19), (ss$vc - 1L):ss$vc), color = .xl_col(.XL_PANEL))
    wb <- openxlsx2::wb_add_font(wb, sheet, dims = .d1(r(19), ss$vc),
      name = "Poppins", size = 11, bold = TRUE, color = .xl_col(.XL_TEAL))
    wb <- openxlsx2::wb_add_cell_style(wb, sheet, dims = .d1(r(19), ss$vc),
      horizontal = "right", vertical = "center")
  }
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(19), heights = 18)

  # Row +20: Investment Likelihood — merged B:G, oz-color fill; text is INDEX/MATCH formula
  wb <- .mff(wb, sheet, r(20), 2:7, fill = oz_fill, font_size = 11, bold = TRUE,
    h_align = "center")
  wb <- openxlsx2::wb_add_formula(wb, sheet, dims = .d1(r(20), 2L),
    x = paste0('="Investment Likelihood: "&', .xlmatch(h_ref, "oz_short", '"—"')))
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(20), heights = 24)

  # Row +21: ATO composite (XLOOKUP formula)
  wb <- .mff(wb, sheet, r(21), 2:5,
    value = "Access to Opportunities (ATO) Composite Score /100",
    fill = .XL_HAIR, font_size = 8, font_color = .XL_MUTED)
  wb <- openxlsx2::wb_add_formula(wb, sheet,
    dims = .d1(r(21), 6L), x = .xlup(h_ref, "ato_composite"))
  wb <- openxlsx2::wb_add_numfmt(wb, sheet,
    dims = .d1(r(21), 6L), numfmt = .AD_FMT[["ato_composite"]])
  wb <- openxlsx2::wb_merge_cells(wb, sheet, dims = .dr(r(21), 6:7))
  wb <- openxlsx2::wb_add_fill(wb, sheet, dims = .dr(r(21), 6:7), color = .xl_col(.XL_HAIR))
  wb <- openxlsx2::wb_add_font(wb, sheet, dims = .d1(r(21), 6L),
    name = "Poppins", size = 12, bold = TRUE, color = .xl_col(.XL_TEAL_MID))
  wb <- openxlsx2::wb_add_cell_style(wb, sheet, dims = .d1(r(21), 6L),
    horizontal = "right", vertical = "center")
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(21), heights = 15)

  # Rows +22 to +24: spacers
  wb <- openxlsx2::wb_set_row_heights(wb, sheet, rows = r(22):r(24), heights = 8)

  # Map image anchored at col A, tract header row
  if (!is.null(map_paths) && geoid %in% names(map_paths) &&
      !is.na(map_paths[[geoid]]) && file.exists(map_paths[[geoid]])) {
    wb <- openxlsx2::wb_add_image(
      wb, sheet = sheet,
      dims  = paste0("A", r(0)),
      file  = map_paths[[geoid]],
      width = 2.5, height = 3.5, units = "in"
    )
  }

  wb
}

# ── Per-county detail sheet ───────────────────────────────────────────────────

.write_county_sheet <- function(wb, county_data, county_name, map_paths) {
  sheet_name <- paste0(county_name, " County")
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet_name,
    tab_color = .xl_col(.XL_TEAL))
  wb <- openxlsx2::wb_set_col_widths(wb, sheet_name, cols = 1:7,
    widths = c(36, 22, 13, 22, 13, 22, 13))
  # Col H: hidden GEOID anchor used by XLOOKUP formulas in tract blocks
  wb <- openxlsx2::wb_set_col_widths(wb, sheet_name, cols = 8L,
    widths = 0.1, hidden = TRUE)

  # Sheet title
  wb <- .mff(wb, sheet_name, 1, 1:7,
    value = paste0(county_name, " County — OZ 2.0 Candidate Tracts — Detail"),
    fill = .XL_TEAL, font_size = 14, bold = TRUE)
  wb <- openxlsx2::wb_set_row_heights(wb, sheet_name, rows = 1, heights = 28)

  wb <- .mff(wb, sheet_name, 2, 1:7,
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
    current_row <- current_row + 25L
  }

  wb
}

# ── Main export function ──────────────────────────────────────────────────────

#' Export all OZ 2.0 tract data to a styled Excel workbook.
#'
#' @param report_data  Master data frame (one row per tract) from index.qmd.
#' @param tract_maps   Named list of ggplot objects keyed by GEOID.
#' @param settings     Named list from config/settings.yml.
#' @param remotes_dir  Path to data/remotes — map PNGs cached here.
#' @param output_path  Overrides default output/OZ_Tracts_Export.xlsx.
export_excel <- function(report_data, tract_maps, settings, remotes_dir,
                         output_path = NULL) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("openxlsx2 is required. Run: install.packages('openxlsx2')", call. = FALSE)
  }
  if (is.null(output_path)) {
    output_path <- file.path(settings$output_dir, "OZ_Tracts_Export.xlsx")
  }
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  map_dir <- file.path(remotes_dir, "map_cache")
  message("Saving tract map PNGs for Excel...")
  map_paths <- .save_map_pngs_xl(tract_maps, report_data$GEOID, map_dir)

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

# workbook_content.R — content shared by the Excel workbook (export_excel.R)
# and its PDF companion (_workbook_pdf.qmd), so the two cannot drift apart.
#
#   xl_readme_content() — README text: about, sheet guide, glossary,
#                         methodology notes, data sources
#   xl_values()         — the values the workbook's formulas display, computed
#                         in R with the same rules (the PDF never opens Excel)
#   xl_display()        — a value formatted the way the workbook shows it

# ── README text ───────────────────────────────────────────────────────────────

xl_readme_content <- function(county_names, date = Sys.Date()) {
  list(
    title    = "OZ 2.0 Candidate Tracts — WFRC Analysis",
    subtitle = paste0("WFRC & MAG Region · Wasatch Front, Utah · Generated ",
                      format(date, "%B %d, %Y")),
    about = paste0(
      "This workbook presents the WFRC/MAG Opportunity Zone 2.0 candidate tract analysis ",
      "for the nine-county Wasatch Front region. It covers every census tract on the ",
      "federal OZ 2.0 eligible list, with metrics spanning land use, transit access, ",
      "housing inventory, projected growth, and Access to Opportunities indicators. ",
      "The Summary sheet provides a regional overview of all eligible tracts. ",
      "Per-county sheets offer a detailed profile for each tract, including an embedded ",
      "map and key metrics organized by theme. The All Data sheet contains the complete ",
      "set of metrics in a flat, filterable table for cross-tract comparison and analysis."
    ),
    sheets = data.frame(
      name = c("README", "Summary - 4 Key Measures",
               paste0(county_names, " County - Detailed"), "All Data - Detailed"),
      desc = c(
        "This sheet: about this workbook, abbreviation glossary, and data sources.",
        "Regional overview: one row per tract with key profile and access metrics for all eligible tracts across the Wasatch Front.",
        paste0(county_names, " County: per-tract detail blocks with embedded map and metrics organized by theme."),
        "Complete dataset: all metrics for every tract in one filterable table. Use this sheet for cross-tract comparison and custom analysis."
      ),
      stringsAsFactors = FALSE
    ),
    glossary = data.frame(
      term = c("ATO", "DEVACRES", "ERU", "GEOID", "MF", "OZ 1.0", "OZ 2.0", "SAP",
               "SFA", "SFD", "TAZ", "WC / MUorC"),
      full = c("Access to Opportunities", "Developable Acres", "Equivalent Residential Unit",
               "Census Tract Geographic ID", "Multi-family housing",
               "Opportunity Zones (2018 round)", "Opportunity Zones 2.0 (current)",
               "Station Area Plan", "Single Family Attached", "Single Family Detached",
               "Traffic Analysis Zone", "Wasatch Choice / Metro-Urban-City Center"),
      desc = c(
        "Composite 0-100 index combining driving and transit access to jobs and households.",
        "Developable land within a TAZ from the ATO layer; used as intersection weight for ATO scores.",
        "Growth density metric: 1 household = 1 ERU, 1 job = 0.4 ERU. ERU puts gains in job density on an equal footing with gains in residential housing unit density for comparison purposes.",
        "11-digit FIPS code: 2-digit state + 3-digit county + 6-digit tract.",
        "Apartments, condos, etc.",
        "Prior federal OZ designations; overlap shown for context.",
        "Current federal eligible tract list under evaluation.",
        "Adopted housing plan within 1/2-mile of a rail or BRT stop.",
        "Single family home with some form of shared wall or floor (i.e. includes duplexes, townhomes, and other 'missing middle' style homes).",
        "Traditional single family home, with no walls shared with other housing units (i.e. excludes duplexes, townhomes, and other 'missing middle' style homes).",
        "WFRC/MAG forecast unit; growth projections are area-proportioned from TAZ to tract.",
        "WFRC/MAG regional growth center framework. MUorC = Metropolitan, Urban, or City center types."
      ),
      stringsAsFactors = FALSE
    ),
    methodology = data.frame(
      metric = c("ATO Composite Score", "ATO % of County Avg", "ERU Added / ERU per Acre",
                 "Cities", "SAP & ATO Availability"),
      text = c(
        paste0(
          "Each of the four raw ATO subscores — Jobs (Auto), Households (Auto), Jobs (Transit), ",
          "Households (Transit) — is rescaled 0–100 using min-max normalization across all ",
          "MPO-region tracts in this export. The four normalized values are then averaged to produce ",
          "a single 0–100 composite index (higher = greater access). ",
          "Scores are relative to the tracts included in this export: adding or removing tracts ",
          "would shift individual values."),
        paste0(
          "Tract ATO Composite Score ÷ average ATO Composite Score for all tracts in the same ",
          "county × 100. The county average is computed via AVERAGEIF in the All Data sheet and ",
          "excludes non-MPO tracts (Tooele County), which have no ATO data. ",
          "A value of 100 means the tract matches its county average; values above 100 indicate ",
          "above-average access for that county."),
        paste0(
          "ERU (Equivalent Residential Unit) standardizes growth across land use types. ",
          "ERU Added = Households Added + (Jobs Added × 0.4). ",
          "ERU per Acre = ERU Added ÷ Developable Acres. ",
          "The 0.4 weight reflects that employment uses are less land-intensive than housing. ",
          "Growth figures are sourced from WFRC/MAG TAZ projections (2027–2037), ",
          "area-proportioned to census tract boundaries using developable acres as the weight."),
        paste0(
          "Determined by spatial intersection of the census tract boundary with U.S. Census ",
          "TIGER/Line Place boundaries (incorporated cities, towns, and census-designated places). ",
          "Municipalities covering less than 2% of the tract area are excluded to avoid listing ",
          "artifacts from boundary slivers. Where multiple cities qualify, they are listed in ",
          "descending order of intersection area."),
        paste0(
          "Station Area Plan (SAP) and Access to Opportunities (ATO) metrics are derived from ",
          "WFRC/MAG datasets that cover only the MPO planning boundary. Tooele County tracts ",
          "fall outside this boundary and therefore have no SAP or ATO data. These cells show ",
          "'n/a' in the per-county detail sheets and are left blank in the All Data sheet. ",
          "*Some SAP are showing '0', even though they have stations. This is because they were ",
          "completed before the state law was enacted, and therefore, no housing data was reported. ",
          "This is mostly pertinent to Salt Lake City and Ogden City.")
      ),
      stringsAsFactors = FALSE
    ),
    sources = data.frame(
      text = c(
        "Federal OZ 2.0 Eligibility List — U.S. Treasury / HUD",
        "Urban Institute OZ Tool Classification — Urban Institute (Methodology Explanation)",
        "Wasatch Choice for 2050 Centers & Districts — WFRC/MAG",
        "ACS Population, Households, Income, Poverty, Unemployment — U.S. Census Bureau",
        "UTA Rail/BRT Stops — Utah Transit Authority",
        "Station Area Plans (SAP) — WFRC/MAG",
        "Freeway Interchanges — WFRC/MAG",
        "Housing Inventory — WFRC/MAG",
        "Access to Opportunities (ATO) Scores — WFRC/MAG",
        "TAZ Growth Projections 2027-2037 — WFRC/MAG"
      ),
      url = c(
        NA,
        "https://www.urban.org/projects/informing-2026-opportunity-zone-selections-state-and-local-decisionmakers/opportunity",
        rep(NA, 8)
      ),
      stringsAsFactors = FALSE
    )
  )
}

# ── Workbook values ───────────────────────────────────────────────────────────
# The workbook stores raw inputs in "All Data - Detailed" and derives the rest
# with formulas; the other sheets look values up from there. xl_values()
# reproduces those formulas (see .write_alldata_sheet and .write_summary_sheet
# in export_excel.R) so the PDF shows exactly what Excel would.
#
# Missing values: the workbook writes some NA inputs as empty cells (shown
# "n/a") and leaves the rest as #N/A errors (shown "-"). Formula results that
# are "" also show "n/a". xl_values() returns NA for all of these;
# xl_display() picks "n/a" or "-" from the metric name.

# Inputs export_excel.R blanks (shown "n/a") and formulas that return "" (shown "n/a")
.XL_NA_BLANK <- c("oz1_acres", "sap_acres", "sap_planned_hh_per_acre", "exit_dist_edge_mi",
                  "ato_jobauto", "ato_hhauto", "ato_jobtransit", "ato_hhtransit",
                  "sap_pct", "ato_composite", "ato_pct_county_avg", "eru_per_acre")

xl_values <- function(report_data) {
  d <- report_data
  minmax <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1]) * 100
  }

  d$res_acres_total <- d$res_acres_sfd + d$res_acres_mf_sfa
  d$oz1_pct    <- ifelse(is.na(d$oz1_acres) | d$total_acres == 0, 0,
                         d$oz1_acres / d$total_acres * 100)
  d$wc_muc_pct <- d$wc_pct_metropolitan_center + d$wc_pct_urban_center + d$wc_pct_city_center
  d$sap_pct    <- ifelse(is.na(d$sap_acres), NA_real_, d$sap_acres / d$total_acres * 100)
  d$eru_added  <- d$hh_added + d$jobs_added * 0.4
  d$eru_per_acre <- ifelse(is.na(d$dev_acres) | d$dev_acres == 0, NA_real_,
                           d$eru_added / d$dev_acres)
  d$ato_composite <- ifelse(
    is.na(d$ato_jobauto), NA_real_,
    (minmax(d$ato_jobauto) + minmax(d$ato_hhauto) +
       minmax(d$ato_jobtransit) + minmax(d$ato_hhtransit)) / 4
  )
  cty_avg <- stats::ave(d$ato_composite, d$county,
                        FUN = function(x) mean(x, na.rm = TRUE))
  d$ato_pct_county_avg <- ifelse(is.na(d$ato_composite), NA_real_,
                                 d$ato_composite / cty_avg * 100)
  d$tract_label <- as.character(d$TractName)
  d$oz_short <- vapply(as.character(d$oztoolclassification), .xl_short_class, character(1))

  # Within-county ranks (RANK.EQ, descending; ties share a rank; non-numeric -> NA)
  rank_desc <- function(x) {
    x  <- signif(x, 15)   # Excel compares at 15 significant digits: near-equal values tie
    r  <- rep(NA_integer_, length(x))
    ok <- !is.na(x)
    r[ok] <- vapply(x[ok], function(v) sum(x[ok] > v) + 1L, integer(1))
    r
  }
  by_cty <- function(x) stats::ave(x, d$county, FUN = rank_desc)
  d$rank_muc <- by_cty(d$wc_muc_pct)
  d$rank_ato <- by_cty(d$ato_composite)
  d$rank_sap <- by_cty(d$sap_planned_hh_per_acre)
  d$rank_eru <- by_cty(d$eru_per_acre)
  d
}

# Urban Institute class -> short label (same rule as .xl_oz_short in export_excel.R)
.xl_short_class <- function(cls) {
  if (is.null(cls) || length(cls) == 0 || is.na(cls)) return("—")
  if (grepl("Likely to attract", cls, ignore.case = TRUE) &&
      !grepl("More|Less|Un", cls, ignore.case = TRUE)) return("Likely")
  if (grepl("More likely",         cls, ignore.case = TRUE)) return("More Likely")
  if (grepl("Less likely",         cls, ignore.case = TRUE)) return("Less Likely")
  if (grepl("Unlikely|not likely", cls, ignore.case = TRUE)) return("Unlikely")
  cls
}

# Format a value the way the workbook displays it (its number formats, see
# .AD_FMT in export_excel.R): "#,##0", "0.0", "0.00", '0.0"%"', '"$"#,##0', text.
xl_display <- function(x, metric) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(if (metric %in% .XL_NA_BLANK) "n/a" else "-")
  }
  fmt <- switch(metric,
    pop = , hh = , units_total = , transit_stop_count = , freeway_exit_count = ,
    ato_jobtransit = , ato_hhtransit = , ato_jobauto = , ato_hhauto = ,
    hh_added = , pop_added = , jobs_added = "int",
    oz1_pct = , wc_muc_pct = , wc_pct_metropolitan_center = , wc_pct_urban_center = ,
    wc_pct_city_center = , wc_pct_neighborhood_center = , wc_pct_employment_district = ,
    wc_pct_industrial_district = , wc_pct_retail_district = , sap_pct = ,
    ato_pct_county_avg = "pct1",
    # pvrty_r / unmpl_r arrive as text ("12.61%") and print as-is, like Excel
    eru_per_acre = , exit_dist_edge_mi = "dec2",
    mfi = "usd",
    total_acres = , dev_acres = , oz1_acres = , res_acres_total = , res_acres_sfd = ,
    res_acres_mf_sfa = , sap_acres = , sap_planned_hh_per_acre = , ato_composite = ,
    eru_added = "dec1",
    "text")
  num <- suppressWarnings(as.numeric(x))
  switch(fmt,
    int  = format(round(num), big.mark = ",", scientific = FALSE, trim = TRUE),
    pct1 = paste0(formatC(num, format = "f", digits = 1), "%"),
    dec1 = formatC(num, format = "f", digits = 1),
    dec2 = formatC(num, format = "f", digits = 2),
    usd  = paste0("$", format(round(num), big.mark = ",", scientific = FALSE, trim = TRUE)),
    as.character(x))
}

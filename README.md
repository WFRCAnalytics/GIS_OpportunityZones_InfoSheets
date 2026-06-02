# OZ 2.0 Eligible Tract Info Sheets — WFRC & MAG Region

Generates landscape-oriented PDF info sheets for every census tract on Utah's **Opportunity Zones 2.0** eligible list. Produced by the Wasatch Front Regional Council (WFRC) and Mountainland Association of Governments (MAG) to support OZ 2.0 investment siting decisions.

Each county produces two PDFs:

| PDF | Content |
|---|---|
| **Summary** | One-page table — all tracts in the county, side by side |
| **Detail** | One page per tract — large map, key metrics, 5 data panels |

---

## Prerequisites

- R 4.6.0+
- [Quarto](https://quarto.org/) (tested with 1.6+)
- [Poppins font](https://fonts.google.com/specimen/Poppins) installed system-wide (required for Typst PDF output)
- Access to the GIS data sources listed in `config/data_sources.yml`

---

## Setup

```r
# 1. Restore the R package library (renv manages all dependencies)
renv::restore()

# 2. Verify data source paths in config/data_sources.yml
#    Fields marked FILL_IN need ArcGIS REST service URLs or local paths.
```

---

## Running the Pipeline

The pipeline runs inside `index.qmd` in two stages.

### Stage 1 — Build the data (runs automatically on `quarto render`)

```bash
quarto render index.qmd
```

This executes all chunks except the `report` chunk, which is marked `#| eval: false`. It downloads/caches GIS data, computes per-tract metrics, and generates maps.

### Stage 2 — Render county PDFs (run the `report` chunk interactively)

Open `index.qmd` in RStudio and run the `report` chunk manually. It:
1. Joins all metric tables into `data/remotes/report_data.rds`
2. Saves `data/remotes/tract_maps.rds`
3. Calls `quarto::quarto_render()` for each county → writes PDFs to `output/summary/` and `output/detail/`

> **Windows note:** Close any open PDF viewer windows before running Stage 2. Typst writes to `_county_summary.pdf` and `_county_detail.pdf` as intermediates; Windows file locking will cause an error if a viewer has them open. The pipeline will detect this and stop with a clear message.

### Testing a single county

```bash
# Summary sheet — defaults to Salt Lake County (set in YAML params)
quarto render _county_summary.qmd

# Detail sheet
quarto render _county_detail.qmd
```

---

## Output

```
output/
  summary/
    Box_Elder_OZ_Summary.pdf
    Weber_OZ_Summary.pdf
    ...
  detail/
    Box_Elder_OZ_Detail.pdf
    Weber_OZ_Detail.pdf
    ...
```

County order follows the WFRC region first (Box Elder, Weber, Davis, Salt Lake, Tooele, Morgan) then MAG (Utah, Summit, Wasatch).

---

## Summary Sheet Columns

| Column | Source | Notes |
|---|---|---|
| Map | Computed | Tract-level map: WC centers, SAP buffers, rail/BRT stops, freeway exits |
| Tract GEOID | Census | 11-digit census tract identifier |
| Tract Profile | ACS + GIS | Pop., HH, Total Acres, Dev. Acres |
| OZ 1.0 Acres / % | GIS intersection | Overlap with prior OZ 1.0 designations |
| Wasatch Choice Center Coverage | GIS intersection | % of tract in any WC center/district |
| Access to Opportunities Score | WFRC ATO layer | Composite 0–100 index (driving + transit) |
| Station Area Plan Housing | SAP data | Planned HH/Acre within SAP boundaries |
| Projected Growth 2027–37 (ERU/Acre) | WFRC/MAG TAZ forecast | Area-proportioned from TAZ layer |
| OZ Investment Likelihood | Urban Institute OZ Tool | Classification badge |

## Detail Sheet Panels

| Panel | Metrics |
|---|---|
| **Hero stats** (upper right) | Pop., HH, Total Acres, OZ 1.0%, ATO Score, WC Coverage, SAP HH/Acre, ERU/Acre |
| **Centers** | % of tract covered by each Wasatch Choice center/district type |
| **Housing & Demographics** | Units, residential acres by type; poverty rate, unemployment, median family income |
| **Transit** | Rail/BRT stops, SAP acres/%, ATO transit jobs and HHs |
| **Auto Access** | Freeway exits, ATO auto jobs and HHs |
| **Projected Growth 2027–37** | HHs, pop., jobs, ERU added; ERU/Acre |

---

## Domain Glossary

| Term | Definition |
|---|---|
| **OZ 2.0** | Federal Opportunity Zones 2.0 — current eligible tract designation |
| **OZ 1.0** | Prior Opportunity Zone round (2018) — shown for historical context |
| **Wasatch Choice (WC)** | WFRC/MAG regional growth center framework (Wasatch Choice for 2050) |
| **MUorC** | Metropolitan, Urban, or City center — higher-intensity WC designations |
| **SAP** | Station Area Plan — adopted housing plan within ½-mile of rail or BRT |
| **ATO** | Access to Opportunities — composite index combining driving and transit access to jobs and households |
| **ERU** | Equivalent Residential Unit — 1 HH = 1 ERU, 1 job = 0.4 ERU (standard planning conversion) |
| **TAZ** | Traffic Analysis Zone — WFRC/MAG small-area forecast unit |
| **DEVACRES** | Developable acres in a TAZ; used as area-proportioning weight for ATO metrics |
| **Urban Institute OZ Tool** | Third-party classification of OZ investment likelihood based on market indicators |

---

## Configuration

- **`config/data_sources.yml`** — paths/URLs for all input GIS layers
- **`config/settings.yml`** — map dimensions (3×2in), analysis CRS (EPSG:26912), output directory

All spatial data is cached in **GPKG format** under `data/remotes/` (gitignored).

---

## Key Data Sources

| Dataset | Source |
|---|---|
| SAP Housing & Connections Metrics | [Google Sheet (Byron Head)](https://docs.google.com/spreadsheets/d/1kQR94N1clI0LX5h9Bhi34Y5FHY6bqP09keCvRpbqoWs/) — also saved as `data/raw/SAP Housing and Connections Metrics.xlsx` |

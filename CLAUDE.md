# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

Generates landscape-oriented PDF info sheets for every census tract on Utah's Opportunity Zones 2.0 eligible list. Each county produces two PDFs — a one-page summary table and a per-tract detail page — covering land use, transit access, housing, and projected growth metrics. See `OZ-eligible Tract Report, Draft Specifications.md` for the full column schema.

## Tech Stack

- **R 4.6.0** + **Quarto** + **Typst** — report generation (PDF via Typst, not LaTeX)
- **sf**, **ggplot2**, **dplyr**, **purrr** — spatial analysis and mapping
- **renv** — dependency management (`renv.lock` is the source of truth)

## Commands

Restore the package library after cloning or when `renv.lock` changes:
```r
renv::restore()
```

Run the full data pipeline and render all county PDFs (run the `report` chunk interactively in RStudio — it has `#| eval: false`):
```r
# Inside RStudio with index.qmd open, run the "report" chunk manually.
# It saves report_data.rds + tract_maps.rds, then calls quarto::quarto_render()
# per county to produce output/summary/ and output/detail/ PDFs.
```

Render a single child document for testing (run from the project root):
```bash
quarto render _county_summary.qmd
quarto render _county_detail.qmd
```

Snapshot new packages added during development:
```r
renv::snapshot()
```

## File Structure

```
index.qmd                  — master pipeline: data assembly, maps, report dispatch
_county_summary.qmd        — parameterized summary table (one page per county)
_county_detail.qmd         — parameterized detail pages (one page per tract)
config/
  data_sources.yml         — all input layer paths/URLs
  settings.yml             — map dimensions, CRS, output dir
src/
  download_data.R          — caching layer for remote GIS data
data/remotes/              — gitignored; populated at runtime
  report_data.rds          — joined master table (one row per tract)
  tract_maps.rds           — named list of ggplot objects keyed by GEOID
  map_cache/               — PNG files per tract (GEOID.png)
output/
  summary/                 — <County>_OZ_Summary.pdf per county
  detail/                  — <County>_OZ_Detail.pdf per county
```

## Architecture

The pipeline runs in four stages inside `index.qmd`:

1. **Load** — read spatial layers from GPKG cache via `sf::read_sf()` with SQL `query=` and `wkt_filter=` for server-side filtering.
2. **Calculate** — per-tract metrics: WC center intersections, housing centroid joins, SAP buffer overlaps, ATO weighted averages (by DEVACRES), TAZ growth area-proportioning, composite ATO 0–100 index.
3. **Maps** — `make_tract_map()` generates a ggplot per tract; saved to `tract_maps.rds`. PNG cache in `data/remotes/map_cache/` is shared between summary and detail renders.
4. **Render** — the `report` chunk (marked `#| eval: false`) saves RDS files then calls `quarto::quarto_render()` in a `purrr::walk()` over counties. PDFs land in `output/summary/` and `output/detail/`.

## Typst Layout — Critical Constraints

### Font
**Poppins only.** Inter and Open Sans are variable fonts; Typst 0.14.2 cannot render bold weight for variable fonts. Poppins is a static font family — all weights work.

### Page dimensions
Landscape, 11×8.5in, margins `x: 0.4in y: 0.35in` → **10.2in × 7.8in** usable.

### Map dimensions
Saved at **3×2in** (3:2 aspect), 200 dpi. In the detail sheet the map column is 4.8in wide (height 3.2in at 3:2).

### Parser gotcha — `(` after a function call
In Typst markup mode, `(` immediately after `#function()` is parsed as a function call argument:
```
#linebreak()(Urban Institute)   ← ERROR: expects comma
#linebreak()#[(Urban Institute)] ← OK: content block
```
Wrap `(text)` in `#[(...)]` whenever it follows a Typst function call with no intervening text.

### Intermediate PDF locking on Windows
`quarto::quarto_render()` always compiles to `_county_summary.pdf` / `_county_detail.pdf` first (Typst uses the `.typ` basename), then renames to the `output_file`. If a PDF viewer has those files open, the pipeline fails with OS error 32. The `report` chunk has a pre-flight `file.remove()` check that stops early with a clear message if either file is locked.

### Raw Typst from R
Child documents emit Typst by `cat()`-ing inside `#| results: asis` chunks:
- Use `paste0()` (not `sprintf()`) when building strings that contain `%` — metric values like "45.2%" would be misinterpreted as format specifiers.
- `%%` in `sprintf()` format strings → single `%` in output.
- Panel content with `%` values must be assembled via `paste0()`, not `sprintf()`.

## WFRC Branding & Colors

| Use | Hex |
|---|---|
| Primary dark teal (headers, panel headers) | `#003b4f` |
| Sky blue accent (rules, highlights) | `#7ec8e3` |
| OZ: Likely to attract investment | `#2a7f46` |
| OZ: More likely | `#0072b5` |
| OZ: Less likely | `#e07b39` |
| OZ: Unlikely / not likely | `#c0392b` |
| Progress bar track | `#e8f4f8` |

## Domain Glossary

| Term | Meaning |
|---|---|
| **OZ 2.0** | Federal Opportunity Zones 2.0 — the current eligible tract list |
| **OZ 1.0** | Prior round of OZ designations (2018); overlap shown for context |
| **WC / Wasatch Choice** | Wasatch Choice for 2050 — WFRC/MAG regional growth centers framework |
| **MUorC** | Metropolitan, Urban, or City center type (higher-intensity WC designations) |
| **SAP** | Station Area Plan — adopted housing plan within ½-mile of a rail/BRT stop |
| **ATO** | Access to Opportunities — composite 0–100 index (driving + transit, jobs + HHs) |
| **ERU** | Equivalent Residential Unit — growth measure: 1 HH = 1 ERU, 1 job = 0.4 ERU |
| **TAZ** | Traffic Analysis Zone — WFRC/MAG forecast unit for growth projections |
| **DEVACRES** | Developable acres within a TAZ (from ATO layer); used as intersection weight |

## Data Sources Config

All input layers are declared in `config/data_sources.yml`. Load with:
```r
config <- yaml::read_yaml("config/data_sources.yml")
```

Spatial cache uses **GPKG format** (never RDS for spatial data). Fix data quality issues at read time via `sf::read_sf()` arguments; do not patch mid-pipeline.

Primary sort order for output: County (primary), Tract GEOID (secondary).

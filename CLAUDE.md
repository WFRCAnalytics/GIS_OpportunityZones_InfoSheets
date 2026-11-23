# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

Generates landscape-oriented PDF info sheets for every census tract on Utah's Opportunity Zones 2.0 eligible list. Each county produces two PDFs — a one-page summary table and a per-tract detail page — covering land use, transit access, housing, and projected growth metrics. See `OZ-eligible Tract Report, Draft Specifications.md` for the full column schema.

## Tech Stack

- **R 4.6.0** + **Quarto** + **Typst** — report generation (PDF via Typst, not LaTeX)
- **sf**, **ggplot2**, **dplyr**, **purrr** — spatial analysis and mapping
- **basemapper** (v0.1.0, `ar-puuk/basemapper/r-basemapper`) — UGRC Lite vector basemap via `basemapper::geom_basemap("UGRC.Lite")`, rendered at `ggsave()` time; installing from source needs Rust + the `x86_64-pc-windows-gnu` target + Rtools45
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
_workbook_pdf.qmd          — PDF companion to the Excel workbook (README, Summary,
                             county sheets as Excel prints them; no All Data table)
config/
  data_sources.yml         — all input layer paths/URLs
  settings.yml             — map dimensions, CRS, output dir
src/
  download_data.R          — caching layer for remote GIS data
  export_excel.R           — Excel workbook (openxlsx2, live formulas)
  workbook_content.R       — README text + R versions of the workbook's formulas,
                             shared by export_excel.R and _workbook_pdf.qmd
data/remotes/              — gitignored; populated at runtime
  report_data.rds          — joined master table (one row per tract)
  tract_maps.rds           — named list of ggplot objects keyed by GEOID
  map_cache/               — PNG files per tract (GEOID.png)
output/
  summary/                 — <County>_OZ_Summary.pdf per county
  detail/                  — <County>_OZ_Detail.pdf per county
  Opportunity Zones 2.0 - WF Candidate Tract Analysis.xlsx / .pdf — workbook + its PDF
```

## Architecture

The pipeline runs in four stages inside `index.qmd`:

1. **Load** — read spatial layers from GPKG cache via `sf::read_sf()` with SQL `query=` and `wkt_filter=` for server-side filtering.
2. **Calculate** — per-tract metrics: WC center intersections, housing centroid joins, SAP buffer overlaps, ATO weighted averages (by DEVACRES), TAZ growth area-proportioning, composite ATO 0–100 index.
3. **Maps** — `make_tract_map()` generates a ggplot per tract; saved to `tract_maps.rds`. PNG cache in `data/remotes/map_cache/` is shared between summary and detail renders.
4. **Render** — the `report` chunk (marked `#| eval: false`) saves RDS files then calls `quarto::quarto_render()` in a `purrr::walk()` over counties. PDFs land in `output/summary/` and `output/detail/`.

## Typst Layout — Critical Constraints

### Font
**Poppins** (body) and **IBM Plex Mono** (GEOIDs, glossary, badges), both static families shipped in `assets/fonts/` (SIL OFL, licences alongside) and loaded through `font-paths: ["assets/fonts"]` in each child document's YAML — no system install needed. Inter and Open Sans are variable fonts; Typst 0.14.2 cannot render bold weight for variable fonts. Poppins has no "→" glyph: set arrows in IBM Plex Mono, or Typst falls back to Libertinus Serif. SVGs drawn in R (donut, gauge) must name `Poppins` as their font family, or they embed Arial.

### Design source
Layout sizes come from `handoff/visual-reference/summary_refined.jsx` and `detail_refined.jsx`. Their page is 1056 × 816 px for 11 × 8.5 in, so **1 px = 0.75 pt**: convert the mock's px values directly rather than estimating from the PNG renders.

Typst sizes text boxes to the glyphs, while the browser gives every line its full line height, so the px spacings only carry over once text has browser-like line boxes. The detail sheet sets `#set text(top-edge: 1.1em, bottom-edge: -0.4em)` (Poppins' 1.5em "normal" line height: ascender 1.05 + descender 0.35 + line gap 0.1, text centred); the summary uses `top-edge: "ascender", bottom-edge: "descender"`. Where the mock sets its own `lineHeight` (title 1, key numbers 0.9, card labels 1.1), the text overrides the edges to match. To check fidelity, render a page at 288 dpi (same pixel size as the reference PNGs) and compare landmarks.

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

### Parser gotcha — `/` at the start of content
`[ / 3]` starts a term-list item and fails with "expected colon". Escape it: `[ \/ 3]` (in R source: `'[ \\/ 3]'`).

### Parser gotcha — `#set` runs to end of line
`#set par(leading: 0.3em)#align(...)` fails ("`#` is not valid in code"). End the rule with `;`: `#set par(leading: 0.3em);#align(...)`.

### Page header placement
A `#set page(header: ...)` header sits at the bottom of the top margin, so the top margin = desired top gap + header height. Adding `#v()` above the header pushes it down instead of opening a gap at the top.

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
| **ERU** | Equivalent Residential Unit — growth measure: 1 HH = 1 ERU, 1 job = 0.4 ERU. ERU per acre = ERU added ÷ **developable** acres everywhere (info sheets, workbook, workbook PDF) |
| **TAZ** | Traffic Analysis Zone — WFRC/MAG forecast unit for growth projections |
| **DEVACRES** | Developable acres within a TAZ (from ATO layer); used as intersection weight |

## Data Sources Config

All input layers are declared in `config/data_sources.yml`. Load with:
```r
config <- yaml::read_yaml("config/data_sources.yml")
```

Spatial cache uses **GPKG format** (never RDS for spatial data). Fix data quality issues at read time via `sf::read_sf()` arguments; do not patch mid-pipeline.

Primary sort order for output: County (primary), Tract GEOID (secondary).

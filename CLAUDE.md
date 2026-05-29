# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Purpose

Generates landscape-oriented info sheets for every census tract on Utah's Opportunity Zones 2.0 eligible list. Each tract becomes a row; each data section (map, demographics, centers, housing, transit, auto, growth) becomes a column. See `OZ-eligible Tract Report, Draft Specifications.md` for the full column schema.

## Tech Stack

- **R 4.6.0** + **Quarto** — report generation
- **renv** — dependency management (`renv.lock` is the source of truth for package versions)

## Commands

Restore the package library after cloning or when `renv.lock` changes:
```r
renv::restore()
```

Render the report:
```bash
quarto render index.qmd
```

Snapshot new packages added during development:
```r
renv::snapshot()
```

## Data Sources Config

All input data sources are declared in `config/data_sources.yml`. Fields marked `FILL_IN` need actual ArcGIS REST service URLs; fields marked `VERIFY_FIELD`/`VERIFY_VALUE` need confirmation against the live service schema. Load the config in R with:

```r
config <- yaml::read_yaml("config/data_sources.yml")
```

## Architecture

The project is in early development. The intended pipeline is:

1. **GIS data assembly** — pull census tract boundaries, OZ 2.0 eligibility list, and spatial layers (transit stations, SAP buffers, Wasatch Choice centers, freeway interchanges, TAZ forecasts, housing inventory) into R.
2. **Per-tract calculations** — for each eligible tract, compute the metrics defined in the draft spec: area intersections with center types, housing unit counts by type, transit/auto accessibility scores (weighted by developable acres from TAZ), projected growth 2027–2037 via area-proportioning where TAZ boundaries don't align with tract boundaries.
3. **Report rendering** — `index.qmd` drives Quarto output; each tract row is assembled from the computed data.

Data sources referenced in the spec:
- WFRC web map (eligible tracts): `https://wfrc.maps.arcgis.com/apps/mapviewer/index.html?webmap=3ae7c36bdc8c49e69d91f410717b3b37`
- Jan 1 2025 Housing Unit Inventory
- Regional forecast (TDM TAZ data) for 2027–2037 projections
- Urban Institute data (column TBD)

Primary sort order for output: County (primary), Tract ID (secondary).

# Code Review: `build_ugrc_map.R` — UGRC LiteBase Style Replication

**Reviewed against:** `LiteBase_VectorTileServer_root.json`, `LiteLabels_VectorTileServer_root.json`,
`VectorHillshade_VectorTileServer_root.json`, and reference screenshots `UGRC_Vector_LiteBase_L08–L18.png`

---

## Summary

The code is an ambitious, well-structured attempt to replicate the UGRC LiteBase style in ggplot2 and
gets many things right — the color constants, hillshade alpha bands, road symbol ordering, and
TRAX/FrontRunner styling are largely faithful. However there are **several confirmed bugs** and
**systematic limitations** that explain why the output still diverges from the reference screenshots.
The issues cluster into three categories: data value errors (wrong alpha/color constants), rendering
architecture gaps (centroids vs. line placement, single-size vs. per-class labels), and drawing order
deviations.

---

## Critical Issues (Blocking)

### 1. `Municipalities_Carto:2` fill alpha is 2.5× too opaque — line 152

```r
.C_M2_FILL <- rgb(153, 148, 138, 64, maxColorValue = 255)  # ← 64/255 = 0.25
```

JSON spec: `fill-color: rgba(153,148,138,0.1)` — 10% alpha. The code uses 25%. At z12–z15
(where municipality fills are active), this creates a visible gray smear over urban areas. Every city
block in the Salt Lake Valley sits inside a municipality polygon. Reference at L12–L13 shows
essentially transparent fill; code output would show gray tinting over the entire urban core.

**Fix:**
```r
.C_M2_FILL <- rgb(153, 148, 138, 26, maxColorValue = 255)  # 26/255 ≈ 0.10
```

---

### 2. City labels all rendered at one size (median of label classes) — lines 2982–2998

```r
cls_i <- as.integer(suppressWarnings(median(
  city_lbl$map_label_class, na.rm = TRUE
)))
p <- .lbl(p, city_lbl, sz = .sz_city(zoom, cls_i), ...)  # ← ONE size for ALL cities
```

The reference at L09–L11 shows "SALT LAKE CITY" visibly larger than "Taylorsville" or "Herriman".
The JSON has three prominence tiers — major (`cls %in% c(0,3,6,9,12,15)`), medium
(`cls %in% c(1,4,7,10,13,16)`), minor (remainder) — each with its own interpolated size. Using the
median collapses all labels to one size. County labels already do this correctly (loop at lines
2960–2976); city labels must do the same.

The **same bug** applies to highway labels (line 3011) and street labels (line 3038) — both use
median class for a single size.

**Fix:** Loop over label-class groups the same way county labels do, computing `sz` per group.

---

### 3. Muni Carto:2 inner-line alpha at z12–15 is 0.55 but JSON says 0.20 — line 161

```r
.C_M2_IN_SYM0_LO <- rgb(160, 134, 179, 140, maxColorValue = 255)  # 0.55 alpha
```

JSON: `rgba(160,134,179,0.20)` — 20% alpha. The comment claims this was "raised to 0.55 for print
visibility" but the outer constants (`C_M2_OUT`) at line 155 are `rgb(144, 120, 161, 46)` = 0.18
alpha, close to the JSON's 0.15. The inner constants are inflated to 0.55 — more than 2.5× the spec.
This makes the purple/green/teal/mauve inner halo visible on screen where the reference shows it as
barely perceptible.

---

## Required Changes

### 4. Drawing order: water drawn before parks; JSON has parks before water — lines 1692 vs 1811

JSON order from the LiteBase layer array:

```
Cemeteries / GolfCourses / ParksLocal  ← drawn first (~z-index 4)
UtahParksAndMonuments                  ← drawn next  (~z-index 6)
Contours                               ← ~z-index 7
...
UtahMajorRiversPoly                    ← ~z-index 10  (rivers AFTER parks)
StreamsNHDHighRes                      ← ~z-index 11
LakesNHDHighRes_OLD / GSLWaterLevel    ← ~z-index 12–13
```

The R code draws: water (step 3) → contours (step 4) → parks (step 5). This is inverted from the
JSON. The practical consequence: parks that contain streams will have their stream drawn *under* the
park fill. In the reference, rivers and streams correctly appear *over* the park fill — visible
wherever parks intersect waterways (e.g., Jordan River corridor through Liberty Park).

---

### 5. Road labels drawn at line centroids, not along the road — line 1634

```r
hwy_lbl <- extract_label_coords(.filter_highway_labels(lbl_data$highway, zoom))
str_lbl <- extract_label_coords(.filter_street_labels(lbl_data$street, zoom))
```

`extract_label_coords` calls `st_centroid`. Road name labels in Mapbox GL follow road geometry with
`symbol-placement: line`. The reference at L12–L15 shows "California Ave", "Redwood Rd", etc. running
along the road centerline. The code places them as horizontal text at the geometric midpoint. This is
the single most visually distinctive difference between R output and the reference at z12+.

The code already has `.line_label_pts()` for streams and GSL. Road labels need the same treatment:
```r
# highway: JSON symbol-spacing = 480 (shield classes use point placement, skip those)
hwy_line_lbl <- .line_label_pts(lbl_data$highway, spacing_px = 480, zoom)
# street: JSON symbol-spacing = 1000
str_line_lbl <- .line_label_pts(lbl_data$street, spacing_px = 1000, zoom)
```

Note: ggplot2 `geom_text` does not support angle-following-line. Spacing labels along the road at
correct intervals is the achievable approximation.

---

### 6. County cls=3 label at polygon centroid vs line-placed along boundary — line 1612

JSON `Counties - 72k down` (cls=3, z12–z18): `"symbol-placement":"line"`. At L12 the reference shows
"SALT LAKE" spread with letter-spacing along the county border. The code draws cls=3 at the polygon
centroid — a centered block label. Use `.line_label_pts()` on county boundary geometries for cls=3.

---

### 7. `check_overlap = TRUE` suppresses too many labels — lines 1389, 1403

Both `geom_text` and `geom_shadowtext` calls use `check_overlap = TRUE`. ggplot2's overlap check is a
greedy first-drawn-wins algorithm. Mapbox GL uses a spatial collision detection pass that is far more
permissive. The reference at L09–L11 shows 20+ suburb labels simultaneously without collision (West
Valley City, Taylorsville, Murray, Holladay, etc.). The R code with `check_overlap = TRUE` would
suppress many of these because each subsequent label is checked against all prior ones in render order.

Consider removing `check_overlap = TRUE` from the `.lbl()` helper and relying on label zoom-class
filtering to control density instead.

---

### 8. Highway shield icons and other sprite layers — no external dependencies needed

The JSON uses `icon-image` sprites in three places. All three can be approximated with pure ggplot2
shapes, keeping the file self-contained.

**Highway shields** (`Labels/Roads.../label` cls=0/1/2/11): The code currently outputs only the route
number text with no background. Use `ggplot2::geom_label()` (zero extra dependencies) with
type-specific fill/border to approximate each shield family:

| JSON shield | `geom_label()` approximation |
|---|---|
| Interstates (cls=0) | `fill="#003087"`, `color="white"`, `label.r=unit(0,"pt")` (square) |
| US Highways (cls=1) | `fill="white"`, `color="#333"`, `label.r=unit(3,"pt")` (rounded) |
| State Highways (cls=2/11) | `fill="white"`, `color="#555"`, `label.r=unit(0,"pt")` (square) |

`geom_label()` draws a filled rectangle with a border behind each text string. Set
`label.padding=unit(1.5,"pt")` and `label.size=0.3` to keep the badge compact. The text size should
match the existing `.sz(8.333)` already used for these shield classes. This produces a visually
recognizable numbered badge without any PNG or external package.

**Railroad tie tick marks** (`Base/Railroads/0` — `icon-image` at `symbol-spacing: 6.667px`):
The JSON places a perpendicular tick icon every 6.667px along the railroad line to produce the
classic sleeper/tie pattern. Approximate this by computing perpendicular offset points along each
railroad linestring (using `.line_label_pts()` for spacing, then applying a small `±` perpendicular
shift) and drawing them as short `geom_segment` stubs in the same `#B3AFAF` color at width `0.3mm`.
No external package needed — pure sf geometry arithmetic.

**Transit station markers** (`LightRailStations_UTA`, `CommuterRailStops_UTA`): The JSON already
specifies `icon-color: rgba(0,0,0,0)` (fully transparent icon) — these are intentionally invisible
point markers whose only visible output is the station name text label. The code already handles this
correctly (comment at line 2245). No change needed.

**City capital marker** (sym=0, `CitiesTownsLocations_VT`): Already approximated as `shape=8` (star)
at line 2417. This is a valid drop-in replacement.

---

### 9. Muni Carto:2 outer-line alpha is 0.18 vs JSON 0.15 — lines 155–158

```r
.C_M2_OUT <- list(
  "0" = rgb(144, 120, 161, 46, maxColorValue = 255),  # 46/255 = 0.18
```

JSON: `rgba(144,120,161,0.15)`. Minor (3 percentage points), but the pattern of raising all these
alphas for print means screen rendering looks over-saturated for municipality halos. Consider a
`for_print` parameter rather than baking inflated values in, since the project produces both screen
maps and PDFs.

---

### 10. Road fill linetype for sym=5 (unpaved) uses wrong scale — line 2133

```r
lty <- if (si == 5L) "32" else "solid"
```

JSON `Major Local Roads Not Paved_7/0` (z9–11): `line-dasharray: [3, 2]` with `line-width: 1.333px`.
ggplot2 hex linetype digits represent proportions of 1 *linewidth unit*, not absolute pixels. At a
fill width of `fil = 1.333px`, `"32"` produces dashes that grow proportionally with line width. This
makes the visual dash spacing different from the JSON spec at every zoom. Use `"22"` to approximate a
simple even-dashed unpaved road.

---

## Suggestions

### 11. Outdated comment vs code in `.HS_ALPHA_TBL` — line 109

```r
# Using 0.70 in ggplot2...
`0` = c(lt11 = 1.00, ...)   # ← comment says 0.70, code implements 1.00
```

At z8–10, the 90m shade polygons at alpha=1.00 do match the reference (dark mountain shadows are
solid medium gray). The comment is wrong. Delete or correct it to say `1.00`.

---

### 12. `library()` imports violate project convention — lines 9–13

```r
library(sf)
library(ggplot2)
library(dplyr)
library(showtext)
library(shadowtext)
```

The project requires all function calls to use `package::function()` notation and not rely on
`library()` imports. The draw section already uses `ggplot2::geom_sf()`, `sf::st_bbox()`, etc.
correctly throughout. Remove the `library()` calls; the file is already self-contained without them.

---

### 13. `%>%` requires `dplyr` attached — lines 2988, 3003

With `library(dplyr)` removed (see #12), `%>%` calls at lines 2988 and 3003 would fail. Replace with
the native pipe `|>` (R ≥ 4.1), which has no package dependency.

---

### 14. Lake label size uses median of classes — line 2525

```r
sz_lake <- .sz_water_lbl(
  zoom,
  suppressWarnings(as.integer(median(cls_lake[cls_lake >= 0], na.rm = TRUE)))
)
```

Same structural issue as city labels (#2): major lakes (cls=0) and minor lakes (cls=2) get the same
label size from the median. Loop over class groups to compute size per class.

---

### 15. `extract_label_coords` called on raw line geometries — line 549

```r
co <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(sf_obj))))
```

The `st_centroid` of a curved LINESTRING may not lie *on* the line. `.line_label_pts()` already
handles this correctly via arc-length interpolation. Call `.line_label_pts()` first for line-geometry
label layers, then `extract_label_coords` on the resulting POINT geometries.

---

## Verdict

**Request Changes** — Issues #1 (Muni fill alpha), #2 (city label single-size collapse), and #4
(drawing order inversion) produce clearly visible deviations from the reference at every zoom level.
Issues #5 and #6 (line-placed road/county labels) are the biggest remaining gap but are architectural
limitations of ggplot2 rather than code errors.

---

## Prioritized Implementation Plan

| Priority | Issue | Action |
|---|---|---|
| P0 | #1 — Muni fill alpha | Change `rgb(..., 64, ...)` → `rgb(..., 26, ...)` (10% per JSON) |
| P0 | #3 — Muni inner-line alpha | Lower `.C_M2_IN_SYM0_LO` and `.C_M2_IN` alphas to JSON spec |
| P0 | #2 — Per-class city/hwy/street labels | Add per-class loop (copy county label loop pattern) |
| P1 | #4 — Drawing order parks vs water | Move park draw block before water draw block |
| P1 | #7 — `check_overlap` over-suppression | Remove `check_overlap = TRUE` from `.lbl()` |
| P1 | #5 — Road labels along lines | Route hwy/street through `.line_label_pts()` |
| P1 | #6 — County cls=3 along boundary | Route cls=3 through `.line_label_pts()` on boundary geom |
| P1 | #8a — Highway shields | Replace bare text with `geom_label()` per shield type (no new pkg) |
| P1 | #8b — Railroad tie ticks | Perpendicular `geom_segment` stubs via arc-length spacing (no new pkg) |
| P2 | #10 — Unpaved road linetype | Change `"32"` → `"22"` |
| P2 | #12/#13 — Remove `library()`, use `\|>` | Style cleanup |
| P3 | #11 — Stale comment | Fix `.HS_ALPHA_TBL` comment to say `1.00` |
| P3 | #14/#15 — Per-class lake labels | Loop over class groups for lake label sizing |

**Known unresolvable limitations in ggplot2 (document, do not attempt to fix):**
- `text-letter-spacing` (tracking) — no equivalent in `geom_text`/`geom_shadowtext`
- Text rotation following line direction — `geom_text` does not support per-feature angle from geometry
- Multi-tile stitching — current architecture fetches one PBF tile per call

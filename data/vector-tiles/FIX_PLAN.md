# UGRC Basemap Style Fidelity Fix Plan

Systematic fixes for all 31 discrepancies found between `ugrc_basemap.R` and the
authoritative JSON spec (`root-LightBase.json`, `root-VectorHillshade.json`).

Status legend: ⬜ pending · ✅ fixed · 🚫 limitation (not fixable in ggplot2)

---

## Phase 10 — Road Layer Z-Order + Missing Sym 0
*Issues #1, #2, #3, #5*

- ✅ **10.1** road_cas_order = c(4L,2L,3L,6L,5L,7L,1L,0L)
  JSON bottom→top: sym4 Major Local → sym2 US → sym3 State → sym6 Fed → sym5 Unpaved → sym7 Local → sym1 Ramps
- ✅ **10.2** road_fil_order = c(7L,5L,6L,4L,3L,2L,1L,0L)
  JSON fill bottom→top is the reverse of casing order
- ✅ **10.3** RSTOPS[["0"]] white-version + RSTOPS[["0ir"]] I+R; sym 0 in both loops
- ✅ **10.4** I+R moved to step 14a (after buildings)
  JSON: Buildings ≈ index 359, Roads-I+R ≈ 362 (after buildings)

---

## Phase 11 — Road Linewidth Tables Rebuild
*Issue #8*

Replace the sparse `approx()` stop tables with exact flat-band-correct stops extracted
directly from the JSON. Include both endpoints of every flat band so `approx()` returns
the correct constant value across the full band.

- ⬜ **11.1** Sym 0 I+R casing: `z=[5,6,7,8,9,10,11,12,13,14,15,16,17,18]` `cas=[2.133,3.0,3.333,4.667,6.0,6.667,7.333,8.0,8.0,9.333,14.667,20.0,25.333,26.667]` `fil=[1.333,1.333,1.667,2.667,4.0,4.667,5.333,6.0,6.0,7.333,12.0,17.333,22.667,24.0]`
- ⬜ **11.2** Sym 0 Roads-white-version: `z=[5,6,7,8,10,11,12,13,14,15,16,17,18]` `cas=[1.333,1.333,1.333,4.667,5.333,6.0,6.667,8.0,9.333,12.0,14.667,18.0,18.667]` `fil=[1.333,1.333,1.333,3.333,4.0,4.667,5.333,6.667,8.0,10.667,13.333,16.0,17.333]`
- ⬜ **11.3** Sym 1 Ramps: `z=[11,12,13,14,15,16,17,18]` `cas=[2.667,3.667,4.667,6.0,7.667,9.333,16.0,16.0]` `fil=[1.333,2.0,3.333,4.667,6.0,7.333,13.333,14.667]`
- ⬜ **11.4** Sym 2 US Hwy: `z=[6,8,10,11,12,13,14,15,16,17,18]` `cas=[2.0,3.0,4.333,4.333,5.333,6.667,8.0,11.333,15.333,20.0,21.333]` `fil=[1.0,1.667,2.667,3.333,4.0,4.667,6.0,8.667,13.333,17.333,18.667]`
- ⬜ **11.5** Sym 3 State Hwy: `z=[7,8,9,10,11,12,13,14,15,16,17,18]` `cas=[2.0,2.667,3.0,4.0,4.333,5.333,6.667,8.0,11.333,15.333,20.0,21.333]` `fil=[0.933,1.333,1.667,2.667,3.333,4.0,4.667,6.0,8.667,13.333,17.333,18.667]`
- ⬜ **11.6** Sym 4 Major Local Paved: `z=[9,10,11,12,13,14,15,16,17]` `cas=[2.333,2.667,3.333,4.0,5.0,6.0,8.0,13.333,20.0]` `fil=[1.067,1.333,2.0,2.667,3.333,4.667,6.667,12.0,17.333]`
- ⬜ **11.7** Sym 5 Major Local Unpaved: `z=[9,11,12,13,14,15,16,17]` `cas=[2.667,2.667,3.333,4.0,6.0,6.667,13.333,13.333]` `fil=[1.333,1.333,2.0,2.667,4.667,5.333,12.0,12.0]`
- ⬜ **11.8** Sym 6 Other Federal Aid: `z=[11,12,13,14,15,16,17]` `cas=[2.667,3.333,4.667,6.0,6.667,13.333,20.0]` `fil=[1.333,2.0,3.333,4.667,5.333,12.0,17.333]`
- ⬜ **11.9** Sym 7 Local Roads: `z=[11,12,13,14,15,16,17]` `cas=[0.667,2.667,3.333,4.0,6.0,9.333,14.667]` `fil=[0,1.333,2.0,2.667,4.667,8.0,13.333]`
  Note: `fil=0` at z11 means no fill at z11 — skip draw when `lwf <= 0` (already coded)

---

## Phase 12 — Road Zoom Gates + Sym 5/6/7 Hairline Colours
*Issues #7, #28*

- ⬜ **12.1** `.ROAD_MIN_ZOOM`: `"6" = 11` → `"6" = 10`; `"7" = 12` → `"7" = 11`
- ⬜ **12.2** Sym 6 casing colour at z10-11: use `#B0B0B0` not `.C_ROAD_CAS` — add zoom+sym conditional in casing loop
- ⬜ **12.3** Sym 5 casing colour at z9-11: use `#BABABA` not `.C_ROAD_CAS` — add zoom+sym conditional in casing loop

---

## Phase 13 — Hillshade 90m Opacity + Utah Overdraw
*Issues #4, #31*

- ⬜ **13.1** `.HS_ALPHA_TBL`: set `lt11 = 1.00` for all four symbols (was `0.70`); JSON 90m has no `fill-opacity`
- ⬜ **13.2** Utah tint overdraw: draw tint from `ground$utah` single polygon not `counties` N-row frame

---

## Phase 14 — Streams Zoom Gate
*Issue #6*

- ⬜ **14.1** `.fetch_water()`: `streams = if (zoom >= 11)` → `if (zoom >= 6)`
  `.stream_lw()` for sym 0 already has stops starting at z6 — no draw-loop changes needed

---

## Phase 15 — Contour Linewidths
*Issues #21, #22*

- ⬜ **15.1** Sym 4 (500ft) linewidth: `.lw(1.0)` at all zooms → `.lw(0.667)` at z10-11, `.lw(1.0)` at z12+
- ⬜ **15.2** Sym 5 (1000ft) linewidth: `.lw(1.333)` at all zooms → `.lw(0.667)` z9-10, `.lw(1.0)` z10-12, `.lw(1.333)` z12+

---

## Phase 16 — TRAX Linewidths + Sym 10
*Issues #18, #19*

- ⬜ **16.1** `.trax_lw()`: replace `approx()` with step-function — exact JSON bands z12-14 / z14-16 / z16+
- ⬜ **16.2** Sym 10 draw width: change `linewidth = lw_cas` → `linewidth = lw_fil` (JSON 1.333px = fill width)

---

## Phase 17 — Commuter Rail + Trail Fixes
*Issues #20, #10, #27*

- 🚫 **17.1** Commuter rail dasharray `[4,2.4]` not representable in ggplot2 hex linetype — document the limitation
- ⬜ **17.2** Add trail-specific colour constants: `.C_TRAIL_LBL_TXT = "#8C8989"`, `.C_TRAIL_LBL_HALO = rgba(230,228,225,0.60)`
- ⬜ **17.3** Label section 15h: replace `.C_PARK_LBL` / `.C_PARK_HALO` with `.C_TRAIL_LBL_TXT` / `.C_TRAIL_LBL_HALO`
- ⬜ **17.4** Trail fill linetype: `"11"` at all zooms → `"22"` at z14-15, `"11"` at z15+

---

## Phase 18 — Label Halo and Colour Constants
*Issues #9, #11, #12, #13, #14, #15, #25*

- ⬜ **18.1** Stream labels: add `.C_STM_HALO = rgb(230,226,218,140)` [rgba(230,226,218,0.55)]; use in section 15c
- ⬜ **18.2** Lake labels: fix `.C_WATER_HALO` → `rgb(183,199,199,153)` [rgba(183,199,199,0.60)] — wrong base colour + alpha
- ⬜ **18.3** Park halo constants: add `.C_PARK_HALO = rgb(194,204,173,77)` [0.30] (golf/cemetery) and `.C_PARK_HALO_LO = rgb(194,204,173,51)` [0.20] (parks); update sections 15e/15f/15g
- ⬜ **18.4** Monument label colour: add `.C_MON_LBL_HI = "#8C8A89"` (z8-13); zoom-conditional in section 15d
- ⬜ **18.5** Ski area halo: `.C_SKI_HALO` → `rgb(230,227,225,140)` [rgba(230,227,225,0.55)]
- ⬜ **18.6** WesternStates labels: remove halo entirely — JSON has no `text-halo-color` for this layer
- ⬜ **18.7** GSL halo: zoom-conditional — apply only at z<10; fix alpha 0.50 → 0.60

---

## Phase 19 — State Park Border + Municipality Carto:2
*Issues #16, #17*

- ⬜ **19.1** `.C_SP_BDR`: `"#8C867E"` → `"#999391"` (JSON z13+ State Park inner line-color)
- ⬜ **19.2** Carto:2 sym 0 inner stroke z12-15: add `.C_M2_IN_SYM0_LO = rgb(160,134,179,51)` [rgba(160,134,179,0.20)]; zoom-conditional in draw loop

---

## Phase 20 — POI, WesternStates, Schools, Ski Lift, Buildings
*Issues #23, #24, #26, #29, #30*

- ⬜ **20.1** `gnis_bay` circle: move draw from step 14b to before county borders (after hillshade, step 2)
- ⬜ **20.2** School circle sizes: `size = 1.5/1.2` → `.cr(1.83333)` for both; `stroke = 0.3/0.6` → `.lw(0.866667)` for both
- ⬜ **20.3** WesternStates label fetch gate: `zoom >= 8` → `zoom >= 4`; add per-class zoom filter (class 0: z4-5, class 1: z6-8, class 2: z8+)
- 🚫 **20.4** Ski lift dasharray: `[18,3,1,3]` → ggplot2 hex max is F(15), encodes as [15,3,1,3] — document only
- ⬜ **20.5** Buildings zoom gate: decision — change `zoom >= 14` → `zoom >= 12` for spec-exact, or document as intentional perf optimisation

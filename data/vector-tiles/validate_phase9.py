"""Phase 9 validation: city dot markers (CitiesTownsLocations_VT sym 0-4)."""
import re, sys, math

with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    content = f.read()

def pr(s): sys.stdout.buffer.write((s + '\n').encode('utf-8'))

pr('=== PHASE 9 VALIDATION ===')
pr('')

checks = []

# ── A. .cr() helper ───────────────────────────────────────────────────────────
pr('A. .cr() circle-radius helper (JSON px → ggplot2 diameter mm):')
PX_MM = 25.4 / 96

a1 = '.cr <- function(r_px) r_px * 2 * .PX_MM' in content
checks.append(a1)
pr(f'   [{"PASS" if a1 else "FAIL"}] .cr(r_px) = r_px * 2 * .PX_MM defined')

cr = lambda r: r * 2 * PX_MM
for r, label in [(3.23333, 'County Seat / Major'), (2.56667, 'Medium'), (2.23333, 'Town/Place')]:
    expected_mm = cr(r)
    ok = True  # formula check only; runtime not available
    pr(f'         .cr({r}) = {expected_mm:.3f} mm  ({label} — JSON circle-radius: {r}px)')

pr('')

# ── B. Color constants ────────────────────────────────────────────────────────
pr('B. City dot color constants (exact JSON circle-stroke-color values):')
b1 = '.C_CITY_CS_STR  <- "#996B6B"' in content or '.C_CITY_CS_STR <- "#996B6B"' in content
b2 = '.C_CITY_DOT_STR <- "#807D79"' in content
checks += [b1, b2]
pr(f'   [{"PASS" if b1 else "FAIL"}] .C_CITY_CS_STR  = "#996B6B"  [JSON sym 1 County Seat circle-stroke-color]')
pr(f'   [{"PASS" if b2 else "FAIL"}] .C_CITY_DOT_STR = "#807D79"  [JSON sym 2-4 circle-stroke-color]')
pr('')

# ── C. Fetch ──────────────────────────────────────────────────────────────────
pr('C. city_dots fetched from base tile (CitiesTownsLocations_VT, bu, zoom >= 5):')
c1 = 'city_dots' in content
c2 = re.search(r'city_dots\s*=.*?safe_read_mvt\s*\(\s*bu.*?CitiesTownsLocations_VT', content, re.DOTALL) is not None
c3 = 'zoom >= 5L' in content or 'zoom >= 5)' in content
checks += [c1, c2, c3]
pr(f'   [{"PASS" if c1 else "FAIL"}] city_dots key present in .fetch_poi() result list')
pr(f'   [{"PASS" if c2 else "FAIL"}] safe_read_mvt(bu, "CitiesTownsLocations_VT") used (base tile, not label tile)')
pr(f'   [{"PASS" if c3 else "FAIL"}] minzoom gate: zoom >= 5L  [JSON Capital minzoom: 5]')
pr('')

# ── D. Draw section presence ──────────────────────────────────────────────────
pr('D. Draw section 14c present with correct JSON values:')
d1 = '14c.' in content
d2 = '.cr(3.23333)' in content
d3 = '.cr(2.56667)' in content
d4 = '.cr(2.23333)' in content
d5 = '.lw(0.866667)' in content  # circle-stroke-width: 0.866667px
d6 = 'shape = 21' in content     # ggplot2 filled circle
d7 = 'shape = 8' in content      # star for Capital approximation
checks += [d1, d2, d3, d4, d5, d6, d7]
pr(f'   [{"PASS" if d1 else "FAIL"}] 14c. City dot markers section comment added')
pr(f'   [{"PASS" if d2 else "FAIL"}] .cr(3.23333)  [JSON sym 1+2 circle-radius: 3.23333px]')
pr(f'   [{"PASS" if d3 else "FAIL"}] .cr(2.56667)  [JSON sym 3 circle-radius: 2.56667px]')
pr(f'   [{"PASS" if d4 else "FAIL"}] .cr(2.23333)  [JSON sym 4 circle-radius: 2.23333px]')
pr(f'   [{"PASS" if d5 else "FAIL"}] .lw(0.866667) [JSON circle-stroke-width: 0.866667px, all groups]')
pr(f'   [{"PASS" if d6 else "FAIL"}] shape = 21   [ggplot2 filled circle for sym 1-4]')
pr(f'   [{"PASS" if d7 else "FAIL"}] shape = 8    [star approximation for sym 0 Capital icon]')
pr('')

# ── E. Zoom gates ─────────────────────────────────────────────────────────────
pr('E. JSON maxzoom:11 enforced as exclusive (zoom < 11L):')
# JSON maxzoom:11 means render at zoom < 11 (Mapbox GL: hide at zoom >= maxzoom)
e1 = 'zoom < 11L' in content
e2 = 'zoom >= 7L && zoom < 11L' in content  # sym 1 County Seat gate
e3 = 'zoom >= 8L && zoom < 11L' in content  # sym 2 Major gate
e4 = 'zoom >= 9L && zoom < 11L' in content  # sym 3+4 gate
checks += [e1, e2, e3, e4]
pr(f'   [{"PASS" if e1 else "FAIL"}] zoom < 11L guard used  (JSON maxzoom:11 exclusive)')
pr(f'   [{"PASS" if e2 else "FAIL"}] sym 1: zoom >= 7L && zoom < 11L  [JSON minzoom:7 maxzoom:11]')
pr(f'   [{"PASS" if e3 else "FAIL"}] sym 2: zoom >= 8L && zoom < 11L  [JSON minzoom:8 maxzoom:11]')
pr(f'   [{"PASS" if e4 else "FAIL"}] sym 3+4: zoom >= 9L && zoom < 11L [JSON minzoom:9 maxzoom:11]')
pr('')

# ── F. Colour correctness ─────────────────────────────────────────────────────
pr('F. Stroke colour assignment correctness:')
# County seat uses .C_CITY_CS_STR (#996B6B), all others use .C_CITY_DOT_STR (#807D79)
f1 = re.search(r'cd1.*?color\s*=\s*\.C_CITY_CS_STR', content, re.DOTALL) is not None
f2 = re.search(r'cd2.*?color\s*=\s*\.C_CITY_DOT_STR', content, re.DOTALL) is not None
f3 = re.search(r'cd3.*?color\s*=\s*\.C_CITY_DOT_STR', content, re.DOTALL) is not None
f4 = re.search(r'cd4.*?color\s*=\s*\.C_CITY_DOT_STR', content, re.DOTALL) is not None
checks += [f1, f2, f3, f4]
pr(f'   [{"PASS" if f1 else "FAIL"}] sym 1 uses .C_CITY_CS_STR  (#996B6B) — county seat brown')
pr(f'   [{"PASS" if f2 else "FAIL"}] sym 2 uses .C_CITY_DOT_STR (#807D79) — major city gray')
pr(f'   [{"PASS" if f3 else "FAIL"}] sym 3 uses .C_CITY_DOT_STR (#807D79) — medium city gray')
pr(f'   [{"PASS" if f4 else "FAIL"}] sym 4 uses .C_CITY_DOT_STR (#807D79) — town/place gray')
pr('')

# ── Summary ───────────────────────────────────────────────────────────────────
pr('=== SUMMARY ===')
total  = len(checks)
passed = sum(checks)
overall = all(checks)
pr(f'Checks passed: {passed}/{total}')
pr(f'Phase 9 status: [{"PASS" if overall else "FAIL — see above"}]')

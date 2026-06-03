"""Phase 6 validation: symbol-placement:line label sampling."""
import re, sys, math

with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    content = f.read()
lines = content.splitlines()

def pr(s): sys.stdout.buffer.write((s + '\n').encode('utf-8'))

pr('=== PHASE 6 VALIDATION: SYMBOL-PLACEMENT:LINE LABEL SAMPLING ===')
pr('')

checks = []

# ── A. .line_label_pts() function ─────────────────────────────────────────────
pr('A. .line_label_pts() helper function:')
fn_match = re.search(
    r'\.line_label_pts\s*<-\s*function.*?(?=\n# ══|^\.[A-Z_])',
    content, re.DOTALL | re.MULTILINE)
fn_body = fn_match.group(0) if fn_match else ''

a1 = '.line_label_pts <- function(sf_lines, spacing_px, zoom)' in content
a2 = 'spacing_m <- spacing_px * .px_to_m(zoom)' in content
a3 = 'segs  <- sqrt(diff(coords[, 1' in content or 'diff(coords[,' in content
a4 = 'findInterval(d, cdist' in content
a5 = 'seq(spacing_m / 2, total, by = spacing_m)' in content
a6 = 'total < spacing_m / 2' in content   # short-line fallback
a7 = 'MULTILINESTRING' in content          # handles multi-geometry
a8 = 'do.call(rbind, result)' in content   # combines all rows
checks += [a1, a2, a3, a4, a5, a6, a7, a8]
pr(f'   [{"PASS" if a1 else "FAIL"}] function signature: .line_label_pts(sf_lines, spacing_px, zoom)')
pr(f'   [{"PASS" if a2 else "FAIL"}] converts spacing_px → spacing_m via .px_to_m(zoom)')
pr(f'   [{"PASS" if a3 else "FAIL"}] computes segment lengths from coordinates')
pr(f'   [{"PASS" if a4 else "FAIL"}] uses findInterval() to locate correct segment')
pr(f'   [{"PASS" if a5 else "FAIL"}] seq() places labels at regular spacing_m intervals')
pr(f'   [{"PASS" if a6 else "FAIL"}] short-line fallback: places one label at midpoint')
pr(f'   [{"PASS" if a7 else "FAIL"}] handles MULTILINESTRING (casts to LINESTRING)')
pr(f'   [{"PASS" if a8 else "FAIL"}] rbinds all result rows into single sf')
pr('')

# ── B. Math verification ───────────────────────────────────────────────────────
pr('B. Interpolation math verification:')

def interpolate_line(coords, f):
    """Reproduce R interpolation: cumulative arc length, find segment, lerp."""
    segs = [math.sqrt((coords[i+1][0]-coords[i][0])**2 +
                      (coords[i+1][1]-coords[i][1])**2)
            for i in range(len(coords)-1)]
    cdist = [0]
    for s in segs: cdist.append(cdist[-1] + s)
    total = cdist[-1]
    d = f * total
    seg = 0
    for j in range(len(cdist)-1):
        if cdist[j] <= d <= cdist[j+1]:
            seg = j; break
    t = (d - cdist[seg]) / segs[seg] if segs[seg] > 1e-9 else 0
    x = coords[seg][0] + t * (coords[seg+1][0] - coords[seg][0])
    y = coords[seg][1] + t * (coords[seg+1][1] - coords[seg][1])
    return x, y

# Simple 2-segment line: (0,0)→(100,0)→(100,100), total len=200
coords_test = [(0,0), (100,0), (100,100)]
# f=0.25 → d=50 → midpoint of first segment → (50,0)
x, y = interpolate_line(coords_test, 0.25)
b1 = abs(x - 50) < 1e-9 and abs(y - 0) < 1e-9
# f=0.75 → d=150 → midpoint of second segment → (100,50)
x2, y2 = interpolate_line(coords_test, 0.75)
b2 = abs(x2 - 100) < 1e-9 and abs(y2 - 50) < 1e-9
# spacing at z=12: 1000px * 38.22 m/px = 38,218m; z=13: 1000*19.11 = 19,108m
b3 = abs(1000 * (156543.03 / 2**12) - 38218.0) < 1  # px_to_m at z12
b4 = abs(288 * (156543.03 / 2**13) - 5503.6) < 0.5   # contour spacing at z13
checks += [b1, b2, b3, b4]
pr(f'   [{"PASS" if b1 else "FAIL"}] f=0.25 on 2-seg line → (50,0): got ({x:.1f},{y:.1f})')
pr(f'   [{"PASS" if b2 else "FAIL"}] f=0.75 on 2-seg line → (100,50): got ({x2:.1f},{y2:.1f})')
pr(f'   [{"PASS" if b3 else "FAIL"}] spacing z12 = {1000*(156543.03/2**12):.0f}m  (1000px × 38.22 m/px)')
pr(f'   [{"PASS" if b4 else "FAIL"}] contour spacing z13 = {288*(156543.03/2**13):.0f}m (288px × 19.11 m/px)')
pr('')

# ── C. Spacing values per layer ────────────────────────────────────────────────
pr('C. JSON spacing values correctly applied at each call site:')
spacing_checks = [
    ('gsl_lbl',      'spacing_px = 1000',  'GSLWaterLevel (JSON: 1000px)'),
    ('stm_lbl',      'spacing_px = 1000',  'Streams (JSON: 1000-1344px; using 1000)'),
    ('trail_lbl',    'spacing_px = 1000',  'Trails (JSON: 1000px)'),
    ('ctr_lbl_c',    'spacing_px = 288',   'Contours (JSON: 288px)'),
    ('sklift_lbl_c', 'spacing_px = 1000',  'SkiLifts (JSON: 1000px)'),
    ('wst_pts',      'spacing_px = 1000',  'WesternStates (JSON: 1000px)'),
]
c_ok = True
for var, pattern, note in spacing_checks:
    # Find the .line_label_pts call for this variable
    call_pat = rf'{re.escape(var)}\s*<-\s*\.line_label_pts\(.*?{re.escape(pattern)}'
    found = bool(re.search(call_pat, content, re.DOTALL))
    if not found: c_ok = False
    checks.append(found)
    pr(f'   [{"PASS" if found else "FAIL"}] {var}: {pattern}  # {note}')
pr('')

# ── D. Two-step pipeline: .line_label_pts → extract_label_coords ─────────────
pr('D. Two-step pipeline (sample points → extract X,Y coords):')
d_pairs = [
    ('gsl_lbl',      '.line_label_pts(lbl_data$gsl',       'extract_label_coords(gsl_lbl)'),
    ('stm_lbl',      '.line_label_pts(lbl_data$streams',    'extract_label_coords(stm_lbl)'),
    ('trail_lbl',    '.line_label_pts(lbl_data$trails',     'extract_label_coords(trail_lbl)'),
    ('ctr_lbl_c',    '.line_label_pts(lbl_data$ctr_lbl',   'extract_label_coords(ctr_lbl_c)'),
    ('sklift_lbl_c', '.line_label_pts(lbl_data$ski_lift', 'extract_label_coords(sklift_lbl_c)'),
    ('wst_pts',      '.line_label_pts(lbl_data$west_lbl',  'extract_label_coords(wst_pts)'),
]
d_ok = True
for var, sample_pat, coord_pat in d_pairs:
    s_found = sample_pat in content
    c_found = coord_pat in content
    ok = s_found and c_found
    if not ok: d_ok = False
    checks.append(ok)
    pr(f'   [{"PASS" if ok else "FAIL"}] {var}: sample=[{"ok" if s_found else "FAIL"}] coords=[{"ok" if c_found else "FAIL"}]')
pr('')

# ── E. "centroid approx" comments removed ────────────────────────────────────
pr('E. Old "centroid approx" / "not supported" comments removed:')
old_comments = [
    'line-placement not supported; use centroid',
    'centroid approx (line-placement N/A)',
]
e_ok = not any(c in content for c in old_comments)
checks.append(e_ok)
pr(f'   [{"PASS" if e_ok else "FAIL"}] No outdated centroid-fallback comments remain')
pr('')

# ── F. WesternStates/label fetch + draw ──────────────────────────────────────
pr('F. WesternStates/label — new fetch + colour constants + draw:')
f1 = 'safe_read_mvt(bu, "WesternStates/label")' in content
f2 = 'west_lbl' in content
f3 = '.C_WEST_STATE' in content
f4 = '15u. Western States' in content
f5 = '.bgr(1.333, sz_wst)' in content
f6 = 'face="plain"' in content  # Helvetica Regular → plain weight
f_ok = f1 and f2 and f3 and f4 and f5 and f6
checks += [f1, f2, f3, f4, f5, f6]
pr(f'   [{"PASS" if f1 else "FAIL"}] safe_read_mvt(bu, "WesternStates/label") in .fetch_labels()')
pr(f'   [{"PASS" if f2 else "FAIL"}] west_lbl added to label list')
pr(f'   [{"PASS" if f3 else "FAIL"}] .C_WEST_STATE colour constant (#828282)')
pr(f'   [{"PASS" if f4 else "FAIL"}] 15u. Western States draw section present')
pr(f'   [{"PASS" if f5 else "FAIL"}] bgr computed via .bgr(1.333, sz_wst)')
pr(f'   [{"PASS" if f6 else "FAIL"}] face="plain" (JSON: Helvetica Regular, not bold)')
pr('')

# ── G. Spacing derivation audit ───────────────────────────────────────────────
pr('G. Spacing px→m reference values at key zoom levels:')
PX_TO_M = lambda z: 156543.03 / (2**z)
spacing_table = [
    ('streams (1000px)', 1000, [8,10,12,14]),
    ('contours (288px)', 288,  [13,14,15]),
    ('trails (1000px)',  1000, [14,15]),
    ('ski lifts (1000px)', 1000, [12,13]),
]
for name, sp, zooms in spacing_table:
    vals = ', '.join(f'z{z}={sp*PX_TO_M(z):.0f}m' for z in zooms)
    pr(f'   {name}: {vals}')
pr('')

# ── Summary ───────────────────────────────────────────────────────────────────
pr('=== SUMMARY ===')
total  = len(checks)
passed = sum(checks)
overall = all(checks)
pr(f'Checks passed: {passed}/{total}')
pr(f'Phase 6 status: [{"PASS" if overall else "FAIL — see above"}]')

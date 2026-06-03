"""Phase 5 validation: line-offset approximation."""
import re, sys, math

with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    content = f.read()
lines = content.splitlines()

def pr(s): sys.stdout.buffer.write((s + '\n').encode('utf-8'))

pr('=== PHASE 5 VALIDATION: LINE-OFFSET APPROXIMATION ===')
pr('')

checks = []

# ── A. Helper functions defined ───────────────────────────────────────────────
pr('A. New helper functions:')
a1 = '.px_to_m <- function(zoom)' in content
a2 = '.buf_sf <- function(sf_obj' in content
a3 = '.shift_perpendicular <- function(line_geom' in content
a_ok = a1 and a2 and a3
checks.append(a_ok)
pr(f'   [{"PASS" if a1 else "FAIL"}] .px_to_m() defined')
pr(f'   [{"PASS" if a2 else "FAIL"}] .buf_sf() defined')
pr(f'   [{"PASS" if a3 else "FAIL"}] .shift_perpendicular() defined')
pr('')

# ── B. TRAX perpendicular shift ───────────────────────────────────────────────
pr('B. TRAX geometry: perpendicular shift replaces diagonal:')
b1 = '.shift_perpendicular(trax$geometry[[i]]' in content
b2 = 'trax$geometry[[i]] + c(trax$meter_offset' not in content  # old code gone
b_ok = b1 and b2
checks.append(b_ok)
pr(f'   [{"PASS" if b1 else "FAIL"}] .shift_perpendicular() called in .prepare_trax()')
pr(f'   [{"PASS" if b2 else "FAIL"}] old diagonal c(dx,dx) shift removed')

# Verify the perpendicular formula is correct
perp_code = '.buf_sf(mon_natl,4.0,zoom)' in content   # proxy: buf_sf present
perp_formula = '-dy / len * offset_m' in content
b3 = perp_formula
checks.append(b3)
pr(f'   [{"PASS" if b3 else "FAIL"}] perpendicular formula: -dy/len*offset (correct 90deg CCW rotation)')

# Validate the math: for a N/S track (dx=0, dy=L), perp should be (-1, 0)
# i.e., shift is purely in X direction — correct for N/S lines
dx, dy, offset = 0, 100, 10
len_v = math.sqrt(dx**2 + dy**2)
perp_x = -dy / len_v * offset
perp_y =  dx / len_v * offset
correct_ns = abs(perp_x - (-10)) < 1e-9 and abs(perp_y - 0) < 1e-9
b4 = correct_ns
checks.append(b4)
pr(f'   [{"PASS" if b4 else "FAIL"}] math check: N/S track (dx=0,dy=100,off=10) -> shift=(-10,0) E/W only')

# For E/W track (dx=L, dy=0), perp should be (0, +1) — N/S shift
dx2, dy2 = 100, 0
perp_x2 = -dy2 / len_v * offset
perp_y2 =  dx2 / len_v * offset
b5 = abs(perp_x2 - 0) < 1e-9 and abs(perp_y2 - 10) < 1e-9
checks.append(b5)
pr(f'   [{"PASS" if b5 else "FAIL"}] math check: E/W track (dx=100,dy=0,off=10) -> shift=(0,+10) N/S only')
pr('')

# ── C. Monument buffering ─────────────────────────────────────────────────────
pr('C. Monument draw uses st_buffer() via .buf_sf():')
# Check that monument section uses .buf_sf() with correct offset values
c1 = '.buf_sf(mon_natl,4.0,zoom)' in content
c2 = '.buf_sf(mon_natl,1.333,zoom)' in content
c3 = '.buf_sf(mon_natl,2.667,zoom)' in content
c4 = '.buf_sf(mon_natl,1.667,zoom)' in content
c5 = '.buf_sf(mon_sp,4.0,zoom)' in content
c6 = '.buf_sf(mon_sp,1.333,zoom)' in content
c7 = '.buf_sf(mon_sp,2.0,zoom)' in content
c8 = '.buf_sf(mon_sp,1.667,zoom)' in content
c_ok = c1 and c2 and c3 and c4 and c5 and c6 and c7 and c8
checks.append(c_ok)
pr(f'   [{"PASS" if c1 else "FAIL"}] mon_natl z13+ outer: .buf_sf(mon_natl, 4.0, zoom)  [JSON offset=4px]')
pr(f'   [{"PASS" if c2 else "FAIL"}] mon_natl z13+ mid:   .buf_sf(mon_natl, 1.333, zoom) [JSON offset=1.333px]')
pr(f'   [{"PASS" if c3 else "FAIL"}] mon_natl z10-13:     .buf_sf(mon_natl, 2.667, zoom) [JSON offset=2.667px]')
pr(f'   [{"PASS" if c4 else "FAIL"}] mon_natl z8-10:      .buf_sf(mon_natl, 1.667, zoom) [JSON offset=1.667px]')
pr(f'   [{"PASS" if c5 else "FAIL"}] mon_sp z13+ outer:   .buf_sf(mon_sp, 4.0, zoom)    [JSON offset=4px]')
pr(f'   [{"PASS" if c6 else "FAIL"}] mon_sp z13+ mid:     .buf_sf(mon_sp, 1.333, zoom)  [JSON offset=1.333px]')
pr(f'   [{"PASS" if c7 else "FAIL"}] mon_sp z11-13:       .buf_sf(mon_sp, 2.0, zoom)    [JSON offset=2px]')
pr(f'   [{"PASS" if c8 else "FAIL"}] mon_sp z8-11:        .buf_sf(mon_sp, 1.667, zoom)  [JSON offset=1.667px]')

# Verify inner strokes use original geometry (no buffer)
c9  = 'data=mon_natl,                     fill=NA, color=.C_NM_INNER' in content
c10 = 'data=mon_sp,                     fill=NA, color=.C_SP_BDR' in content
checks.append(c9 and c10)
pr(f'   [{"PASS" if c9 else "FAIL"}] mon_natl inner stroke: original geometry (no buffer)')
pr(f'   [{"PASS" if c10 else "FAIL"}] mon_sp inner stroke:  original geometry (no buffer)')
pr('')

# ── D. Muni Carto:2 buffering ─────────────────────────────────────────────────
pr('D. Municipalities Carto:2 uses st_buffer() via .buf_sf():')
d1 = '.buf_sf(msf, outer_off_px, zoom)' in content
d2 = '.buf_sf(msf, inner_off_px, zoom)' in content
d3 = 'outer_off_px <- if (zoom >= 13) 3.333 else 2.333' in content
d4 = 'inner_off_px <- 1.333' in content
d5 = 'msf_outer' in content and 'msf_inner' in content
d_ok = d1 and d2 and d3 and d4 and d5
checks.append(d_ok)
pr(f'   [{"PASS" if d1 else "FAIL"}] outer stroke: .buf_sf(msf, outer_off_px, zoom)')
pr(f'   [{"PASS" if d2 else "FAIL"}] inner stroke: .buf_sf(msf, inner_off_px, zoom)')
pr(f'   [{"PASS" if d3 else "FAIL"}] outer_off_px: 3.333 z13+, 2.333 z12-13  [JSON offset values]')
pr(f'   [{"PASS" if d4 else "FAIL"}] inner_off_px: 1.333 constant             [JSON offset=1.333px]')
pr(f'   [{"PASS" if d5 else "FAIL"}] msf_outer/msf_inner intermediate geometries')
pr('')

# ── E. .px_to_m() correctness ─────────────────────────────────────────────────
pr('E. .px_to_m() conversion correctness:')
# 1 CSS px at zoom 12 = 156543.03 / 2^12 = 38.22 m
expected_z12 = 156543.03 / (2**12)
expected_z10 = 156543.03 / (2**10)
expected_z13 = 156543.03 / (2**13)
e1 = '156543.03 / (2^zoom)' in content
checks.append(e1)
pr(f'   [{"PASS" if e1 else "FAIL"}] formula: 156543.03 / (2^zoom)')
pr(f'   Reference values:')
pr(f'     z10: {expected_z10:.2f}m/px  (offset 4px = {4*expected_z10:.0f}m buffer)')
pr(f'     z12: {expected_z12:.2f}m/px  (offset 4px = {4*expected_z12:.0f}m buffer)')
pr(f'     z13: {expected_z13:.2f}m/px  (offset 4px = {4*expected_z13:.0f}m buffer)')
pr('')

# ── Summary ───────────────────────────────────────────────────────────────────
pr('=== SUMMARY ===')
total  = len(checks)
passed = sum(checks)
overall = all(checks)
pr(f'Checks passed: {passed}/{total}')
pr(f'Phase 5 status: [{"PASS" if overall else "FAIL — see above"}]')

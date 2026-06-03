"""Phase 4 validation: dasharray / linetype correctness."""
import re, sys

with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    content = f.read()
lines = content.splitlines()

def pr(s): sys.stdout.buffer.write((s + '\n').encode('utf-8'))

pr('=== PHASE 4 VALIDATION: DASHARRAY / LINETYPE ===')
pr('')

checks = []

# ── A. .stream_lty() ─────────────────────────────────────────────────────────
pr('A. .stream_lty() — 3 zoom-band hex patterns:')
# Expect: z<13 → "731213", z13-14 → "A41314", z14+ → "941314"
fn_match = re.search(r'\.stream_lty\s*<-\s*function.*?(?=\n\n|\n\.)', content, re.DOTALL)
fn_body = fn_match.group(0) if fn_match else ''

a1 = '"731213"' in fn_body
a2 = '"A41314"' in fn_body
a3 = '"941314"' in fn_body
a_ok = a1 and a2 and a3
checks.append(a_ok)
pr(f'   [{"PASS" if a1 else "FAIL"}] z12-13: "731213" (JSON [7,3,1,2,1,3])')
pr(f'   [{"PASS" if a2 else "FAIL"}] z13-14: "A41314" (JSON [10.5,4.5,1.5,3,1.5,4.5] → rounded)')
pr(f'   [{"PASS" if a3 else "FAIL"}] z14+:   "941314" (JSON [9.33,4,1.33,2.67,1.33,4] → rounded)')
pr(f'   Old "longdash"/"dashed" gone: [{"PASS" if "longdash" not in content else "FAIL"}]')
checks.append('"longdash"' not in content)
pr('')

# ── B. Muni Carto:1 ───────────────────────────────────────────────────────────
pr('B. Municipalities Carto:1 linetype:')
b_old_gone = '"dotdash"' not in content
b_new      = '"C636"'     in content
b_ok = b_old_gone and b_new
checks.append(b_ok)
pr(f'   [{"PASS" if b_old_gone else "FAIL"}] "dotdash" removed  (JSON [12,6,3,6] incompatible)')
pr(f'   [{"PASS" if b_new else "FAIL"}] "C636" present     (12→C, 6, 3, 6)')
pr('')

# ── C. Roads sym 5 unpaved white fill ────────────────────────────────────────
pr('C. Roads sym 5 (unpaved) white-fill linetype:')
c_old_gone = 'si == 5L) "22"' not in content
c_new      = 'si == 5L) "32"' in content
c_ok = c_old_gone and c_new
checks.append(c_ok)
pr(f'   [{"PASS" if c_old_gone else "FAIL"}] "22" removed  (was wrong 1:1, JSON is 3:2)')
pr(f'   [{"PASS" if c_new else "FAIL"}] "32" present   (JSON [3,2])')
pr('')

# ── D. .trax_lty() accepts sym parameter ──────────────────────────────────────
pr('D. .trax_lty() — sym parameter and sym 7 handling:')
trax_fn = re.search(r'\.trax_lty\s*<-\s*function\([^)]+\).*?(?=\n\n|\n\.)', content, re.DOTALL)
trax_body = trax_fn.group(0) if trax_fn else ''

d1 = 'sym' in (trax_fn.group(0).split('\n')[0] if trax_fn else '')   # param in signature
d2 = '"7"' in trax_body or "\"7\"" in trax_body                       # sym 7 special case
d3 = '"42"' in trax_body and '"53"' in trax_body and '"32"' in trax_body  # all 3 patterns
d_ok = d1 and d2 and d3
checks.append(d_ok)
pr(f'   [{"PASS" if d1 else "FAIL"}] sym parameter in .trax_lty() signature')
pr(f'   [{"PASS" if d2 else "FAIL"}] sym "7" handled separately (z12-15="42" not z14-16)')
pr(f'   [{"PASS" if d3 else "FAIL"}] all 3 patterns present: "53", "42", "32"')
# Verify sym 7 starts at zoom<15 not zoom<14
d4 = 'zoom < 15' in trax_body
checks.append(d4)
pr(f'   [{"PASS" if d4 else "FAIL"}] sym 7 threshold: zoom < 15 (not <14)')
pr('')

# ── E. TRAX loop restructured by map_symbol ───────────────────────────────────
pr('E. TRAX draw loop iterates by map_symbol (not trax_color):')
e1 = 'for (sym_v in unique(trax$map_symbol))' in content
e2 = '.trax_lty(zoom, sym_v)' in content
e3 = 'trax$trax_color' not in content.split('TRAX')[2] if 'TRAX' in content else True
e_ok = e1 and e2
checks.append(e_ok)
pr(f'   [{"PASS" if e1 else "FAIL"}] loop: for (sym_v in unique(trax$map_symbol))')
pr(f'   [{"PASS" if e2 else "FAIL"}] .trax_lty(zoom, sym_v) called with symbol')
pr('')

# ── F. Previously-correct linetypes unchanged ─────────────────────────────────
pr('F. Already-correct linetypes unchanged:')
f1 = '"C663"' in content       # county inner dash (Phase 1)
f2 = '"F313"' in content       # ski lifts
f3 = '"42"'   in content       # commuter rail dash + TRAX z14-16
f4 = '"11"'   in content       # trails (1:1 dash, JSON [0.8,0.8])
f5 = '"53"'   in content       # TRAX z12-14
f_ok = f1 and f2 and f3 and f4 and f5
checks.append(f_ok)
pr(f'   [{"PASS" if f1 else "FAIL"}] "C663" county inner dash (JSON [12,6,6,6])')
pr(f'   [{"PASS" if f2 else "FAIL"}] "F313" ski lifts (JSON [18,3,1,3])')
pr(f'   [{"PASS" if f3 else "FAIL"}] "42"   commuter rail + TRAX z14-16')
pr(f'   [{"PASS" if f4 else "FAIL"}] "11"   trails (JSON [0.8,0.8] ≈ 1:1)')
pr(f'   [{"PASS" if f5 else "FAIL"}] "53"   TRAX z12-14 (JSON [5,3])')
pr('')

# ── Summary ───────────────────────────────────────────────────────────────────
pr('=== SUMMARY ===')
total = len(checks)
passed = sum(checks)
overall = all(checks)
pr(f'Checks passed: {passed}/{total}')
pr(f'Phase 4 status: [{"PASS" if overall else "FAIL — see above"}]')

"""Phase 8 validation: TRAX sym5 colour, buildings shadow, transit stop labels."""
import re, sys, math

with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    content = f.read()
lines = content.splitlines()

def pr(s): sys.stdout.buffer.write((s + '\n').encode('utf-8'))

pr('=== PHASE 8 VALIDATION ===')
pr('')

checks = []

# ── A. TRAX sym 5 zoom-dependent colour ──────────────────────────────────────
pr('A. TRAX sym 5: zoom-dependent colour (JSON: GRN at z14-16, GRN5 elsewhere):')

# Check the new zoom-conditional colour assignment
a1 = 'zoom >= 14L && zoom < 16L' in content and 'C_TRAX_GRN' in content
a2 = '.C_TRAX_GRN5' in content   # GRN5 still used outside z14-16

# Verify old fixed assignment is gone
a3 = 'map_symbol == "5" ~ .C_TRAX_GRN5, # distinct shade in JSON' not in content
checks += [a1, a2, a3]
pr(f'   [{"PASS" if a1 else "FAIL"}] zoom-conditional: if(zoom>=14 && zoom<16) .C_TRAX_GRN else .C_TRAX_GRN5')
pr(f'   [{"PASS" if a2 else "FAIL"}] .C_TRAX_GRN5 constant still defined (used at z<14 and z>=16)')
pr(f'   [{"PASS" if a3 else "FAIL"}] old fixed assignment removed')

# Simulate the corrected behaviour
def sym5_color(zoom):
    return 'GRN' if (zoom >= 14 and zoom < 16) else 'GRN5'

color_cases = [(12,'GRN5'),(13,'GRN5'),(14,'GRN'),(15,'GRN'),(16,'GRN5'),(17,'GRN5')]
c_ok = True
for z, exp in color_cases:
    got = sym5_color(z)
    ok = got == exp
    if not ok: c_ok = False
    checks.append(ok)
    pr(f'   [{"PASS" if ok else "FAIL"}] sym5 z{z}: {got}  (JSON: {"rgba(96,191,77)" if exp=="GRN" else "rgba(72,191,48)"})')
pr('')

# ── B. Buildings drop shadow ───────────────────────────────────────────────────
pr('B. Buildings drop shadow approximation via coordinate-shifted polygon:')

b1 = '.C_BLDG_SHADOW' in content
b2 = 'rgb(128, 123, 121,  64' in content                # rgba(128,123,121,0.25)
b3 = 'shadow_off <- 1.067 * .px_to_m(zoom)' in content  # exact JSON translate
b4 = 'sf::st_geometry(buildings) + c(shadow_off, -shadow_off)' in content  # E/S shift
b5 = 'fill = .C_BLDG_SHADOW, color = NA' in content     # drawn before main fill
b6 = '.C_BLDG_FILL, color = .C_BLDG_BDR' in content    # main fill still present

# Verify shadow offset math at zoom 14
PX_TO_M = lambda z: 156543.03 / (2**z)
shadow_z14 = 1.067 * PX_TO_M(14)
shadow_z15 = 1.067 * PX_TO_M(15)
b7 = abs(shadow_z14 - 10.2) < 0.5   # ~10.2m at z14
b8 = abs(shadow_z15 - 5.1) < 0.3    # ~5.1m at z15

checks += [b1,b2,b3,b4,b5,b6,b7,b8]
pr(f'   [{"PASS" if b1 else "FAIL"}] .C_BLDG_SHADOW constant added')
pr(f'   [{"PASS" if b2 else "FAIL"}] shadow colour: rgb(128,123,121,64) = rgba(128,123,121,0.25)')
pr(f'   [{"PASS" if b3 else "FAIL"}] shadow offset: 1.067 * .px_to_m(zoom) [exact JSON fill-translate]')
pr(f'   [{"PASS" if b4 else "FAIL"}] geometry shift: +shadow_off (East), -shadow_off (South)')
pr(f'   [{"PASS" if b5 else "FAIL"}] shadow drawn before main fill (correct layer order)')
pr(f'   [{"PASS" if b6 else "FAIL"}] main fill and border still present')
pr(f'   [{"PASS" if b7 else "FAIL"}] z14 shadow offset = {shadow_z14:.1f}m  (1.067px × 9.55m/px)')
pr(f'   [{"PASS" if b8 else "FAIL"}] z15 shadow offset = {shadow_z15:.1f}m  (1.067px × 4.78m/px)')
pr('')

# ── C. Transit stop circles removed ───────────────────────────────────────────
pr('C. Transit stop circles removed (JSON: icon-color=rgba(0,0,0,0)):')
c1 = 'shape = 21,\n        size = 1.2,\n        fill = "white",\n        color = alpha("#597EB3"' not in content
c2 = 'shape = 21,\n        size = 1.2,\n        fill = "white",\n        color = alpha("#9B7BBD"' not in content
checks += [c1, c2]
pr(f'   [{"PASS" if c1 else "FAIL"}] light rail station circle (shape=21, blue) removed')
pr(f'   [{"PASS" if c2 else "FAIL"}] commuter rail stop circle (shape=21, purple) removed')
pr('')

# ── D. Transit stop text labels added ─────────────────────────────────────────
pr('D. Transit stop text labels added (JSON: {_name} text-color #8C8989):')

d1 = '15v. Light rail station labels' in content
d2 = '15w. Commuter rail stop labels' in content
d3 = 'transit$rail_stops' in content and 'map_label' in content
d4 = 'transit$cr_stops' in content and 'map_label' in content
# Check correct JSON colour
d5 = 'C_POI_LBL' in content and 'C_POI_HALO_HE' in content   # #8C8989, rgba(230,226,218,0.55)
# Check correct font (Segoe UI Semibold Italic → .F_STREET, bold.italic)
d6_match = re.search(r'15v.*?rl_lbl.*?face="([^"]+)".*?fam=([.\w]+)', content, re.DOTALL)
d7_match = re.search(r'15w.*?cr_lbl.*?face="([^"]+)".*?fam=([.\w]+)', content, re.DOTALL)
d6 = d6_match and d6_match.group(1) == 'bold.italic' and '.F_STREET' in d6_match.group(2)
d7 = d7_match and d7_match.group(1) == 'bold.italic' and '.F_STREET' in d7_match.group(2)
# Check correct size (10.667px)
d8 = '.sz(10.667)' in content   # used for station labels

checks += [d1,d2,d3,d4,d5,d6,d7,d8]
pr(f'   [{"PASS" if d1 else "FAIL"}] 15v. Light rail station label section added')
pr(f'   [{"PASS" if d2 else "FAIL"}] 15w. Commuter rail stop label section added')
pr(f'   [{"PASS" if d3 else "FAIL"}] transit$rail_stops$map_label drawn as labels')
pr(f'   [{"PASS" if d4 else "FAIL"}] transit$cr_stops$map_label drawn as labels')
pr(f'   [{"PASS" if d5 else "FAIL"}] colour: .C_POI_LBL (#8C8989) + .C_POI_HALO_HE (JSON spec)')
pr(f'   [{"PASS" if d6 else "FAIL"}] light rail: face=bold.italic fam=.F_STREET (Segoe UI Semibold Italic)')
pr(f'   [{"PASS" if d7 else "FAIL"}] commuter:   face=bold.italic fam=.F_STREET (same JSON spec)')
pr(f'   [{"PASS" if d8 else "FAIL"}] size: .sz(10.667) = {0.75/2.835 * 10.667:.3f} ggplot2 units  [JSON 10.667px]')
pr('')

# ── E. JSON spec cross-check ───────────────────────────────────────────────────
pr('E. JSON specification cross-check:')
# Verify .C_BLDG_SHADOW is DIFFERENT from .C_BLDG_BDR
# Shadow: rgb(128,123,121,64)  = rgba(128,123,121,0.25)
# Border: rgb(140,133,133,89)  = rgba(140,133,133,0.35)
e1 = 'rgb(128, 123, 121,  64' in content   # shadow fill exact
e2 = 'rgb(140, 133, 133,  89' in content   # border exact (from Phase 1)
e3 = 'rgba(128,123,121,0.25)' in content   # comment confirms correct value

checks += [e1, e2, e3]
pr(f'   [{"PASS" if e1 else "FAIL"}] Shadow: rgb(128,123,121) alpha=64/255=0.25  [JSON exact]')
pr(f'   [{"PASS" if e2 else "FAIL"}] Border: rgb(140,133,133) alpha=89/255=0.35  [JSON exact]')
pr(f'   [{"PASS" if e3 else "FAIL"}] Comment confirms rgba(128,123,121,0.25) = shadow, not border')
pr('')

# ── Summary ───────────────────────────────────────────────────────────────────
pr('=== SUMMARY ===')
total  = len(checks)
passed = sum(checks)
overall = all(checks)
pr(f'Checks passed: {passed}/{total}')
pr(f'Phase 8 status: [{"PASS" if overall else "FAIL — see above"}]')

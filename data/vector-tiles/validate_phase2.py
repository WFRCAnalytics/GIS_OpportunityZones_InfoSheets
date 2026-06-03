import re, sys

with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    content = f.read()
lines = content.splitlines()

def pr(s):
    sys.stdout.buffer.write((s + '\n').encode('utf-8'))

def extract_lbl_call(content, start_idx, search_window=3000):
    """Find the first .lbl( call after start_idx and return the text inside it."""
    search = content[start_idx:start_idx + search_window]
    lbl_pos = search.find('.lbl(')
    if lbl_pos == -1:
        return None
    # Now find the matching closing ) by tracking depth
    open_pos = lbl_pos + len('.lbl(')
    depth = 1
    i = open_pos
    while i < len(search) and depth > 0:
        if search[i] == '(':
            depth += 1
        elif search[i] == ')':
            depth -= 1
        i += 1
    return search[open_pos:i - 1]

pr('=== PHASE 2 FULL VALIDATION ===')
pr('')

# 1. System font tryCatch calls
sys_calls = [l for l in lines if 'tryCatch' in l and 'font_add' in l]
pr(f'1. System tryCatch font_add: {len(sys_calls)} remaining  [{"PASS" if not sys_calls else "FAIL"}]')
pr('')

# 2. Google Fonts loaded (whitespace-tolerant)
pr('2. Google Fonts via font_add_google():')
gf_expected = [
    ('Poller One',    'poller',   'Poller One Regular (exact)'),
    ('Noto Sans JP',  'noto_jp',  'Yu Gothic Bold substitute'),
    ('Arimo',         'arimo',    'Arial/Helvetica substitute'),
    ('Source Sans 3', 'source3',  'Segoe UI Semibold substitute'),
    ('Bitter',        'bitter',   'Verdana Bold Italic substitute'),
    ('Cinzel',        'cinzel',   'Copperplate33bc substitute'),
]
all_gf = True
for name, alias, note in gf_expected:
    pat = rf'font_add_google\(\s*"{re.escape(name)}"\s*,\s*"{re.escape(alias)}"\s*\)'
    found = bool(re.search(pat, content))
    status = 'PASS' if found else 'FAIL'
    if not found:
        all_gf = False
    pr(f'   [{status}] font_add_google("{name}", "{alias}")  # {note}')
pr(f'   Overall: [{"PASS" if all_gf else "FAIL"}]')
pr('')

# 3. Font constants
pr('3. Font family constants:')
fc_expected = [
    ('.F_COUNTY', 'poller'),
    ('.F_CITY',   'noto_jp'),
    ('.F_HWY',    'arimo'),
    ('.F_STREET', 'source3'),
    ('.F_WATER',  'bitter'),
    ('.F_MUNI',   'cinzel'),
]
all_fc = True
for const, alias in fc_expected:
    pat = rf'^\s*{re.escape(const)}\s*<-\s*"{re.escape(alias)}"'
    found = any(re.match(pat, l) for l in lines)
    status = 'PASS' if found else 'FAIL'
    if not found:
        all_fc = False
    pr(f'   [{status}] {const} <- "{alias}"')
pr(f'   Overall: [{"PASS" if all_fc else "FAIL"}]')
pr('')

# 4. Old fallback checks
old = [l for l in lines if 'font_families()' in l]
pr(f'4. sysfonts::font_families() checks: {len(old)} (expect 0)  [{"PASS" if not old else "FAIL"}]')
pr('')

# 5. fam= distribution
pr('5. fam= distribution in .lbl() calls:')
fam_counts = {}
for l in lines:
    m = re.search(r'fam\s*=\s*([\.\w"]+)', l)
    if m:
        val = m.group(1)
        if val.startswith('.F_'):
            fam_counts[val] = fam_counts.get(val, 0) + 1
for k in sorted(fam_counts):
    pr(f'   {k}: {fam_counts[k]} uses')
pr('')

# 6. Per-section audit — extract ONLY the specific .lbl() call for each section
pr('6. Per-label-section font/face audit (exact .lbl() call extraction):')
checks = [
    ('15a. Great Salt Lake', '.F_WATER',  'bold.italic', 'Verdana Bold Italic'),
    ('15b. Lake labels',     '.F_WATER',  'bold.italic', 'Verdana Bold Italic'),
    ('15c. Stream labels',   '.F_WATER',  'bold.italic', 'Verdana Bold Italic'),
    ('15d. Monument',        '.F_MUNI',   'plain',       'Copperplate33bc Regular'),
    ('15e. Park labels',     '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15f. Golf',            '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15g. Cemetery',        '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15h. Trail labels',    '.F_STREET', 'bold',        'Segoe UI Semibold (not italic)'),
    ('15i. Ski area',        '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15j. Contour',         '.F_HWY',    'bold.italic', 'Arial Bold Italic'),
    ('15k. Ski lift',        '.F_HWY',    'italic',      'Arial Italic'),
    ('15l. GNIS bay',        '.F_WATER',  'bold.italic', 'Verdana Bold Italic'),
    ('15m. GNIS place',      '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15n. OpenSource',      '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15o. Ski area loc',    '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15p. Trailhead',       '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15q. Higher ed',       '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15r. K-12',            '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15s. Healthcare',      '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15t. Airport',         '.F_STREET', 'bold.italic', 'Segoe UI Semibold Italic'),
    ('15. County labels',    '.F_COUNTY', 'bold',        'Poller One Regular'),
    ('16. City',             '.F_CITY',   'bold',        'Yu Gothic Bold'),
    ('17. Highway labels',   '.F_HWY',    'bold',        'Arial Bold / Helvetica Bold'),
    ('18. Street labels',    '.F_STREET', 'bold',        'Segoe UI Semibold'),
]

all_sec = True
pass_cnt = 0
for comment, exp_fam, exp_face, json_font in checks:
    idx = content.find(comment)
    if idx == -1:
        pr(f'   [FAIL] comment not found: "{comment}"')
        all_sec = False
        continue

    # Extract just the specific .lbl() call after this comment
    call_body = extract_lbl_call(content, idx, search_window=3000)
    if call_body is None:
        pr(f'   [FAIL] no .lbl() found after: "{comment}"')
        all_sec = False
        continue

    fam_m  = re.findall(r'fam\s*=\s*([\.\w]+)',   call_body)
    face_m = re.findall(r'face\s*=\s*"([^"]+)"', call_body)
    got_fam  = fam_m[-1]  if fam_m  else 'MISSING'
    got_face = face_m[-1] if face_m else 'MISSING'

    ok_fam  = got_fam  == exp_fam
    ok_face = got_face == exp_face
    status  = 'PASS' if (ok_fam and ok_face) else 'FAIL'
    if not (ok_fam and ok_face):
        all_sec = False
    else:
        pass_cnt += 1

    fam_note  = '' if ok_fam  else f' [GOT {got_fam}, WANT {exp_fam}]'
    face_note = '' if ok_face else f' [GOT {got_face}, WANT {exp_face}]'
    pr(f'   [{status}] {comment:<28} fam={got_fam}{fam_note}  face={got_face}{face_note}')

pr('')
pr(f'   Section audit: {pass_cnt}/{len(checks)} passed  [{"PASS" if all_sec else "FAIL"}]')
pr('')
pr('=== SUMMARY ===')
overall = (not sys_calls) and all_gf and all_fc and (not old) and all_sec
pr(f'Phase 2 status: [{"PASS" if overall else "FAIL — see details above"}]')

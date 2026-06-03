"""
Patch all .lbl() calls to add bgr= argument.
Strategy: find each .lbl( call, locate the sz= argument value, insert bgr= after sz=.
halo_px is 1.333 for all labels EXCEPT county class 3 (1.0) and highway (1.333).
"""
import re

with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    content = f.read()

# We need to insert bgr= after the sz= argument in each .lbl() call.
# Pattern: sz = <expr>,
# We'll match the sz= line and insert bgr = .bgr(<halo_px>, <sz_expr>), after it.

# For most labels halo_px = 1.333
# For county class 3 halo_px = 1.0 (handled specially in the county loop)
# We'll use 1.333 for all and fix county cls3 separately.

# The sz= values come in several forms:
#   sz = some_value,          (e.g. sz = sz_lake,)
#   sz = .sz_park_lbl(zoom),
#   sz = .sz_county(ci),
# We need to capture the sz expression and generate bgr = .bgr(1.333, <sz_expr>)

def add_bgr(match):
    sz_expr = match.group(1).rstrip(',').strip()
    indent  = match.group(2)
    return f'sz = {sz_expr},\n{indent}bgr = .bgr(1.333, {sz_expr}),'

# Pattern: "sz = <expr>," on a line, followed by possible trailing comma
# We want to capture sz_expr and the leading whitespace of that line
pattern = r'(sz = ([^,\n]+)),(\n(\s+))'

def replacer(m):
    sz_expr = m.group(2).strip()
    before_newline = m.group(1)
    newline_and_indent = m.group(3)
    indent = m.group(4)
    return f'{before_newline},\n{indent}bgr = .bgr(1.333, {sz_expr}),\n{indent}'

new_content = re.sub(
    r'(sz\s*=\s*([^,\n]+)),\n(\s+)(bgr|face)',
    lambda m: m.group(0),  # skip if bgr already present
    content
)

# Only add bgr= where it's not already present
# Match: sz = <expr>, followed by newline+indent+something-that-is-NOT-bgr
pattern2 = r'(sz\s*=\s*([^\n,]+)),\n(\s+)(?!bgr)'

def add_bgr2(m):
    sz_full = m.group(1)   # e.g. "sz = sz_lake"
    sz_expr = m.group(2).strip()  # e.g. "sz_lake"
    indent  = m.group(3)
    return f'{sz_full},\n{indent}bgr = .bgr(1.333, {sz_expr}),\n{indent}'

result = re.sub(pattern2, add_bgr2, content)

# Count changes
orig_count = content.count('bgr =')
new_count  = result.count('bgr =')
added      = new_count - orig_count

with open('ugrc_basemap.R', 'w', encoding='utf-8') as f:
    f.write(result)

print(f'bgr= arguments added: {added}')
print(f'Total bgr= in file: {new_count}')

# Now fix county cls3 which needs halo_px=1.0 not 1.333
# County cls3 label is drawn inside the for(ci in ...) loop
# The cls3 check: if (ci >= 3L) -> lbl_col = .C_CNTY_LO
# We need to make bgr depend on ci:
# Replace: bgr = .bgr(1.333, .sz_county(ci)),
# With:    bgr = .bgr(if (ci >= 3L) 1.0 else 1.333, .sz_county(ci)),
with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    result2 = f.read()

old_county_bgr = 'bgr = .bgr(1.333, .sz_county(ci)),'
new_county_bgr = 'bgr = .bgr(if (ci >= 3L) 1.0 else 1.333, .sz_county(ci)),  # cls3 halo=1.0px, cls1-2 halo=1.333px'

if old_county_bgr in result2:
    result2 = result2.replace(old_county_bgr, new_county_bgr, 1)
    print('Fixed county cls3 halo_px to 1.0')
else:
    print('WARNING: county cls3 halo_px pattern not found')

with open('ugrc_basemap.R', 'w', encoding='utf-8') as f:
    f.write(result2)

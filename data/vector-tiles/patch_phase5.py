"""Phase 5: replace monument and muni draw sections with st_buffer() versions."""
import re

with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    content = f.read()

# ── Monument section ──────────────────────────────────────────────────────────
# Find from "# National Monuments" to just before "if (nrow(rec$ski)"
mon_start = content.find('  # National Monuments (sym 0)')
mon_end   = content.find('  if (nrow(rec$ski) > 0)')
assert mon_start != -1, 'monument start not found'
assert mon_end   != -1, 'monument end not found'

mon_new = """  # National Monuments (sym 0) — .buf_sf() approximates JSON line-offset.
  # Buffering the polygon outward by offset_px CSS pixels, then drawing its
  # border, replicates the visual halo that line-offset creates in Mapbox GL.
  if (nrow(mon_natl) > 0) {
    if (zoom >= 13) {
      # z13+: outer offset=4px, mid offset=1.333px, inner on boundary
      p <- p + ggplot2::geom_sf(data=.buf_sf(mon_natl,4.0,zoom),   fill=NA, color=.C_NM_OUTER, linewidth=.lw(8.0))
      p <- p + ggplot2::geom_sf(data=.buf_sf(mon_natl,1.333,zoom), fill=NA, color=.C_NM_MID,   linewidth=.lw(4.0))
      p <- p + ggplot2::geom_sf(data=mon_natl,                     fill=NA, color=.C_NM_INNER, linewidth=.lw(0.667))
    } else if (zoom >= 10) {
      # z10-13: outer offset=2.667px, inner on boundary
      p <- p + ggplot2::geom_sf(data=.buf_sf(mon_natl,2.667,zoom), fill=NA, color="#C7C0BD",   linewidth=.lw(5.333))
      p <- p + ggplot2::geom_sf(data=mon_natl,                     fill=NA, color=.C_NM_INNER, linewidth=.lw(0.667))
    } else {
      # z8-10: outer offset=1.667px, inner on boundary
      p <- p + ggplot2::geom_sf(data=.buf_sf(mon_natl,1.667,zoom), fill=NA, color="#CCC5C2",   linewidth=.lw(3.333))
      p <- p + ggplot2::geom_sf(data=mon_natl,                     fill=NA, color=.C_NM_INNER, linewidth=.lw(0.667))
    }
  }
  # State Parks (sym 1) — same st_buffer() strategy
  if (nrow(mon_sp) > 0) {
    if (zoom >= 13) {
      # z13+: outer offset=4px, mid offset=1.333px, inner on boundary
      p <- p + ggplot2::geom_sf(data=.buf_sf(mon_sp,4.0,zoom),   fill=NA, color=.C_SP_OUTER, linewidth=.lw(8.0))
      p <- p + ggplot2::geom_sf(data=.buf_sf(mon_sp,1.333,zoom), fill=NA, color=.C_SP_MID,   linewidth=.lw(2.667))
      p <- p + ggplot2::geom_sf(data=mon_sp,                     fill=NA, color=.C_SP_BDR,   linewidth=.lw(1.0))
    } else if (zoom >= 11) {
      # z11-13: outer offset=2px, inner on boundary
      p <- p + ggplot2::geom_sf(data=.buf_sf(mon_sp,2.0,zoom), fill=NA, color=.C_SP_MID,   linewidth=.lw(4.0))
      p <- p + ggplot2::geom_sf(data=mon_sp,                   fill=NA, color=.C_NM_INNER, linewidth=.lw(0.667))
    } else {
      # z8-11: outer offset=1.667px, inner on boundary
      p <- p + ggplot2::geom_sf(data=.buf_sf(mon_sp,1.667,zoom), fill=NA, color=.C_SP_MID,   linewidth=.lw(3.333))
      p <- p + ggplot2::geom_sf(data=mon_sp,                     fill=NA, color=.C_SP_INNER, linewidth=.lw(0.667))
    }
  }
"""

content = content[:mon_start] + mon_new + content[mon_end:]
print('Monument section replaced')

# ── Muni Carto:2 section ──────────────────────────────────────────────────────
# Find the Carto:2 for loop block
muni_start = content.find('  # 7. Municipalities Carto:2')
muni_end   = content.find('\n  # 8. Municipalities Carto:1')
assert muni_start != -1, 'muni2 start not found'
assert muni_end   != -1, 'muni2 end not found'

muni_new = """  # 7. Municipalities Carto:2 — .buf_sf() approximates JSON line-offset halos.
  # JSON uses offset strokes (outer 2.333-3.333px, inner 1.333px outward).
  # Buffering the polygon by the offset then drawing its border replicates this.
  # sym 0 outer colour changes at z15 (slightly different purple shade in JSON).
  .m2_out0 <- if (zoom >= 15) .C_M2_OUT[["0"]]
              else rgb(160, 134, 179, 38, maxColorValue=255)  # rgba(160,134,179,0.15)
  outer_off_px <- if (zoom >= 13) 3.333 else 2.333   # JSON z12-13=2.333, z13+=3.333
  inner_off_px <- 1.333                               # constant across all zooms
  if (nrow(muni$muni2) > 0) {
    for (sv in c("0", "1", "2", "3")) {
      msf <- muni$muni2[!is.na(muni$muni2$map_symbol) & muni$muni2$map_symbol == sv, ]
      if (nrow(msf) == 0) next
      out_col <- if (sv == "0") .m2_out0 else .C_M2_OUT[[sv]]
      msf_outer <- .buf_sf(msf, outer_off_px, zoom)
      msf_inner <- .buf_sf(msf, inner_off_px, zoom)
      p <- p + ggplot2::geom_sf(data=msf,       fill=.C_M2_FILL, color=NA)
      p <- p + ggplot2::geom_sf(data=msf_outer, fill=NA, color=out_col,        linewidth=m2_ow)
      p <- p + ggplot2::geom_sf(data=msf_inner, fill=NA, color=.C_M2_IN[[sv]], linewidth=m2_iw)
    }
  }
"""

content = content[:muni_start] + muni_new + content[muni_end+1:]  # +1 skip the leading \n
print('Muni Carto:2 section replaced')

with open('ugrc_basemap.R', 'w', encoding='utf-8') as f:
    f.write(content)

print('Done')

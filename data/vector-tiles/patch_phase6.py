"""
Phase 6: symbol-placement:line
1. Add .line_label_pts() helper after .buf_sf()
2. Add WesternStates/label and west_lbl colour/size constants
3. Add west_lbl to .fetch_labels()
4. Replace extract_label_coords() with .line_label_pts() for all line-placed layers
5. Add Western States label draw section
"""
import re

with open('ugrc_basemap.R', 'r', encoding='utf-8') as f:
    content = f.read()

# ── 1. Add .line_label_pts() after .buf_sf() ─────────────────────────────────
new_fn = '''
# Sample repeated label positions along line geometries to replicate
# Mapbox GL symbol-placement:line with symbol-spacing.
# spacing_px: JSON symbol-spacing value; zoom: current tile zoom level.
# Returns a new sf with POINT geometry, one row per label position.
.line_label_pts <- function(sf_lines, spacing_px, zoom) {
  if (nrow(sf_lines) == 0) return(sf_lines)
  spacing_m <- spacing_px * .px_to_m(zoom)
  crs_out   <- sf::st_crs(sf_lines)

  result <- lapply(seq_len(nrow(sf_lines)), function(i) {
    row  <- sf_lines[i, , drop = FALSE]
    geom <- sf::st_geometry(row)[[1L]]

    # Ensure LINESTRING — cast MULTILINESTRING to its first sub-geometry
    if (inherits(geom, "MULTILINESTRING")) {
      sub <- sf::st_cast(sf::st_sfc(geom, crs = crs_out), "LINESTRING")
      if (length(sub) == 0L) return(NULL)
      geom <- sub[[1L]]
    }
    if (!inherits(geom, "LINESTRING")) return(NULL)

    coords <- tryCatch(sf::st_coordinates(sf::st_sfc(geom, crs = crs_out)),
                       error = function(e) NULL)
    if (is.null(coords) || nrow(coords) < 2L) return(NULL)

    # Cumulative arc length in map units
    segs  <- sqrt(diff(coords[, 1L])^2 + diff(coords[, 2L])^2)
    cdist <- c(0, cumsum(segs))
    total <- cdist[length(cdist)]
    if (total <= 0) return(NULL)

    # Decide where to place labels
    targets <- if (total < spacing_m / 2) {
      total / 2                              # too short: one label at midpoint
    } else {
      seq(spacing_m / 2, total, by = spacing_m)
    }

    # Interpolate a POINT at each target distance
    pts <- lapply(targets, function(d) {
      seg <- max(1L, findInterval(d, cdist, rightmost.closed = TRUE))
      seg <- min(seg, nrow(coords) - 1L)
      t   <- if (segs[seg] > 1e-9) (d - cdist[seg]) / segs[seg] else 0
      x   <- coords[seg, 1L] + t * (coords[seg + 1L, 1L] - coords[seg, 1L])
      y   <- coords[seg, 2L] + t * (coords[seg + 1L, 2L] - coords[seg, 2L])
      pt_row          <- row
      pt_row$geometry <- sf::st_sfc(sf::st_point(c(x, y)), crs = crs_out)
      pt_row
    })
    pts <- Filter(Negate(is.null), pts)
    if (length(pts) == 0L) return(NULL)
    do.call(rbind, pts)
  })

  result <- Filter(Negate(is.null), result)
  if (length(result) == 0L) return(sf_lines[0L, ])
  do.call(rbind, result)
}

'''

insert_after = '.buf_sf <- function(sf_obj, offset_px, zoom) {\n  d <- offset_px * .px_to_m(zoom)\n  tryCatch(sf::st_buffer(sf_obj, dist = d), error = function(e) sf_obj)\n}'
idx = content.find(insert_after)
assert idx != -1, 'buf_sf not found'
end = idx + len(insert_after)
content = content[:end] + new_fn + content[end:]
print('Added .line_label_pts()')

# ── 2. Add Western States label colour constant after .C_GNIS_HALO ────────────
old_c = '.C_GNIS_HALO <- rgb(230, 228, 225, 140, maxColorValue = 255) # rgba(230,228,225,0.55)'
new_c = old_c + '''
.C_WEST_STATE     <- "#828282"                                       # WesternStates/label text
.C_WEST_STATE_HALO <- rgb(242, 240, 237, 128, maxColorValue = 255)  # light halo (no explicit halo in JSON)'''
content = content.replace(old_c, new_c, 1)
print('Added Western States colour constants')

# ── 3. Add west_lbl to .fetch_labels() ───────────────────────────────────────
old_fetch_end = '''    ski_lift_lbl = if (zoom >= 12) safe_read_mvt(bu, "SkiLifts/label")                                  else .empty_sf(),
    ctr_lbl      = if (zoom >= 13) safe_read_mvt(bu, "Contours_10MeterDEM_50ft_generalized/label")      else .empty_sf()
  )
}'''
new_fetch_end = '''    ski_lift_lbl = if (zoom >= 12) safe_read_mvt(bu, "SkiLifts/label")                                  else .empty_sf(),
    ctr_lbl      = if (zoom >= 13) safe_read_mvt(bu, "Contours_10MeterDEM_50ft_generalized/label")      else .empty_sf(),
    # WesternStates/label — line-following, z>=8
    west_lbl     = if (zoom >= 8)  safe_read_mvt(bu, "WesternStates/label")                             else .empty_sf()
  )
}'''
content = content.replace(old_fetch_end, new_fetch_end, 1)
print('Added west_lbl to .fetch_labels()')

# ── 4a. GSL label — replace extract_label_coords with .line_label_pts ─────────
old_gsl = '''  # 15a. Great Salt Lake label — italic, water colour, z≥7
  if (nrow(lbl_data$gsl) > 0 && any(!is.na(lbl_data$gsl$map_label))) {
    gsl_lbl <- extract_label_coords(lbl_data$gsl)'''
new_gsl = '''  # 15a. Great Salt Lake label — line-placed, JSON spacing=1000px, z≥7
  if (nrow(lbl_data$gsl) > 0 && any(!is.na(lbl_data$gsl$map_label))) {
    gsl_lbl <- .line_label_pts(lbl_data$gsl, spacing_px = 1000, zoom = zoom)
    gsl_lbl <- extract_label_coords(gsl_lbl)'''
content = content.replace(old_gsl, new_gsl, 1)
print('Updated GSL label to use .line_label_pts()')

# ── 4b. Stream labels — replace extract_label_coords ──────────────────────────
old_stm = '''  # 15c. Stream labels — italic, water colour, major z≥8 down to minor z≥12
  if (nrow(lbl_data$streams) > 0 && any(!is.na(lbl_data$streams$map_label))) {
    stm_lbl <- extract_label_coords(lbl_data$streams)'''
new_stm = '''  # 15c. Stream labels — line-placed, JSON spacing=1000px (1344 for major)
  if (nrow(lbl_data$streams) > 0 && any(!is.na(lbl_data$streams$map_label))) {
    stm_lbl <- .line_label_pts(lbl_data$streams, spacing_px = 1000, zoom = zoom)
    stm_lbl <- extract_label_coords(stm_lbl)'''
content = content.replace(old_stm, new_stm, 1)
print('Updated stream labels to use .line_label_pts()')

# ── 4c. Trail labels — replace extract_label_coords ───────────────────────────
old_trail = '''  # 15h. Trail labels — z≥14 (line-placement not supported; use centroid)
  if (nrow(lbl_data$trails) > 0 && any(!is.na(lbl_data$trails$map_label))) {
    trail_lbl <- extract_label_coords(lbl_data$trails)'''
new_trail = '''  # 15h. Trail labels — line-placed, JSON spacing=1000px, z≥14
  if (nrow(lbl_data$trails) > 0 && any(!is.na(lbl_data$trails$map_label))) {
    trail_lbl <- .line_label_pts(lbl_data$trails, spacing_px = 1000, zoom = zoom)
    trail_lbl <- extract_label_coords(trail_lbl)'''
content = content.replace(old_trail, new_trail, 1)
print('Updated trail labels to use .line_label_pts()')

# ── 4d. Contour labels — replace extract_label_coords ─────────────────────────
old_ctr = '''  # 15j. Contour elevation labels — Arial Bold Italic, z≥13; centroid approx (line-placement N/A)
  if (nrow(lbl_data$ctr_lbl) > 0 && any(!is.na(lbl_data$ctr_lbl$map_label))) {
    ctr_lbl_c <- extract_label_coords(lbl_data$ctr_lbl)'''
new_ctr = '''  # 15j. Contour elevation labels — line-placed, JSON spacing=288px, z≥13
  if (nrow(lbl_data$ctr_lbl) > 0 && any(!is.na(lbl_data$ctr_lbl$map_label))) {
    ctr_lbl_c <- .line_label_pts(lbl_data$ctr_lbl, spacing_px = 288, zoom = zoom)
    ctr_lbl_c <- extract_label_coords(ctr_lbl_c)'''
content = content.replace(old_ctr, new_ctr, 1)
print('Updated contour labels to use .line_label_pts()')

# ── 4e. Ski lift labels — replace extract_label_coords ────────────────────────
old_sk = '''  # 15k. Ski lift labels — #FFFEFA on #B39898 halo, z≥12
  if (
    nrow(lbl_data$ski_lift_lbl) > 0 &&
      any(!is.na(lbl_data$ski_lift_lbl$map_label))
  ) {
    sklift_lbl_c <- extract_label_coords(lbl_data$ski_lift_lbl)'''
new_sk = '''  # 15k. Ski lift labels — line-placed, JSON spacing=1000px, z≥12
  if (
    nrow(lbl_data$ski_lift_lbl) > 0 &&
      any(!is.na(lbl_data$ski_lift_lbl$map_label))
  ) {
    sklift_lbl_c <- .line_label_pts(lbl_data$ski_lift_lbl, spacing_px = 1000, zoom = zoom)
    sklift_lbl_c <- extract_label_coords(sklift_lbl_c)'''
content = content.replace(old_sk, new_sk, 1)
print('Updated ski lift labels to use .line_label_pts()')

# ── 5. Add Western States label section before county labels ──────────────────
# Insert just before "# 15. County labels"
insert_before = '  # 15. County labels — Poller One, light text on gray halo'
west_section = '''  # 15u. Western States label — line-placed, Helvetica Regular → arimo, z≥8
  # JSON: text-color #828282, size 10.667px, spacing 1000px
  if (!is.null(lbl_data$west_lbl) && nrow(lbl_data$west_lbl) > 0 &&
      any(!is.na(lbl_data$west_lbl$map_label))) {
    wst_pts <- .line_label_pts(lbl_data$west_lbl, spacing_px = 1000, zoom = zoom)
    wst_pts <- extract_label_coords(wst_pts)
    if (nrow(wst_pts) > 0 && any(!is.na(wst_pts$map_label))) {
      sz_wst <- .sz(10.667)
      p <- .lbl(p, wst_pts, col=.C_WEST_STATE, halo=.C_WEST_STATE_HALO,
                 sz=sz_wst, bgr=.bgr(1.333, sz_wst), face="plain", fam=.F_HWY)
    }
  }

  '''
content = content.replace(insert_before, west_section + insert_before, 1)
print('Added Western States label draw section')

with open('ugrc_basemap.R', 'w', encoding='utf-8') as f:
    f.write(content)
print('Done')

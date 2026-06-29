# ==== filter_export_to_garden.R ====
# Dependencies: sf, rprojroot (for stable location at app root directory)
library(sf)
library(rprojroot)

# <U+2014><U+2014> Optional: specify minimum area (m<U+00B2>) via command line, default 5000 m<U+00B2> <U+2014><U+2014> #
args <- commandArgs(trailingOnly = TRUE)
min_area_m2 <- if (length(args) >= 1 && !is.na(suppressWarnings(as.numeric(args[1])))) {
  as.numeric(args[1])
} else {
  20000
}

# 1) Locate paths
base_dir <- tryCatch(
  rprojroot::find_root(rprojroot::has_file("app.R")),
  error = function(e) getwd()
)
in_path <- file.path(base_dir, "dataset", "export.geojson")
out_path <- file.path(base_dir, "dataset", "garden.geojson")

cat(sprintf("[filter] input:  %s\n", in_path))
cat(sprintf("[filter] output: %s\n", out_path))
stopifnot(file.exists(in_path))

# 2) Read data
s0 <- st_read(in_path, quiet = TRUE)
cat(sprintf("[filter] read ok: %d features, %d columns\n", nrow(s0), ncol(s0)))

# 3) Find name column (compatible with different datasets)
name_candidates <- c("name", "NAME", "park_name", "reserve_nam", "res_name", "pname", "title", "label")
name_col <- intersect(names(s0), name_candidates)
name_chr <- if (length(name_col)) as.character(s0[[name_col[1]]]) else rep(NA_character_, nrow(s0))
name_norm <- tolower(trimws(name_chr))

# 4) First remove point features with name only 'garden'/'park'
gt0 <- as.character(st_geometry_type(s0, by_geometry = TRUE))
is_point0 <- gt0 %in% c("POINT", "MULTIPOINT")
is_generic_name <- !is.na(name_norm) & name_norm %in% c("garden", "park")
rm_generic_points <- is_point0 & is_generic_name
cat(sprintf("[filter] remove generic points (name=='garden'/'park'): %d\n", sum(rm_generic_points, na.rm = TRUE)))
s1 <- s0[!rm_generic_points, , drop = FALSE]

# 5) Keep only polygons (also handle GeometryCollection <U+2192> extract polygons); remove Z/M dimensions
s1 <- tryCatch(st_zm(s1, drop = TRUE, what = "ZM"), error = function(e) s1)

gt1 <- as.character(st_geometry_type(s1, by_geometry = TRUE))
if (any(gt1 == "GEOMETRYCOLLECTION")) {
  s1 <- st_collection_extract(s1, "POLYGON", warn = FALSE)
}

gt2 <- as.character(st_geometry_type(s1, by_geometry = TRUE))
keep_poly <- gt2 %in% c("POLYGON", "MULTIPOLYGON")
removed_points_lines <- sum(!keep_poly, na.rm = TRUE)
cat(sprintf("[filter] remove non-polygons (points/lines/etc.): %d\n", removed_points_lines))

s2 <- s1[keep_poly, , drop = FALSE]
s2 <- s2[!st_is_empty(s2), , drop = FALSE]

# 6) Coordinate system handling + fix invalid geometries
crs <- st_crs(s2)
if (is.null(crs) || is.na(crs$epsg)) {
  st_crs(s2) <- 4326
} else if (crs$epsg != 4326) {
  s2 <- st_transform(s2, 4326)
}
s2 <- suppressWarnings(st_make_valid(s2))

# 6.6) <U+2014><U+2014> Low-information name filtering: only contains park/garden (or plurals/combinations), or empty
# Helper function: convert name to lowercase alphanumeric string only
norm_key <- function(x) gsub("[^a-z0-9]+", "", tolower(trimws(x)))
# Allowed keys that consist only of these words
pg_keys <- c("park", "parks", "garden", "gardens", "parkgarden", "gardenpark", "parkandgarden", "gardenandpark")
# Re-extract name column for s2 (to prevent row index changes after upstream filtering)
name_col2 <- intersect(names(s2), name_candidates)
name_chr2 <- if (length(name_col2)) as.character(s2[[name_col2[1]]]) else rep(NA_character_, nrow(s2))
key2 <- norm_key(name_chr2)

rm_empty_name <- is.na(name_chr2) | !nzchar(trimws(name_chr2))
rm_generic_name <- !rm_empty_name & key2 %in% pg_keys
rm_lowinfo <- rm_empty_name | rm_generic_name

cat(sprintf(
  "[filter] remove low-info polygons (empty or only 'park/garden'): %d\n",
  sum(rm_lowinfo, na.rm = TRUE)
))

if (any(rm_lowinfo, na.rm = TRUE)) {
  s2 <- s2[!rm_lowinfo, , drop = FALSE]
}

# 6.5) <U+2014><U+2014> Filter by area (unit: m<U+00B2>)
# Enable s2 in lat/lon coordinates to get true area on Earth's ellipsoid (m<U+00B2>)
s2_old <- sf::sf_use_s2()
sf::sf_use_s2(TRUE)
area_m2 <- as.numeric(st_area(s2))
sf::sf_use_s2(s2_old)

# Record area and filter
s2$`__area_m2__` <- area_m2
keep_big <- is.finite(area_m2) & (area_m2 >= min_area_m2)

kept <- sum(keep_big, na.rm = TRUE)
total <- nrow(s2)
pct <- if (total > 0) 100 * kept / total else 0

cat(sprintf(
  "[filter] area filter: min_area_m2 = %.0f <U+2192> kept %d / %d (%.1f%%)\n",
  min_area_m2, kept, total, pct
))

if (total > 0) {
  qs <- stats::quantile(area_m2, probs = c(0, .25, .5, .75, .9, .95, 1), na.rm = TRUE)
  qtxt <- paste(names(qs), format(round(qs), big.mark = ","), collapse = " | ")
  cat(sprintf("[filter] area(m^2) quantiles: %s\n", qtxt))
}

s3 <- s2[keep_big, , drop = FALSE]

if (nrow(s3) == 0) {
  cat("[filter] WARNING: no polygons remain after area filtering; writing empty GeoJSON.\n")
}

# 7) Write garden.geojson (overwrite)
if (file.exists(out_path)) unlink(out_path)
st_write(s3, out_path, driver = "GeoJSON", quiet = TRUE)
cat(sprintf("[filter] saved polygons: %d -> %s\n", nrow(s3), out_path))

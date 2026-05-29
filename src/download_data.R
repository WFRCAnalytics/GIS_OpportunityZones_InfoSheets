library(yaml)
library(arcgislayers)
library(tigris)
library(tidycensus)
library(sf)

load_config <- function(path = "config/data_sources.yml") {
  yaml::read_yaml(path)
}

.cache_path <- function(remotes_dir, name) {
  file.path(remotes_dir, paste0(name, ".gpkg"))
}

# Validates that the GPKG exists and contains the expected layer name.
.is_valid_gpkg <- function(path, layer) {
  tryCatch({ layer %in% sf::st_layers(path)$name }, error = function(e) FALSE)
}

# Normalises geometry column to 'geom' (so OGR SQL SELECT lists can name it explicitly)
# and renames any 'fid' attribute column to 'fid_src' (GPKG reserves 'fid').
.write_gpkg <- function(data, path, layer) {
  geom_col <- attr(data, "sf_column")
  conflict <- tolower(names(data)) == "fid" & names(data) != geom_col
  if (any(conflict)) names(data)[conflict] <- paste0(names(data)[conflict], "_src")
  data <- sf::st_set_geometry(data, "geom")
  sf::st_write(data, path, layer = layer, delete_dsn = TRUE, quiet = TRUE)
}

.fetch_arcgis <- function(name, cfg, remotes_dir) {
  if (identical(cfg$url, "FILL_IN")) {
    message("skip   : ", name, " (URL not configured)")
    return(NULL)
  }
  path <- .cache_path(remotes_dir, name)
  if (file.exists(path) && !cfg$force_download && .is_valid_gpkg(path, name)) {
    message("cache  : ", name)
    return(path)
  }
  message("download: ", name)
  data <- arcgislayers::arc_open(cfg$url) |> arcgislayers::arc_select()
  if (!inherits(data, "sf")) {
    stop("'", name, "' did not return an sf object — service may lack geometry or returned partial data.")
  }
  .write_gpkg(data, path, layer = name)
  path
}

.fetch_tigris <- function(name, cfg, state, year, remotes_dir) {
  path <- .cache_path(remotes_dir, name)
  if (file.exists(path) && !cfg$force_download && .is_valid_gpkg(path, name)) {
    message("cache  : ", name)
    return(path)
  }
  message("download: ", name)
  data <- switch(name,
    counties = tigris::counties(state = state, year = year),
    tracts   = tigris::tracts(state = state, year = year),
    places   = tigris::places(state = state, year = year)
  )
  .write_gpkg(data, path, layer = name)
  path
}

.fetch_acs <- function(cfg, remotes_dir) {
  path <- file.path(remotes_dir, "acs.csv")
  if (file.exists(path) && !cfg$force_download) {
    message("cache  : acs")
    return(path)
  }
  message("download: acs")
  data <- tidycensus::get_acs(
    geography = cfg$geography,
    variables = unlist(cfg$variables),
    state     = cfg$state,
    year      = cfg$year,
    survey    = cfg$survey
  )
  readr::write_csv(data, path)
  path
}

# Ensures all layers are cached and returns a named list of file paths.
# Callers read layers with sf::read_sf(cache_paths$name, query = ...).
download_all <- function(config) {
  remotes_dir <- config$settings$data_dir_remotes
  dir.create(remotes_dir, showWarnings = FALSE, recursive = TRUE)

  arcgis <- mapply(
    .fetch_arcgis,
    name     = names(config$arcgis_layers),
    cfg      = config$arcgis_layers,
    MoreArgs = list(remotes_dir = remotes_dir),
    SIMPLIFY = FALSE
  )

  tigris_paths <- mapply(
    .fetch_tigris,
    name     = names(config$tigris$layers),
    cfg      = config$tigris$layers,
    MoreArgs = list(
      state       = config$tigris$state,
      year        = config$tigris$year,
      remotes_dir = remotes_dir
    ),
    SIMPLIFY = FALSE
  )

  acs_path <- .fetch_acs(config$tidycensus, remotes_dir)

  c(arcgis, tigris_paths, list(acs = acs_path))
}

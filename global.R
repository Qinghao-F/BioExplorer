# global.R
# Load libraries and define helper for reading CSV data only

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(readr)
library(stringr)
library(htmltools)
library(tibble)
library(leaflet)
library(leaflet.extras)
library(ggplot2)


source("tableau-in-shiny-v1.2.R")

# ---------- Data Helper ----------

# Safely read a CSV and normalize columns to: name, lat, lon, category, value
read_and_normalize_points <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(
      name = character(), lat = numeric(), lon = numeric(),
      category = character(), value = numeric()
    ))
  }

  df <- suppressMessages(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE, guess_max = 10000)
  )

  get_col <- function(df, candidates, default = NA) {
    for (nm in candidates) {
      hit <- names(df)[tolower(names(df)) == tolower(nm)]
      if (length(hit)) {
        return(df[[hit[1]]])
      }
      rx <- grep(nm, names(df), ignore.case = TRUE, value = TRUE)
      if (length(rx)) {
        return(df[[rx[1]]])
      }
    }
    if (is.na(default)[1]) {
      return(default)
    }
    rep(default, nrow(df))
  }

  # Prefer Common Name as 'name'; fallbacks included
  name_vec <- get_col(df, c("name", "^common.?name$", "title", "site", "label", "species", "taxa"),
    default = NA_character_
  )

  # Prefer Taxa as 'category'; with fallbacks
  category_vec <- get_col(df, c(
    "taxa", "category", "type", "class", "group",
    "kingdom", "phylum", "order", "family", "genus", "species"
  ),
  default = "Unknown"
  )

  # Latitude / Longitude from dedicated columns
  lat_vec <- suppressWarnings(as.numeric(
    get_col(df, c("^lat$", "latitude", "y"), default = NA_real_)
  ))
  lon_vec <- suppressWarnings(as.numeric(
    get_col(df, c("^lon$", "^lng$", "longitude", "x"), default = NA_real_)
  ))

  # Parse from geopoint if missing
  if (any(is.na(lat_vec) | is.na(lon_vec))) {
    geo <- get_col(df, c("^geopoint$", "geo", "coords", "coordinate", "coordinates"), default = NULL)
    if (!is.null(geo)) {
      m <- stringr::str_match(geo, "(-?\\d+(?:\\.\\d+)?)\\s*,\\s*(-?\\d+(?:\\.\\d+)?)")
      lat_from_geo <- suppressWarnings(as.numeric(m[, 2]))
      lon_from_geo <- suppressWarnings(as.numeric(m[, 3]))
      replace_na_idx_lat <- which(is.na(lat_vec) & !is.na(lat_from_geo))
      replace_na_idx_lon <- which(is.na(lon_vec) & !is.na(lon_from_geo))
      if (length(replace_na_idx_lat)) lat_vec[replace_na_idx_lat] <- lat_from_geo[replace_na_idx_lat]
      if (length(replace_na_idx_lon)) lon_vec[replace_na_idx_lon] <- lon_from_geo[replace_na_idx_lon]
    }
  }

  value_vec <- suppressWarnings(as.numeric(
    get_col(df, c("value", "score", "count", "size"), default = NA_real_)
  ))

  out <- tibble::tibble(
    name     = name_vec,
    lat      = lat_vec,
    lon      = lon_vec,
    category = category_vec,
    value    = value_vec
  ) %>%
    dplyr::filter(!is.na(lat), !is.na(lon))

  out$category[is.na(out$category) | out$category == ""] <- "Unknown"
  out$name[is.na(out$name) | out$name == ""] <- "(no name)"
  out
}

# global
points_data <- read_csv("./dataset/garden.csv", show_col_types = FALSE) %>%
  rename(
    name = `Common Name`,
    lat = lat,
    lon = lon,
    value = `Sighting Count`
  ) %>%
  filter(!is.na(lat), !is.na(lon))

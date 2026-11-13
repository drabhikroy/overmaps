# Runs the real app with the network calls replaced by offline demo data (for browser testing).
suppressMessages({ library(shiny); library(tibble) })
source("tests/demo_data.R")
env <- new.env(parent = globalenv()); suppressMessages(sys.source("app.R", envir = env))
env$geocode <- function(q) tibble(label = c("Elkins, Randolph County, West Virginia, United States", "Elkins Park, Montgomery County, Pennsylvania, United States"),
                                  lat = c(lat0, lat0), lon = c(lon0, lon0),
                                  boundary = list(demo_boundary(), NULL))      # the first match has city limits, the second does not
env$geocode_address <- function(q, bbox = NULL, limit = 1, bounded = FALSE) {
  if (grepl("zzz", q)) return(NULL)
  Sys.sleep(0.4)
  tibble(label = c("401 Davis Avenue, Elkins, Randolph County, West Virginia", "401 Davis Street, Elkins, Randolph County, West Virginia"), lat = c(38.9231, 38.9240), lon = c(-79.8465, -79.8450)) |> utils::head(limit)
}
env$fetch_roads <- function(lon, lat, radius_m, crs_m, shape = NULL, on_try = NULL, detail = "all") {
  cat("fetch", detail, if (is.null(shape)) "circle" else "city", "\n", file = "tests/fetch_calls.log", append = TRUE)
  if (detail == "paths") {
    mk <- function(x1, y1, x2, y2) st_linestring(matrix(c(cx + x1, cy + y1, cx + x2, cy + y2), ncol = 2, byrow = TRUE))
    pp <- st_sf(highway = c("cycleway", "cycleway", "footway", "path", "pedestrian"), footway = NA_character_, bicycle = NA_character_,
                geometry = st_sfc(mk(-2500, 300, 2500, 350), mk(400, -2500, 450, 2500), mk(-1500, -900, 900, -1200), mk(-800, 1500, 1600, 1000), mk(-300, -300, 300, 300), crs = crs0))
    return(list(osm_lines = st_transform(pp, 4326)))
  }
  r <- demo_roads(); set.seed(2); d <- r$osm_lines
  if (detail == "ancient") {        # an ancient site: the same streets, moved to wherever the site is
    d <- sf::st_set_crs(sf::st_set_geometry(d, sf::st_geometry(sf::st_set_crs(d, NA)) + c(lon - lon0, lat - lat0)), 4326)
    d$highway <- ifelse(seq_len(nrow(d)) %% 3 == 0, "path", d$highway); d$osm_id <- as.character(seq_len(nrow(d)))
    return(list(osm_lines = d))
  }
  d$sidewalk <- ifelse(d$highway %in% c("primary", "secondary", "tertiary") | runif(nrow(d)) < .35, "both", NA)
  d$cycleway <- ifelse(d$highway %in% c("secondary", "tertiary") & runif(nrow(d)) < .5, "lane", NA)
  d$oneway <- ifelse(d$highway == "residential" & runif(nrow(d)) < .08, "yes", NA)
  list(osm_lines = d)
}
env$photon_search <- function(q, kind = "place", near_bbox = NULL, limit = 6, bias = NULL) {
  Sys.sleep(0.2)
  if (kind == "place") {
    q0 <- tolower(q)
    if (grepl("^\\d*\\s*dav", q0)) return(tibble(label = c("401 Davis Avenue, Elkins, West Virginia", "401 Davis Street, Elkins, West Virginia"), lon = c(-79.8465, -79.8450), lat = c(38.9231, 38.9240), osm_type = "N", osm_id = c(11, 12), is_addr = TRUE, where = "Elkins, West Virginia, United States"))
    if (!grepl("^elk", q0)) return(NULL)
    return(tibble(label = c("Elkins, West Virginia, United States", "Elkins Park, Pennsylvania, United States"), lon = c(lon0, lon0 + .01), lat = c(lat0, lat0), osm_type = "R", osm_id = c(1, 2), is_addr = FALSE, where = ""))
  }
  if (!grepl("^\\d*\\s*dav", tolower(q))) return(NULL)
  tibble(label = c("401 Davis Avenue, Elkins, West Virginia", "401 Davis Street, Elkins, West Virginia"), lon = c(-79.8465, -79.8450), lat = c(38.9231, 38.9240), osm_type = "N", osm_id = c(11, 12))
}
env$geocode_lookup <- function(osm_type, osm_id) tibble(label = "Elkins, Randolph County, West Virginia, United States", lat = lat0, lon = lon0, boundary = list(demo_boundary()))
port <- as.integer(Sys.getenv("PORT", "8765"))
runApp(shinyApp(env$ui, env$server), host = "127.0.0.1", port = port, launch.browser = FALSE)

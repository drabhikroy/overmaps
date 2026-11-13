# Street hierarchy map: type in a place, get a map of its roads by role.
# Preview is rendered at 1600 x 2000 px and scaled to fit the window; downloads: PNG, JPEG, TIFF, PDF, SVG.
# Run with: shiny::runApp("path/to/this/folder")  (or open app.R in RStudio and click Run App)
#
# install.packages(c("shiny", "bslib", "sf", "osmdata", "dplyr", "tidyr", "purrr", "tibble",
#                    "stringr", "ggplot2", "ggtext", "showtext", "glue", "httr2", "ragg"))

library(shiny)
library(bslib)
library(sf)
library(osmdata)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(ggplot2)
library(ggtext)
library(showtext)
library(glue)
library(patchwork)       # attached so the | and / layout operators work on a fresh start, whichever map is drawn first
library(httr2)

# ---- Settings ---------------------------------------------------------------
OVERMAPS_VERSION <- "1.0.0"      # keep in step with DESCRIPTION, CITATION.cff, and the newest CHANGELOG.md section; a test checks all four
max_radius_km <- 50         # the absolute largest radius; the slider offers only a range suited to the chosen place
min_radius_km <- 0.5
full_detail_km <- 6         # up to this radius every street is downloaded; beyond it the map keeps main streets only, so it stays fast
main_detail_km <- 25        # beyond this radius only highways, arterials, and major collectors
all_streets_cap_km <- 12    # the "include every street" switch works up to this radius
max_city_km2 <- 1500      # largest city boundary drawn; bigger areas make Overpass slow and the map unreadable
max_query_chars <- 200      # longest place name accepted

# Show users only the app's own messages, not raw internal R/GEOS errors. Comment this out while debugging.
sanitize_errors <- TRUE
if (sanitize_errors) options(shiny.sanitize.errors = TRUE)
default_radius_km <- 3
user_agent <- "overmaps-shiny-app (personal project)"   # Nominatim requires an identifying agent

# Public Overpass servers. The first block is the global list; regional servers that suit the place are put in front of it
# (see overpass_mirrors_for), and OpenStreetMap's own download API is a last resort for small areas.
# overpass.kumi.systems is an alias of overpass.private.coffee, so it is not listed twice. To use other servers, set STREET_MAPS_OVERPASS to a
# comma-separated list of interpreter URLs; the list is then used exactly as given.
custom_mirrors <- function() {
  custom <- trimws(strsplit(Sys.getenv("STREET_MAPS_OVERPASS", unset = ""), ",")[[1]])
  custom[grepl("^https?://", custom)]
}
overpass_mirrors <- local({
  custom <- custom_mirrors()
  if (length(custom)) custom else c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.private.coffee/api/interpreter",
    "https://maps.mail.ru/osm/tools/overpass/api/interpreter")
})
osm_api_url <- "https://api.openstreetmap.org/api/0.6/map"     # not Overpass: the main site's own map download, limited to about 50,000 nodes

hw <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential",
        "motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link")

# Map styles. Each one sets the paper, text, and ring colors plus one color per road role
# (highways, arterials, major collectors, minor collectors, local streets), darkest/loudest first.
styles <- list(
  classic = list(label = "Classic", bg = "#f6f3ec", ink = "#1c1f24", muted = "#5a626b", faint = "#656d76",
                 ring = "#d8d2c4", plate = "#fcfaf5", cols = c("#a6372a", "#d9822b", "#1f3a5a", "#5d7b99", "#b9c3cd"),
                 speed = c("#2b7a9b", "#3d8f72", "#9c7a08", "#c4601a", "#b3261e"),     # slow to fast
                 # Direction: a day-and-dusk ring built from this app's own colors. North-south is the deep navy of night, east-west is the burnt orange
                 # of the sun's path (the same orange as arterials), and the diagonals carry teal and plum. The four in-between headings sit
                 # between their neighbors (steel, golden olive, brick, violet), so the ring turns smoothly and closes back on navy.
                 dir = c("#2a4d8f", "#356698", "#00899f", "#a6993f", "#d9822b", "#bb5f52", "#9c3d62", "#6f457b"),
                 dens = c("#c2dbf1", "#9ec3e2", "#74a7d4", "#4985bc", "#20609b", "#053b6c")),   # sparse to dense: one blue hue, light to dark

  dusk    = list(label = "Dusk", bg = "#121c2b", ink = "#f2eee6", muted = "#b5bfcb", faint = "#8794a4",
                 ring = "#2b3c52", plate = "#17263b", cols = c("#ff6b4a", "#f5b14a", "#7fd0e6", "#5b8db3", "#46607c"),
                 speed = c("#4cc2e0", "#6fd1a0", "#f2d14a", "#ff9f43", "#ff5d5d"),
                 dir = c("#648eed", "#6cabe7", "#43d5dc", "#d0bf5e", "#f79c40", "#f1857d", "#e96fa3", "#b67cca"),
                 dens = c("#15364f", "#155276", "#12719c", "#2092be", "#45b6de", "#87dcf7")),   # sparse to dense: dark to bright blue
  mono    = list(label = "Mono", bg = "#ffffff", ink = "#111111", muted = "#454545", faint = "#707070",
                 ring = "#cfcfcf", plate = "#fafafa", cols = c("#000000", "#2e2e2e", "#595959", "#8c8c8c", "#c8c8c8"),
                 speed = c("#8e8e8e", "#707070", "#4d4d4d", "#2a2a2a", "#000000"),
                 dir = c("#000000", "#2f2f2f", "#5c5c5c", "#7a7a7a", "#8c8c8c", "#7a7a7a", "#5c5c5c", "#2f2f2f"),   # gray cannot tell NE from NW
                 dens = c("#e6e6e6", "#c8c8c8", "#a3a3a3", "#7a7a7a", "#4a4a4a", "#111111"))
)
# A style is chosen by name. Unknown or missing names fall back to Classic (the value comes from the browser, so it is checked here).
split_style <- function(x) {
  x <- as.character(x %||% "classic")[1]; if (is.na(x)) x <- "classic"
  list(name = if (isTRUE(x %in% names(styles))) x else "classic", mode = "standard")
}
get_style_name <- function(x) split_style(x)$name
get_style <- function(x) styles[[split_style(x)$name]]
style_id <- function(style, mode = NULL) get_style_name(style)

bg <- styles$classic$bg; ink <- styles$classic$ink; muted <- styles$classic$muted
faint <- styles$classic$faint; ring_col <- styles$classic$ring

roles <- tribble(
  ~role,                      ~col,      ~lw,
  "Highways & major routes",  styles$classic$cols[1], 2.20,
  "Arterials",                styles$classic$cols[2], 1.75,
  "Major collectors",         styles$classic$cols[3], 1.30,
  "Minor collectors",         styles$classic$cols[4], 0.95,
  "Local streets",            styles$classic$cols[5], 0.50
)
role_of <- function(h) {
  case_when(h %in% c("motorway", "trunk", "motorway_link", "trunk_link") ~ roles$role[1],
            h %in% c("primary", "primary_link") ~ roles$role[2],
            h %in% c("secondary", "secondary_link") ~ roles$role[3],
            h %in% c("tertiary", "tertiary_link") ~ roles$role[4],
            TRUE ~ roles$role[5])
}

# ---- Fonts (fall back to the default sans font if Google Fonts is unreachable) ---
fonts_ok <- tryCatch({
  sysfonts::font_add_google("Oswald", "head")
  sysfonts::font_add_google("Fira Sans", "body")
  TRUE
}, error = function(e) FALSE)
head_font <- if (fonts_ok) "head" else "sans"
body_font <- if (fonts_ok) "body" else "sans"
showtext_auto()
fig_in <- c(8, 10)       # the figure is always 8 x 10 inches; only the pixel density changes
preview_dpi <- 250       # on-screen preview: 2000 x 2500 px, scaled down by the browser to fit (sharp on HiDPI screens)
print_dpi <- 300         # PNG, JPEG, and TIFF downloads: 2400 x 3000 px
vector_dpi <- 72         # PDF and SVG are measured in points (72 per inch); current showtext sizes vector text itself, so this only matters for older versions
# showtext sizes text for one specific dpi, so draw_figure() sets it right before each render.

export_formats <- c("PNG (300 dpi)" = "png", "JPEG (300 dpi)" = "jpeg", "TIFF (300 dpi)" = "tiff",
                    "PDF (vector)" = "pdf", "SVG (vector)" = "svg")
format_ext <- c(png = "png", jpeg = "jpg", tiff = "tif", pdf = "pdf", svg = "svg")

# ---- Helpers ----------------------------------------------------------------
# A boundary polygon from Nominatim's GeoJSON, as an sf geometry in lon/lat, or NULL if the place is only a point or line.
parse_boundary <- function(g) {
  if (!is.list(g) || !isTRUE(g$type %in% c("Polygon", "MultiPolygon"))) return(NULL)
  tryCatch({
    x <- suppressWarnings(sf::read_sf(as.character(jsonlite::toJSON(g, auto_unbox = TRUE, digits = NA)), quiet = TRUE))
    geom <- st_geometry(x)
    if (length(geom) != 1 || !as.character(st_geometry_type(geom)) %in% c("POLYGON", "MULTIPOLYGON")) return(NULL)
    st_set_crs(geom, 4326)
  }, error = function(e) NULL)
}

# Geocode a place name with OpenStreetMap's Nominatim (up to 5 candidates, each with its boundary when it has one).
geocode <- function(q) {
  resp <- request("https://nominatim.openstreetmap.org/search") |>
    req_url_query(q = q, format = "jsonv2", limit = 5, addressdetails = 0, polygon_geojson = 1, polygon_threshold = 0.0003) |>
    req_user_agent(user_agent) |>
    req_timeout(20) |>
    req_perform()
  res <- resp_body_json(resp)
  if (length(res) == 0) return(NULL)
  tibble(
    label = map_chr(res, "display_name"),
    lat = map_dbl(res, \(r) as.numeric(r$lat)),
    lon = map_dbl(res, \(r) as.numeric(r$lon)),
    boundary = map(res, \(r) parse_boundary(r$geojson)),
    south = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[1]] %||% NA))),     # Nominatim's bounding box: south, north, west, east
    north = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[2]] %||% NA))),
    west  = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[3]] %||% NA))),
    east  = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[4]] %||% NA)))
  )
}

# Find one street address, preferring results near the chosen place (bbox = c(south, north, west, east)). Returns NULL if nothing matches.
geocode_address <- function(q, bbox = NULL, limit = 1, bounded = FALSE) {
  req <- request("https://nominatim.openstreetmap.org/search") |>
    req_url_query(q = q, format = "jsonv2", limit = limit, addressdetails = 0) |>
    req_user_agent(user_agent) |> req_timeout(20)
  if (!is.null(bbox) && length(bbox) == 4 && all(is.finite(bbox)))
    req <- req_url_query(req, viewbox = sprintf("%.5f,%.5f,%.5f,%.5f", bbox[3], bbox[2], bbox[4], bbox[1]), bounded = as.integer(isTRUE(bounded)))
  res <- resp_body_json(req_perform(req))
  if (length(res) == 0) return(NULL)
  tibble(label = map_chr(res, \(r) r$display_name %||% ""), lat = map_dbl(res, \(r) as.numeric(r$lat)), lon = map_dbl(res, \(r) as.numeric(r$lon)))
}

# The text sent to the address search: the address, plus the place name unless the address already names it.
address_query <- function(addr, near) {
  if (grepl(strsplit(near, ",")[[1]][1], addr, fixed = TRUE)) addr else paste(addr, near, sep = ", ")
}

# An address is assumed to lie inside the place that was found: search only that place's box first, and look wider only when nothing is there.
find_address <- function(addr, near, bbox, limit = 5) {
  q <- address_query(addr, near)
  inside <- geocode_address(q, bbox, limit = limit, bounded = TRUE)
  if (!is.null(inside) && nrow(inside) > 0) return(list(hits = inside, outside = FALSE))
  # Nothing in OpenStreetMap inside the place: for a U.S. place, ask the Census Bureau's address file before looking wider.
  cs <- us_city_state(near)
  if (nzchar(cs)) {
    census <- tryCatch(census_search(paste(addr, cs, sep = ", "), limit), error = function(e) NULL)
    if (!is.null(census) && nrow(census) > 0) return(list(hits = census[, c("label", "lat", "lon")], outside = FALSE))
  }
  list(hits = geocode_address(q, bbox, limit = limit, bounded = FALSE), outside = TRUE)
}

# ---- Suggestions while typing -----------------------------------------------------------------------------------------------------
# Nominatim's usage policy forbids search-as-you-type, so suggestions come from Photon, a free search service built on OpenStreetMap data and meant for it.
# Only the text typed so far (and the place's box, for addresses) is sent, and only while the "Suggest as I type" switch is on.
photon_url <- "https://photon.komoot.io/api/"
photon_label <- function(pr, kind) {
  pick <- function(...) { v <- unlist(pr[c(...)], use.names = FALSE); v <- v[!is.na(v) & nzchar(v)]; v }
  place_name <- pick("city", "town", "village", "locality", "district")[1]
  if (kind == "address") {
    street <- paste(c(pick("housenumber"), pick("street", "name")[1]), collapse = " ")
    parts <- c(street, place_name, pick("state")[1])
  } else parts <- c(pick("name")[1], pick("county")[1], pick("state")[1], pick("country")[1])
  paste(unique(parts[!is.na(parts) & nzchar(parts)]), collapse = ", ")
}
# A Photon result is a street address when it has a house number or a street. OpenStreetMap stores a house that carries only an address as place=house,
# so the key "place" alone does not make a result a town, and a house with an address must still be suggested.
photon_is_address <- function(pr) {
  has_number <- !is.null(pr$housenumber) && nzchar(as.character(pr$housenumber))
  has_street <- !is.null(pr$street) && nzchar(as.character(pr$street))
  is_house  <- isTRUE(pr$osm_key == "place") && isTRUE(pr$osm_value %in% c("house", "housenumber", "plot"))
  has_number || is_house || (has_street && !isTRUE(pr$osm_key %in% c("place", "boundary")))
}
# kind: "place" is the one search box: it keeps cities, towns, counties and the like, and also street addresses (each with the place it lies in, in `where`);
# "address" keeps only results that have a street. near_bbox is c(south, north, west, east). bias is c(lat, lon): results near it rank higher,
# which is what makes "412 E Kirkwood Ave" find the one in the town on the map and not the first one in the world.
photon_search <- function(q, kind = "place", near_bbox = NULL, limit = 6, bias = NULL) {
  req <- request(photon_url) |> req_url_query(q = q, limit = 15, lang = "en") |> req_user_agent(user_agent) |> req_timeout(6) |> req_error(is_error = function(r) FALSE)
  if (!is.null(near_bbox) && length(near_bbox) == 4 && all(is.finite(near_bbox)))
    req <- req_url_query(req, bbox = sprintf("%.5f,%.5f,%.5f,%.5f", near_bbox[3], near_bbox[1], near_bbox[4], near_bbox[2]))      # Photon wants west, south, east, north
  else if (!is.null(bias) && length(bias) == 2 && all(is.finite(bias)))
    req <- req_url_query(req, lat = sprintf("%.5f", bias[1]), lon = sprintf("%.5f", bias[2]))
  resp <- req_perform(req)
  if (resp_status(resp) != 200) return(NULL)
  photon_rows(resp_body_json(resp)$features, kind, limit)
}
# Turn Photon's features into suggestion rows. Split from photon_search so it can be tested without the network.
photon_rows <- function(ft, kind = "place", limit = 6) {
  if (length(ft) == 0) return(NULL)
  rows <- lapply(ft, function(f) {
    pr <- f$properties; xy <- unlist(f$geometry$coordinates)
    if (length(xy) < 2) return(NULL)
    is_addr <- photon_is_address(pr)
    is_place <- !is_addr && isTRUE(pr$osm_key %in% c("place", "boundary"))
    keep <- if (kind == "place") (is_place || is_addr) else is_addr
    if (!keep) return(NULL)
    lab <- photon_label(pr, if (is_addr) "address" else "place"); if (!nzchar(lab)) return(NULL)
    pick <- function(...) { v <- unlist(pr[c(...)], use.names = FALSE); v <- v[!is.na(v) & nzchar(v)]; v }
    wh <- c(pick("city", "town", "village", "locality", "district")[1], pick("state")[1], pick("country")[1]); wh <- wh[!is.na(wh)]
    where <- if (is_addr) paste(unique(wh), collapse = ", ") else ""
    tibble(label = lab, lon = as.numeric(xy[1]), lat = as.numeric(xy[2]), osm_type = as.character(pr$osm_type %||% ""), osm_id = as.numeric(pr$osm_id %||% NA), is_addr = is_addr, where = where)
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(NULL)
  out <- out[!duplicated(out$label), ]
  utils::head(out, limit)
}
# The U.S. Census Bureau's address geocoder: free, no key, built from the Bureau's own address ranges and refreshed twice a year. It covers houses that
# OpenStreetMap volunteers have not mapped yet, but it needs a city and state (or a ZIP code) in the text. Returns the same shape as photon_rows().
census_url <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"
title_case <- function(x) gsub("(^|[[:space:]-])([a-z])", "\\1\\U\\2", tolower(as.character(x)), perl = TRUE)      # "412 E KIRKWOOD AVE" becomes "412 E Kirkwood Ave"
census_label <- function(m) {
  ac <- m$addressComponents
  st <- trimws(sub(",.*$", "", m$matchedAddress %||% ""))
  street <- title_case(st)
  city <- title_case(ac$city %||% ""); state <- toupper(ac$state %||% ""); zip <- ac$zip %||% ""
  paste(c(street, city, paste(c(state, zip)[nzchar(c(state, zip))], collapse = " ")), collapse = ", ")
}
census_rows <- function(matches, limit = 5) {
  if (length(matches) == 0) return(NULL)
  rows <- lapply(matches, function(m) {
    x <- suppressWarnings(as.numeric(m$coordinates$x)); y <- suppressWarnings(as.numeric(m$coordinates$y))
    if (!is.finite(x) || !is.finite(y)) return(NULL)
    ac <- m$addressComponents; city <- title_case(ac$city %||% ""); state <- toupper(ac$state %||% "")
    where <- paste(c(city, state)[nzchar(c(city, state))], collapse = ", ")
    tibble(label = trimws(census_label(m)), lon = x, lat = y, osm_type = "", osm_id = NA_real_, is_addr = TRUE, where = where)
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(NULL)
  utils::head(out[!duplicated(out$label), ], limit)
}
census_search <- function(q, limit = 5) {
  resp <- request(census_url) |> req_url_query(address = q, benchmark = "Public_AR_Current", format = "json") |>
    req_user_agent(user_agent) |> req_timeout(8) |> req_error(is_error = function(r) FALSE) |> req_perform()
  if (resp_status(resp) != 200) return(NULL)
  census_rows(resp_body_json(resp)$result$addressMatches, limit)
}
# The place a label names, for a U.S. search: drops the country and the county, so "Bloomington, Monroe County, Indiana, United States" becomes
# "Bloomington, Indiana". Returns "" when the place is not in the United States.
us_city_state <- function(near) {
  parts <- str_trim(strsplit(as.character(near)[1], ",")[[1]])
  if (length(parts) < 2 || !grepl("^United States", parts[length(parts)])) return("")
  parts <- parts[-length(parts)]
  parts <- parts[!grepl("County$|Parish$|Borough$|^[0-9]", parts)]
  paste(parts, collapse = ", ")
}

# Typed text that starts with a house number is taken as a street address. The place it lies in is what is left of the geocoder's label once the number,
# the street, and any postal code are dropped.
looks_like_address <- function(q) grepl("^\\s*[0-9]+[A-Za-z]?\\s+[^,0-9]", as.character(q)[1])
city_of_label <- function(label) {
  parts <- str_trim(strsplit(as.character(label)[1], ",")[[1]])
  # Nominatim gives the house number and the street as two parts; the Census file gives "412 E Kirkwood Ave" as one.
  parts <- if (length(parts) && grepl("^[0-9]+[A-Za-z]?$", parts[1])) parts[-seq_len(min(2, length(parts)))] else parts[-1]
  parts <- str_trim(sub("[[:space:]]+[0-9]{5}(-[0-9]{4})?$", "", parts))        # a ZIP code that trails the state ("IN 47408") goes
  parts <- parts[nzchar(parts) & !grepl("^[0-9][0-9A-Za-z -]*$", parts)]
  paste(parts, collapse = ", ")
}
# One exact OpenStreetMap object (type N, W or R, and its number) in the same shape geocode() returns, so a picked suggestion behaves like a search result.
geocode_lookup <- function(osm_type, osm_id) {
  if (!isTRUE(osm_type %in% c("N", "W", "R")) || !is.finite(osm_id)) return(NULL)
  resp <- request("https://nominatim.openstreetmap.org/lookup") |>
    req_url_query(osm_ids = paste0(osm_type, format(osm_id, scientific = FALSE)), format = "jsonv2", addressdetails = 0, polygon_geojson = 1, polygon_threshold = 0.0003) |>
    req_user_agent(user_agent) |> req_timeout(20) |> req_perform()
  res <- resp_body_json(resp)
  if (length(res) == 0) return(NULL)
  tibble(label = map_chr(res, "display_name"), lat = map_dbl(res, \(r) as.numeric(r$lat)), lon = map_dbl(res, \(r) as.numeric(r$lon)),
         boundary = map(res, \(r) parse_boundary(r$geojson)),
         south = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[1]] %||% NA))), north = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[2]] %||% NA))),
         west = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[3]] %||% NA))), east = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[4]] %||% NA))))
}

# UTM zone for a point, so distances and bearings are in meters wherever the place is.
utm_epsg <- function(lon, lat) {
  zone <- min(60, floor((lon + 180) / 6) + 1)    # lon = 180 exactly belongs to zone 60, not a 61st
  (if (lat >= 0) 32600 else 32700) + zone
}
# UTM is defined from 80 degrees S to 84 degrees N; beyond that, distances would be wrong.
utm_supported <- function(lat) is.finite(lat) && lat <= 84 && lat >= -80

# A usable drawing shape from a boundary: in meters, valid, one piece of geometry, with fine detail simplified away.
# Returns NULL if nothing usable is left.
city_shape <- function(boundary, crs_m) {
  if (is.null(boundary)) return(NULL)
  sh <- tryCatch({
    x <- st_make_valid(st_transform(boundary, crs_m))
    x <- suppressWarnings(st_collection_extract(x, "POLYGON"))
    if (length(x) == 0) return(NULL)
    x <- st_union(x)
    st_make_valid(st_simplify(x, preserveTopology = TRUE, dTolerance = max(5, sqrt(as.numeric(st_area(x)) / pi) * 0.002)))
  }, error = function(e) NULL)
  if (is.null(sh) || length(sh) == 0 || as.numeric(st_area(sh)) <= 0) return(NULL)
  sh
}
shape_radius <- function(shape) sqrt(as.numeric(st_area(shape)) / pi)    # radius of the circle with the same area, used to scale text and hexagons

# Nominatim's bounding box for candidate i as c(south, north, west, east); NA where the lookup gave none.
place_bbox <- function(p, i) unname(vapply(c("south", "north", "west", "east"), function(k) if (k %in% names(p)) as.numeric(p[[k]][i]) else NA_real_, 0))

# Distance in meters from a place's center to the farthest point of its boundary, or of its bounding box when there is no boundary.
# NA when neither is usable (a single address, say), in which case the slider keeps its general range.
enclosing_radius_m <- function(boundary, lon, lat, bbox = NULL) {
  crs_m <- utm_epsg(lon, lat)
  ctr <- as.numeric(st_coordinates(st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_m)))
  pts <- if (!is.null(boundary)) tryCatch(st_coordinates(st_transform(boundary, crs_m))[, c("X", "Y"), drop = FALSE], error = function(e) NULL)
  if (is.null(pts) && !is.null(bbox) && length(bbox) == 4 && all(is.finite(bbox))) {       # c(south, north, west, east)
    corners <- cbind(c(bbox[3], bbox[4], bbox[3], bbox[4]), c(bbox[1], bbox[1], bbox[2], bbox[2]))
    pts <- st_coordinates(st_transform(st_sfc(st_multipoint(corners), crs = 4326), crs_m))[, c("X", "Y"), drop = FALSE]
  }
  if (is.null(pts) || nrow(pts) == 0) return(NA_real_)
  max(sqrt((pts[, 1] - ctr[1])^2 + (pts[, 2] - ctr[2])^2))
}
# The radius slider for a place: the top is about one and a half times the radius that encloses it, the step suits the size, and the
# starting value covers the place, up to the radius where every street still downloads quickly.
radius_plan <- function(enc_m) {
  if (!is.finite(enc_m) || enc_m < 500) return(list(min = 1, max = 5, step = 0.5, value = default_radius_km))
  r <- enc_m / 1000
  hi <- min(max_radius_km, max(2, r * 1.5))
  step <- if (hi <= 6) 0.5 else if (hi <= 15) 1 else if (hi <= 30) 2.5 else 5
  hi <- min(max_radius_km, ceiling(hi / step) * step)
  lo <- min(max(step, floor(r / 8 / step) * step), hi - step)
  v <- ceiling(min(r, full_detail_km) / step) * step
  if (v > full_detail_km) v <- floor(full_detail_km / step) * step           # a coarse step must not push the start past the every-street limit
  list(min = lo, max = hi, step = step, value = min(max(v, lo), hi))
}
# How much of the street network to download for a radius: "all" streets, "main" (tertiary and up), or "major" (secondary and up).
# The same plan expressed in the displayed unit (steps and ends are rounded to values that read well in miles).
radius_plan_units <- function(plan, u = "km") {
  if (!identical(u, "mi")) return(plan)
  step <- if (plan$step <= 0.5) 0.25 else if (plan$step <= 1) 0.5 else if (plan$step <= 2.5) 1 else 2.5
  lo <- max(step, ceiling(plan$min / km_per_mile / step - 1e-9) * step)
  hi <- max(lo + step, floor(plan$max / km_per_mile / step + 1e-9) * step)
  list(min = lo, max = hi, step = step, value = min(hi, max(lo, round(plan$value / km_per_mile / step) * step)))
}
detail_for <- function(radius_m, all_streets = FALSE) {
  r <- radius_m / 1000
  if (r <= full_detail_km || (isTRUE(all_streets) && r <= all_streets_cap_km)) "all" else if (r <= main_detail_km) "main" else "major"
}

# ---- Changing options without downloading again ------------------------------------------------------------------------------
# A download covers a box around the area that was asked for. Later changes to the radius, the area type, or the street detail are drawn from
# those same streets whenever they fit inside that box; only a request that reaches past it (a bigger radius, a new place, a new address, or
# local streets that were left out) needs a fresh download.
place_key <- function(p, i) paste(p$label[i], round(p$lon[i], 5), round(p$lat[i], 5), sep = "|")
detail_rank <- function(d) match(d, c("major", "main", "all"), nomatch = 3L)
# What the options say, in a form that compares cheaply: the radius is rounded so a slider's echo of the same value reads as unchanged.
option_sig <- function(radius_km, area, all_streets, addr_sig) {
  a <- if (isTRUE(area %in% c("dim", "city", "circle"))) area else "dim"
  list(area = a, rk = if (a == "city") NA_real_ else round(min(max(radius_km, min_radius_km), max_radius_km), 4), all = isTRUE(all_streets),
       addr = if (a == "city") "" else (addr_sig %||% ""))
}
# Rebuild the drawing data for new options from the streets already on hand. Returns NULL when new streets are needed.
need_draw <- function(why) structure(list(why = why), class = "need_draw")
plan_live <- function(ld, st) {
  if (is.null(ld) || is.null(ld$roads) || is.null(ld$cover)) return(need_draw("new streets"))
  if (!identical(st$key, ld$key)) return(need_draw("streets for the place you found"))
  sig <- option_sig(st$radius_km, st$area, st$all_streets, st$addr_sig)
  if (identical(sig, ld$sig)) return(ld)                                         # nothing that matters has changed
  if (!identical(sig$area, "city") && !identical(sig$addr, ld$sig$addr)) return(need_draw("streets around the new address"))   # a new address needs a lookup, so a new download
  crs_m <- ld$crs_m; lon <- ld$base_lon; lat <- ld$base_lat
  radius_m <- as.integer(round(sig$rk * 1000)); shape <- NULL; inside <- NULL; center_txt <- ld$center_txt
  if (identical(sig$area, "city")) {
    shape <- city_shape(st$boundary, crs_m)
    if (is.null(shape) || as.numeric(st_area(shape)) > max_city_km2 * 1e6) return(need_draw("the city limits"))
    radius_m <- as.integer(round(shape_radius(shape)))
    pt <- suppressWarnings(st_coordinates(st_transform(st_point_on_surface(shape), 4326))); lon <- pt[1, 1]; lat <- pt[1, 2]; center_txt <- NULL
    want <- shape
  } else {
    if (is.null(ld$sig$addr) || !nzchar(ld$sig$addr)) center_txt <- NULL
    want <- st_buffer(center_of(lon, lat, crs_m), radius_m)
    if (identical(sig$area, "dim")) inside <- city_shape(st$boundary, crs_m)
  }
  detail <- detail_for(radius_m, sig$all)
  if (detail_rank(detail) > detail_rank(ld$detail)) return(need_draw("the local streets"))      # they were not downloaded
  if (!isTRUE(as.logical(st_covers(ld$cover, st_union(want))))) return(need_draw("streets farther out"))      # the area reaches past the streets on hand
  roads <- ld$roads
  if (!identical(detail, ld$detail) && !is.null(roads$osm_lines) && "highway" %in% names(roads$osm_lines))
    roads$osm_lines <- roads$osm_lines[roads$osm_lines$highway %in% detail_classes(detail), ]
  lines <- prep_lines(roads, lon, lat, radius_m, crs_m, shape)
  if (is.null(lines)) return(need_draw("streets for this area"))
  clip <- if (is.null(shape)) st_buffer(center_of(lon, lat, crs_m), radius_m * 0.988) else shape
  net <- network_stats(lines, segments_of(lines), clip, as.numeric(st_area(clip)) / 1e6, radius_m * 0.03)
  upd <- list(lines = lines, lon = lon, lat = lat, radius_m = radius_m, shape = shape, inside = inside, detail = detail, center_txt = center_txt, net = net, sig = sig, live = TRUE)
  for (k in names(upd)) ld[k] <- list(upd[[k]])        # field by field: modifyList would merge two street tables column by column, and drop NULLs
  ld
}

# ---- Downloading streets ----------------------------------------------------------------------------------------------------
# OpenStreetMap's public Overpass servers are shared, and in 2026 they are often slow, overloaded, or rate-limiting. So:
#  * each server gets a quick status check first (6 s), so a dead or swamped one costs seconds, not its whole time limit;
#  * each real attempt has a hard client-side limit, and the next server starts as soon as one fails;
#  * a server that just failed goes to the back of the line for five minutes;
#  * regional servers for the place go first, and for small areas OpenStreetMap's own map download is the last resort;
#  * the whole download has an overall deadline, every failure is recorded with a plain reason, and a finished download is kept
#    on disk, so the same place never needs the servers twice.
mirror_state <- new.env(parent = emptyenv())
mirror_state$bad_until <- list()
overpass_ua <- "street-hierarchy-maps (R Shiny app; data from OpenStreetMap)"

order_mirrors <- function(mirrors = overpass_mirrors, now = Sys.time()) {
  bad <- vapply(mirrors, function(u) { t <- mirror_state$bad_until[[u]]; !is.null(t) && t > now }, logical(1))
  c(mirrors[!bad], mirrors[bad])
}
mark_mirror <- function(url, ok, now = Sys.time(), cool_s = 300) {
  mirror_state$bad_until[[url]] <- if (isTRUE(ok)) NULL else now + cool_s
  invisible(NULL)
}

# Regional servers that cover a place (North America and Europe, run by dresel.systems; updated nightly, which is plenty for roads).
regional_mirrors <- function(lon, lat) {
  if (lon >= -170 && lon <= -50 && lat >= 5 && lat <= 84) "https://ovp.dresel.systems/api-na/interpreter"
  else if (lon >= -25 && lon <= 45 && lat >= 34 && lat <= 72) "https://ovp.dresel.systems/api/interpreter"
  else character()
}
# Servers to try for a bounding box, best first. A custom list from STREET_MAPS_OVERPASS is used exactly as given.
overpass_mirrors_for <- function(bb) {
  cm <- custom_mirrors(); if (length(cm)) return(cm)
  regional <- regional_mirrors((bb[["xmin"]] + bb[["xmax"]]) / 2, (bb[["ymin"]] + bb[["ymax"]]) / 2)
  small <- (bb[["xmax"]] - bb[["xmin"]]) * (bb[["ymax"]] - bb[["ymin"]]) <= 0.02       # about 14 x 14 km at mid-latitudes
  c(regional, overpass_mirrors, if (small) osm_api_url)
}

# Seconds one server may take on the first pass: small areas answer quickly, big cities need longer.
attempt_cap <- function(area_km2) min(60, max(25, 20 + 0.4 * area_km2))

# Plain-language reason for a failure, from the error text we recorded. Returns a short phrase and a class.
explain_failure <- function(msg) {
  m <- tolower(msg)
  if (grepl("could not resolve|resolve host|name or service|getaddrinfo|nodename", m)) c("unreachable", "its address could not be looked up")
  else if (grepl("too many nodes|limit is 50000", m)) c("dense", "this area is too dense for OpenStreetMap's direct download")
  else if (grepl("status check", m)) c("slow", "it did not answer a quick status check")
  else if (grepl("no free query slot", m)) c("busy", "it had no free query slot")
  else if (grepl("status 429|status 406", m)) c("busy", "it is rate-limiting requests")
  else if (grepl("status 50[0-9]|could not finish", m)) c("busy", "it is overloaded")
  else if (grepl("connection refused|failed to connect|couldn't connect|connection reset|ssl|tls|certificate|proxy|recv failure|send failure", m)) c("unreachable", "the connection was refused or interrupted")
  else if (grepl("timeout|timed out", m)) c("slow", "it did not answer in time")
  else if (grepl("status 4", m)) c("rejected", "it rejected the request")
  else c("other", "it failed")
}
host_of <- function(url) sub("^https?://([^/]+).*$", "\\1", url)

# A quick check that a server is alive and has a free query slot, so a dead or swamped one costs seconds rather than a full attempt.
# Servers with no status page, or an unreadable one, are simply tried.
overpass_ready <- function(url, check_s = 6) {
  resp <- tryCatch(request(sub("/interpreter$", "/status", url)) |> req_user_agent(overpass_ua) |> req_timeout(check_s) |>
                     req_error(is_error = function(r) FALSE) |> req_perform(), error = function(e) e)
  if (inherits(resp, "error")) stop(sprintf("status check failed: %s", conditionMessage(resp)), call. = FALSE)
  if (resp_status(resp) != 200) return(invisible(TRUE))
  txt <- tryCatch(resp_body_string(resp), error = function(e) "")
  free <- regmatches(txt, regexec("([0-9]+) slots? available now", txt))[[1]]
  if (length(free) == 2 && as.numeric(free[2]) > 0) return(invisible(TRUE))
  wait <- regmatches(txt, regexec("Slot available after: [^\n]*?in ([0-9]+) seconds", txt))[[1]]
  if (length(wait) == 2) {
    w <- as.numeric(wait[2])
    if (w <= 8) { Sys.sleep(w + 1); return(invisible(TRUE)) }
    stop(sprintf("no free query slot for %d s", w), call. = FALSE)
  }
  invisible(TRUE)
}

parse_osm_doc <- function(doc, q) {
  f <- tempfile(fileext = ".osm"); on.exit(unlink(f), add = TRUE)     # osmdata reads a downloaded answer most reliably from a file
  writeLines(doc, f, useBytes = TRUE)
  osmdata_sf(q, f, quiet = TRUE)
}

# OpenStreetMap's own map download: every feature in the box, so keep only the street classes we draw.
osm_api_get <- function(bb, cap_s, feature_values = hw) {
  resp <- request(osm_api_url) |>
    req_url_query(bbox = sprintf("%.6f,%.6f,%.6f,%.6f", bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])) |>
    req_user_agent(overpass_ua) |> req_timeout(cap_s) |> req_error(is_error = function(r) FALSE) |> req_perform()
  status <- resp_status(resp)
  body <- tryCatch(resp_body_string(resp), error = function(e) "")
  if (status != 200) stop(sprintf("server answered with status %d %s", status, substr(gsub("\\s+", " ", body), 1, 80)), call. = FALSE)
  res <- parse_osm_doc(body, opq(bbox = bb))
  if (!is.null(res$osm_lines) && nrow(res$osm_lines) > 0 && "highway" %in% names(res$osm_lines))
    res$osm_lines <- res$osm_lines[res$osm_lines$highway %in% feature_values, ]
  res
}

# One attempt against one server, within `cap_s` seconds in all.
overpass_get <- function(url, bb, cap_s, feature_values = hw) {
  t0 <- Sys.time()
  if (identical(url, osm_api_url)) return(osm_api_get(bb, cap_s, feature_values))
  overpass_ready(url, check_s = min(6, cap_s / 3))
  left <- max(5, cap_s - as.numeric(difftime(Sys.time(), t0, units = "secs")))
  q <- opq(bbox = bb, timeout = ceiling(left)) |> add_osm_feature(key = "highway", value = feature_values)
  resp <- request(url) |>
    req_user_agent(overpass_ua) |>
    req_body_form(data = opq_string(q)) |>
    req_timeout(left) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()
  status <- resp_status(resp)
  if (status != 200) stop(sprintf("server answered with status %d", status), call. = FALSE)
  doc <- resp_body_string(resp)
  # Overpass reports a query that ran out of time or memory inside a normal-looking answer.
  if (grepl("<remark", doc, fixed = TRUE) && grepl("runtime error|timed out|out of memory", doc, ignore.case = TRUE))
    stop("server could not finish the query", call. = FALSE)
  parse_osm_doc(doc, q)
}

overpass_download <- function(bb, area_km2 = 10, mirrors = overpass_mirrors, deadline_s = 180, passes = c(1, 2.2),
                              on_try = NULL, clock = Sys.time, getter = overpass_get) {
  start <- clock(); base <- attempt_cap(area_km2); tries <- list()
  for (pi in seq_along(passes)) for (u in order_mirrors(mirrors, clock())) {
    if (pi > 1 && identical(u, osm_api_url)) next                  # the main site's download is tried once, never repeated
    cap <- min(base * passes[pi], deadline_s - as.numeric(difftime(clock(), start, units = "secs")))
    if (cap < 8) return(list(data = NULL, tries = tries))          # not enough time left for an attempt that could finish
    if (!is.null(on_try)) on_try(length(tries) + 1, length(mirrors) * length(passes), u, cap)
    t0 <- clock()
    out <- tryCatch(getter(u, bb, cap), error = function(e) e)
    ok <- !inherits(out, "error")
    tries[[length(tries) + 1]] <- list(url = u, ok = ok, secs = as.numeric(difftime(clock(), t0, units = "secs")),
                                       cap = cap, msg = if (ok) "" else conditionMessage(out))
    mark_mirror(u, ok, clock())
    if (ok) return(list(data = out, tries = tries))
  }
  list(data = NULL, tries = tries)
}

# The message a person sees when every server failed: what was tried and why each one failed, so it can be acted on.
download_failure_message <- function(tries) {
  secs <- round(sum(vapply(tries, function(t) t$secs, 0)))
  if (length(tries) == 0) return("There was not enough time to ask OpenStreetMap's servers. Please try again, or choose a smaller area.")
  why <- lapply(tries, function(t) explain_failure(t$msg))
  hosts <- vapply(tries, function(t) host_of(t$url), "")
  first <- !duplicated(hosts)                                      # one line per server: its first reason
  detail <- paste(sprintf("%s: %s", hosts[first], vapply(why[first], function(w) w[2], "")), collapse = "; ")
  if (all(vapply(why, function(w) w[1], "") == "unreachable"))
    return(sprintf("Could not reach any OpenStreetMap server (%s). Check your internet connection, VPN, or firewall, then try again.", detail))
  if (any(vapply(why, function(w) w[1], "") == "dense"))
    detail <- paste0(detail, ". Try a smaller radius, or a different map area setting")
  sprintf("OpenStreetMap's servers did not answer in time (%d attempts over %d seconds). %s. Please try again in a minute, or choose a smaller area.",
          length(tries), secs, detail)
}

# Finished downloads live in the app's data folder, so a place already downloaded needs no server, even after a restart.
roads_cache_dir <- function() { d <- file.path(data_dir(), "roads_cache"); dir.create(d, showWarnings = FALSE, recursive = TRUE); d }
cache_max_days <- 30

# Street classes to download for each level of detail (the "_link" ramps come along with their parent class).
path_hw <- c("cycleway", "footway", "path", "pedestrian")      # separate bike and walking ways, fetched only for the Bike & walk map
# Excavated streets are mapped as paths, footways, tracks, service roads, or residential streets depending on the volunteer, so an ancient site asks for all of them.
ancient_hw <- c(hw, path_hw, "track", "service", "living_street", "road", "bridleway")
detail_classes <- function(detail) switch(detail, ancient = ancient_hw, paths = path_hw, all = hw, main = hw[grepl("^(motorway|trunk|primary|secondary|tertiary)", hw)],
                                          major = hw[grepl("^(motorway|trunk|primary|secondary)", hw)], hw)

fetch_roads <- function(lon, lat, radius_m, crs_m, shape = NULL, on_try = NULL, detail = "all") {
  detail <- if (isTRUE(detail %in% c("all", "main", "major", "paths", "ancient"))) detail else "all"
  center <- st_sfc(st_point(c(lon, lat)), crs = 4326)
  area <- if (is.null(shape)) st_buffer(st_transform(center, crs_m), radius_m + 100) else st_buffer(shape, 100)
  bb <- area |> st_transform(4326) |> st_bbox()
  key <- if (is.null(shape)) sprintf("roads_%.4f_%.4f_%d%s.rds", lon, lat, radius_m, if (detail == "all") "" else paste0("_", detail))
         else sprintf("roads_city_%.4f_%.4f_%.4f_%.4f%s.rds", bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]], if (detail == "all") "" else paste0("_", detail))
  cache <- file.path(roads_cache_dir(), key)
  if (file.exists(cache) && as.numeric(difftime(Sys.time(), file.mtime(cache), units = "days")) < cache_max_days) {
    hit <- tryCatch(readRDS(cache), error = function(e) NULL)
    if (!is.null(hit)) return(hit)
  }

  # Near the 180-degree meridian the box wraps around the whole world; refuse instead of asking for it.
  if (bb[["xmin"]] > bb[["xmax"]] || (bb[["xmax"]] - bb[["xmin"]]) > 5)
    stop(shiny::safeError("Places this close to the 180\u00b0 meridian are not supported yet."))

  res <- overpass_download(bb, area_km2 = as.numeric(st_area(area)) / 1e6, mirrors = overpass_mirrors_for(bb), on_try = on_try,
                          getter = function(u, bb, cap) overpass_get(u, bb, cap, feature_values = detail_classes(detail)))
  if (is.null(res$data)) {
    msg <- download_failure_message(res$tries)
    message("[overmaps] download failed: ", paste(sprintf("%s -> %s", vapply(res$tries, function(t) host_of(t$url), ""),
                                                           vapply(res$tries, function(t) substr(t$msg, 1, 120), "")), collapse = " | "))
    stop(shiny::safeError(msg))
  }
  saveRDS(res$data, cache)
  prune_cache()
  res$data
}

# Keep only the newest few downloads so the folder does not grow without limit.
prune_cache <- function(keep = 40) {
  f <- list.files(roads_cache_dir(), pattern = "^roads_.*\\.rds$", full.names = TRUE)
  if (length(f) <= keep) return(invisible(0))
  old <- f[order(file.mtime(f), decreasing = TRUE)][-seq_len(keep)]
  invisible(sum(file.remove(old)))
}

# Split clipped geometries into single-part lines without losing any part. Casting a column that
# mixes LINESTRING and MULTILINESTRING straight to LINESTRING keeps only the first part of each
# multi-part geometry, which silently dropped street length (roads that leave and re-enter the
# ring, or loop back across themselves).
explode_lines <- function(x) {
  if (nrow(x) == 0) return(x)
  x <- suppressWarnings(st_collection_extract(x, "LINESTRING"))
  if (nrow(x) == 0) return(x)
  x <- suppressWarnings(st_cast(st_cast(x, "MULTILINESTRING"), "LINESTRING"))   # "already of that type" notes are harmless
  filter(x, !st_is_empty(geometry))
}

# Turn the raw download into clipped, classified road lines.
prep_lines <- function(roads, lon, lat, radius_m, crs_m, shape = NULL) {
  if (is.null(roads$osm_lines) || nrow(roads$osm_lines) == 0) return(NULL)
  center_m <- st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_m)
  # Clip a hair inside the ring so thick roads and their paper casing never poke past the edge.
  clip_circle <- if (is.null(shape)) st_buffer(center_m, radius_m * 0.988) else {
    inner <- st_buffer(shape, -radius_m * 0.012)
    if (length(inner) == 0 || all(st_is_empty(inner))) shape else inner
  }
  tagged <- facility_flags(roads$osm_lines)        # sidewalk and bike lane tags, read before the other columns are dropped
  lines <- roads$osm_lines |>
    select(any_of(c("osm_id", "name", "highway", "maxspeed", "bridge", "tunnel", "oneway"))) |>
    st_transform(crs_m)
  lines$walk_tag <- tagged$walk; lines$bike_tag <- tagged$bike
  if (!"name" %in% names(lines)) lines$name <- NA_character_
  if (!"maxspeed" %in% names(lines)) lines$maxspeed <- NA_character_
  for (k in c("bridge", "tunnel", "oneway")) if (!k %in% names(lines)) lines[[k]] <- NA_character_     # tags are optional; missing means untagged
  if (!"highway" %in% names(lines)) return(NULL)
  lines <- suppressWarnings(st_intersection(lines, clip_circle)) |>
    explode_lines() |>
    filter(!is.na(highway)) |>
    mutate(role = factor(role_of(highway), levels = roles$role),
           km = as.numeric(st_length(geometry)) / 1000)
  if (nrow(lines) == 0) NULL else lines
}

# Sidewalk and bike lane tags on a street. Columns such as sidewalk, sidewalk.left, cycleway.both (or with a colon) are all looked at.
# A sidewalk counts when it is tagged both, left, right, yes, or separate (mapped as its own way); a bike facility when it is a lane, track, or separate path.
# "no", "none", and a shared lane (painted arrows only) do not count. Missing tags mean unknown, which the map shows as untagged, not as absent.
facility_flags <- function(x) {
  n <- nrow(x); nm <- names(x)
  any_of_vals <- function(pattern, ok) {
    cols <- nm[grepl(pattern, nm)]; if (!length(cols)) return(rep(FALSE, n))
    hit <- vapply(cols, function(k) tolower(as.character(sf::st_drop_geometry(x)[[k]])) %in% ok, logical(n))
    rowSums(matrix(hit, nrow = n)) > 0
  }
  list(walk = any_of_vals("^sidewalk([.:](left|right|both))?$", c("both", "left", "right", "yes", "separate")),
       bike = any_of_vals("^cycleway([.:](left|right|both))?$", c("lane", "track", "opposite_lane", "opposite_track", "share_busway", "separate", "yes")))
}
active_levels <- c("No sidewalk or bike tag", "Sidewalk tagged", "Bike lane tagged", "Sidewalk and bike lane", "Walking path", "Bike path")
active_class <- function(lines) factor(active_levels[ifelse(lines$walk_tag & lines$bike_tag, 4L, ifelse(lines$bike_tag, 3L, ifelse(lines$walk_tag, 2L, 1L)))], levels = active_levels)
# Separate walking and bike ways (the extra download), clipped like the streets. Crossings are left out; sidewalks drawn as their own ways count as walking paths.
prep_paths <- function(roads, lon, lat, radius_m, crs_m, shape = NULL) {
  x <- roads$osm_lines
  if (is.null(x) || nrow(x) == 0 || !"highway" %in% names(x)) return(NULL)
  center_m <- st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_m)
  clip <- if (is.null(shape)) st_buffer(center_m, radius_m * 0.988) else { inner <- st_buffer(shape, -radius_m * 0.012); if (length(inner) == 0 || all(st_is_empty(inner))) shape else inner }
  col <- function(k) if (k %in% names(x)) tolower(as.character(x[[k]])) else rep(NA_character_, nrow(x))
  hwy <- col("highway"); ft <- col("footway"); bic <- col("bicycle")
  kind <- ifelse(hwy == "cycleway", 6L, ifelse(hwy == "path" & bic %in% "designated", 6L, ifelse(hwy %in% c("footway", "path", "pedestrian"), 5L, NA_integer_)))
  kind[hwy %in% "footway" & ft %in% "crossing"] <- NA_integer_
  x <- x[!is.na(kind), "highway"]; if (nrow(x) == 0) return(NULL)
  x$kind <- kind[!is.na(kind)]
  x <- suppressWarnings(st_intersection(st_transform(x, crs_m), clip)) |> explode_lines()
  if (nrow(x) == 0) return(NULL)
  x$km <- as.numeric(st_length(x$geometry)) / 1000
  x[, c("kind", "km")]
}
load_paths <- function(lon, lat, radius_m, crs_m, shape = NULL) {
  prep_paths(fetch_roads(lon, lat, radius_m, crs_m, shape, detail = "paths"), lon, lat, radius_m, crs_m, shape)
}
# Place one name per through route, beside its road, skipping names that do not fit or would crowd.
label_mm <- 2.9   # street-name text size (ggplot mm, about 8 pt)
make_labels <- function(lines, radius_m, crs_m, center_m, shape = NULL) {
  m_per_char <- radius_m * 0.020
  offset_m <- radius_m * 0.038
  through <- lines |> filter(role %in% roles$role[1:3], !is.na(name))
  if (nrow(through) == 0) return(NULL)
  by_name <- through |>
    mutate(rank = as.integer(role)) |>
    group_by(name) |>
    summarise(rank = min(rank), geometry = st_union(geometry), .groups = "drop")

  lab <- pmap_dfr(list(by_name$name, by_name$rank, st_geometry(by_name)), function(nm, rk, g) {
    tryCatch({
      parts <- suppressWarnings(st_cast(st_line_merge(st_cast(st_sfc(g, crs = crs_m), "MULTILINESTRING")), "LINESTRING"))
      len <- as.numeric(st_length(parts))
      i <- which.max(len)
      need <- nchar(nm) * m_per_char
      if (len[i] < need * 0.9) return(NULL)
      p <- st_coordinates(st_cast(st_line_sample(parts[i], sample = c(0.45, 0.5, 0.55)), "POINT"))
      ang <- atan2(p[3, 2] - p[1, 2], p[3, 1] - p[1, 1]) * 180 / pi
      ang <- if (ang > 90) ang - 180 else if (ang < -90) ang + 180 else ang
      rad <- ang * pi / 180
      tibble(name = nm, rank = rk, x = p[2, 1] - offset_m * sin(rad),
             y = p[2, 2] + offset_m * cos(rad), angle = ang, need = need)
    }, error = function(e) NULL)
  })
  if (nrow(lab) == 0) return(NULL)

  # Keep labels whose centers sit inside the ring, then thin out crowded ones.
  cxy <- st_coordinates(center_m)
  if (is.null(shape)) {
    lab <- lab |> filter(sqrt((x - cxy[1])^2 + (y - cxy[2])^2) < radius_m * 0.93 - need / 2)
  } else {
    inner <- st_buffer(shape, -radius_m * 0.05)
    lab <- lab[lengths(st_intersects(st_as_sf(lab, coords = c("x", "y"), crs = crs_m, remove = FALSE), inner)) > 0, ]
  }
  if (nrow(lab) == 0) return(NULL)
  lab <- arrange(lab, rank, desc(need))
  keep <- logical(nrow(lab))
  for (i in seq_len(nrow(lab))) {
    ok <- TRUE
    for (j in which(keep)) {
      d <- sqrt((lab$x[i] - lab$x[j])^2 + (lab$y[i] - lab$y[j])^2)
      if (d < 0.375 * (lab$need[i] + lab$need[j])) { ok <- FALSE; break }
    }
    keep[i] <- ok
  }
  lab[keep, ]
}

# Totals by road role, used by the figure's subtitle and the summary tiles above the map.
road_stats <- function(lines) {
  by_role <- lines |> st_drop_geometry() |> group_by(role) |> summarise(km = sum(km), .groups = "drop") |>
    complete(role, fill = list(km = 0))
  total_km <- sum(by_role$km)
  list(by_role = by_role,
       total_km = total_km,
       local_km = by_role$km[by_role$role == roles$role[5]],
       local_pct = round(100 * by_role$km[by_role$role == roles$role[5]] / total_km),
       through_km = sum(by_role$km[by_role$role %in% roles$role[1:3]]))
}

# ---- Map types ----------------------------------------------------------------
# Every type is drawn from the same downloaded streets, so switching types never needs a new request.
# Block length: a street is cut where it meets another, and each piece is a block, colored by how far it runs between junctions. Streets on the ground are
# cut among themselves and bridges and tunnels among themselves (see network_stats), so an overpass does not shorten a block. Pieces cut by the edge of the map
# come out short, which the caption says.
block_scheme <- function() {
  if (identical(unit_env$u, "mi")) list(breaks_m = c(0, 91.44, 182.88, 365.76, Inf), labels = c("Under 300 ft", "300 to 600 ft", "600 to 1,200 ft", "1,200 ft or more"), short = "600 ft")
  else list(breaks_m = c(0, 100, 200, 400, Inf), labels = c("Under 100 m", "100 to 200 m", "200 to 400 m", "400 m or more"), short = "200 m")
}
block_pieces <- function(lines) {
  if (is.null(lines) || nrow(lines) == 0 || nrow(lines) > max_net_lines) return(NULL)
  tryCatch({
    grade <- is_grade_separated(lines)
    pieces <- function(i) if (!any(i)) NULL else suppressWarnings(st_cast(st_union(st_geometry(lines)[i]), "LINESTRING"))
    g <- do.call(c, Filter(Negate(is.null), list(pieces(!grade), pieces(grade))))
    len <- as.numeric(st_length(g)); keep <- len > 3
    st_sf(len_m = len[keep], geometry = g[keep])
  }, error = function(e) NULL)
}
block_class <- function(len_m) { sc <- block_scheme(); cut(len_m, sc$breaks_m, labels = sc$labels, right = FALSE) }
views <- c(hierarchy = "Road hierarchy", speed = "Speed limits", direction = "Direction", density = "Street density", active = "Bike & walk", blocks = "Block length")
view_blurbs <- c(hierarchy = "Roads by role, from highways to local streets.",
                 speed = "Posted speed limits, where OpenStreetMap has them.",
                 direction = "Which way the streets run, with a compass rose.",
                 density = "Street length per square kilometer, in hexagons.",
                 active = "Sidewalks, bike lanes, and paths, as tagged in OpenStreetMap.",
                 blocks = "Distance between junctions: short blocks are easier to walk.")
get_view <- function(x) {
  x <- as.character(x %||% "hierarchy")[1]
  if (!is.na(x) && x %in% names(views)) x else "hierarchy"
}
# Units: lengths are measured in kilometers everywhere and shown in kilometers or miles. The unit is set for the duration of one
# view_data or make_map call, and every piece of text that shows a length goes through these helpers.
unit_env <- new.env(); unit_env$u <- "km"
km_per_mile <- 1.609344
as_unit <- function(u) if (identical(u, "mi")) "mi" else "km"
uf <- function() if (identical(unit_env$u, "mi")) km_per_mile else 1       # kilometers in one displayed unit
ulab <- function() unit_env$u
uv <- function(km) km / uf()
unit_names <- function(u = unit_env$u) if (identical(u, "mi")) list(len = "miles", area = "square miles") else list(len = "kilometers", area = "square kilometers")
with_units <- function(u, code) { old <- unit_env$u; unit_env$u <- as_unit(u); on.exit(unit_env$u <- old, add = TRUE); force(code) }
# Short distances (hexagon widths): meters, or feet when the map is in miles.
short_len <- function(m) if (identical(unit_env$u, "mi")) paste0(format(round(m * 3.28084 / 10) * 10, big.mark = ","), " ft") else paste0(round(m), " m")
fmt_km <- function(x) paste0(format(round(uv(x)), big.mark = ","), " ", ulab())
radius_txt <- function(radius_m) { x <- radius_m / 1000 / uf(); if (abs(x - round(x)) < 1e-9) round(x) else round(x, 1) }

# One row per straight piece of street: length, midpoint, and direction in degrees clockwise from north
# (0 to 180, because a street has no "forward").
segments_of <- function(lines) {
  empty <- tibble(piece = integer(), len = numeric(), mx = numeric(), my = numeric(), deg = numeric())
  if (nrow(lines) == 0) return(empty)
  m <- st_coordinates(lines)
  n <- nrow(m)
  if (n < 2) return(empty)
  same <- m[-1, "L1"] == m[-n, "L1"]
  dx <- (m[-1, "X"] - m[-n, "X"])[same]; dy <- (m[-1, "Y"] - m[-n, "Y"])[same]
  len <- sqrt(dx^2 + dy^2); keep <- len > 0
  tibble(piece = as.integer(m[-n, "L1"][same][keep]), len = len[keep],
         mx = ((m[-1, "X"] + m[-n, "X"]) / 2)[same][keep], my = ((m[-1, "Y"] + m[-n, "Y"]) / 2)[same][keep],
         deg = (atan2(dx[keep], dy[keep]) * 180 / pi) %% 180)
}
# Average direction of each street piece, weighting by length (doubled angles, so 179 and 1 degrees average to 0).
piece_heading <- function(segs, n_pieces) {
  a <- segs$deg * pi / 90
  cs <- rowsum(segs$len * cos(a), segs$piece); sn <- rowsum(segs$len * sin(a), segs$piece)
  h <- rep(NA_real_, n_pieces)
  h[as.integer(rownames(cs))] <- (atan2(sn[, 1], cs[, 1]) * 90 / pi) %% 180
  h
}
dir_labels <- c("N\u2013S", "NNE\u2013SSW", "NE\u2013SW", "ENE\u2013WSW", "E\u2013W", "ESE\u2013WNW", "SE\u2013NW", "SSE\u2013NNW")
# One row per direction bin for the bar legend next to the compass rose; shares are percentages of street length.
share_bars <- function(sh8) tibble(bin = factor(1:8), lab = factor(dir_labels, levels = rev(dir_labels)), pct = as.numeric(sh8) * 100)
dir_bin <- function(deg) as.integer(floor(((deg + 11.25) %% 180) / 22.5)) + 1L

# Speed limits arrive as "25 mph", "50", "30;50", "RU:urban", "none"... Only plain numbers count.
parse_speed <- function(x) {
  x <- tolower(trimws(as.character(x)))
  num <- suppressWarnings(as.numeric(sub("^([0-9]+([.][0-9]+)?).*$", "\\1", x)))
  num[is.na(x) | !grepl("^[0-9]", x)] <- NA
  num[!is.na(num) & (num <= 0 | num > 200)] <- NA
  list(value = num, mph = !is.na(x) & grepl("mph", x))
}
speed_bins <- list(
  kmh = list(breaks = c(0, 30, 50, 70, 100, Inf), labels = c("up to 30", "31\u201350", "51\u201370", "71\u2013100", "over 100")),
  mph = list(breaks = c(0, 25, 35, 45, 55, Inf), labels = c("up to 25", "30\u201335", "40\u201345", "50\u201355", "over 55")))
unmapped_label <- "No limit mapped"
# Class of each street. Limits are shown in mph if most of the tagged ones are in mph (as in the US), otherwise km/h.
speed_of <- function(maxspeed) {
  p <- parse_speed(maxspeed)
  ok <- !is.na(p$value)
  use_mph <- sum(ok & p$mph) > sum(ok & !p$mph)
  value <- as.numeric(ifelse(p$mph == use_mph, p$value, if (use_mph) p$value / 1.609344 else p$value * 1.609344))
  b <- speed_bins[[if (use_mph) "mph" else "kmh"]]
  cls <- as.character(cut(value, breaks = b$breaks, labels = b$labels, right = TRUE))
  list(class = factor(ifelse(is.na(cls), unmapped_label, cls), levels = c(b$labels, unmapped_label)),
       value = value, unit = if (use_mph) "mph" else "km/h")
}

# Street-density classes (km of street per square kilometer): fixed for every place, not quantiles of one place.
dens_breaks <- c(0, 2, 5, 10, 15, 20, Inf)
dens_labels <- c("under 2", "2\u20135", "5\u201310", "10\u201315", "15\u201320", "20 or more")
# The same classes in miles of street per square mile (round numbers close to the kilometer ones: 3, 8, 16, 24, and 32 are about 2, 5, 10, 15, and 20 per km2).
dens_breaks_mi <- c(0, 3, 8, 16, 24, 32, Inf)
dens_labels_mi <- c("under 3", "3\u20138", "8\u201316", "16\u201324", "24\u201332", "32 or more")
dens_scheme <- function() if (identical(unit_env$u, "mi")) list(breaks = dens_breaks_mi, labels = dens_labels_mi) else list(breaks = dens_breaks, labels = dens_labels)

# Pointy-top hexagons laid over the map; every street segment is assigned to the hexagon holding its midpoint.
hex_density <- function(segs, radius_m, center_m, shape = NULL) {
  cxy <- as.numeric(st_coordinates(center_m)); s <- radius_m / 11       # s = center-to-corner distance
  x <- segs$mx - cxy[1]; y <- segs$my - cxy[2]
  q <- (sqrt(3) / 3 * x - y / 3) / s; r <- (2 / 3 * y) / s; z <- -q - r  # axial -> cube coordinates, then round to the nearest hexagon
  rq <- round(q); rr <- round(r); rz <- round(z)
  dq <- abs(rq - q); dr <- abs(rr - r); dz <- abs(rz - z)
  fq <- dq > dr & dq > dz; fr <- !fq & dr > dz
  rq[fq] <- -rr[fq] - rz[fq]; rr[!fq & !fr] <- -rq[!fq & !fr] - rz[!fq & !fr]
  cells <- tibble(q = rq, r = rr, len = segs$len) |> group_by(q, r) |> summarise(len = sum(len), .groups = "drop")
  cells$x <- cxy[1] + s * sqrt(3) * (cells$q + cells$r / 2); cells$y <- cxy[2] + s * 1.5 * cells$r
  ang <- (30 + 60 * 0:6) * pi / 180
  polys <- lapply(seq_len(nrow(cells)), function(i) st_polygon(list(cbind(cells$x[i] + s * cos(ang), cells$y[i] + s * sin(ang)))))
  hexes <- st_sf(cells, geometry = st_sfc(polys, crs = st_crs(center_m)))
  clipped <- suppressWarnings(st_intersection(hexes, if (is.null(shape)) st_buffer(center_m, radius_m) else shape))
  clipped <- clipped[as.character(st_geometry_type(clipped)) %in% c("POLYGON", "MULTIPOLYGON"), ]
  area <- as.numeric(st_area(clipped))
  clipped$frac <- area / (3 * sqrt(3) / 2 * s^2)
  clipped$dens <- (clipped$len / 1000) / (area / 1e6)                     # km of street per km2 inside the ring
  list(cells = clipped[clipped$frac >= 0.3, ], width_m = s * sqrt(3))      # slivers at the ring's edge are too noisy to shade
}

# Network measures from the street geometry alone. Streets are cut where they cross; a node where three or more street pieces meet is an
# intersection and a node with one is a dead end. Pieces cut by the edge of the map end there without being dead ends, so nodes close to the
# edge are ignored. Crossings are found geometrically; streets tagged as a bridge or tunnel are kept apart from the ones on the ground, so an overpass is not counted as a junction.
max_net_lines <- 40000
# Bridges and tunnels, as tagged in OpenStreetMap ("no" and missing mean the street is on the ground).
is_grade_separated <- function(lines) {
  f <- function(x) { if (is.null(x)) return(rep(FALSE, nrow(lines))); x <- tolower(as.character(x)); !is.na(x) & nzchar(x) & x != "no" }
  f(lines$bridge) | f(lines$tunnel)
}
# Share of street length tagged one-way. Streets without the tag count as two-way, so this is a floor where tagging is thin.
oneway_share <- function(lines) {
  if (is.null(lines$oneway) || nrow(lines) == 0 || sum(lines$km) <= 0) return(NA_real_)
  ow <- tolower(as.character(lines$oneway)) %in% c("yes", "-1", "true", "1", "reverse")
  sum(lines$km[ow]) / sum(lines$km)
}
network_stats <- function(lines, segs, clip, area_km2, edge_m) {
  if (is.null(lines) || nrow(lines) == 0 || nrow(lines) > max_net_lines || !is.finite(area_km2) || area_km2 <= 0) return(NULL)
  res <- tryCatch({
    # A bridge or tunnel crosses other streets without meeting them. Streets on the ground are cut where they cross each other, and bridges and
    # tunnels are cut among themselves, but the two groups are never cut against one another, so a crossing between groups is not a junction.
    grade <- is_grade_separated(lines)
    pieces <- function(i) if (!any(i)) NULL else suppressWarnings(st_cast(st_union(st_geometry(lines)[i]), "LINESTRING"))
    g <- do.call(c, Filter(Negate(is.null), list(pieces(!grade), pieces(grade))))
    m <- st_coordinates(g); first <- !duplicated(m[, "L1"]); last <- !duplicated(m[, "L1"], fromLast = TRUE)
    ends <- rbind(m[first, c("X", "Y"), drop = FALSE], m[last, c("X", "Y"), drop = FALSE])
    key <- paste(round(ends[, 1], 1), round(ends[, 2], 1))
    deg <- table(key); nodes <- names(deg); xy <- do.call(rbind, strsplit(nodes, " ")); storage.mode(xy) <- "double"
    pts <- st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]), coords = c("x", "y"), crs = st_crs(lines))
    away <- as.numeric(st_distance(pts, st_boundary(clip))) > edge_m
    d <- as.integer(deg)[away]
    n_int <- sum(d >= 3); n_dead <- sum(d == 1)
    list(int_per_km2 = n_int / area_km2, n_int = n_int, n_dead = n_dead,
         oneway_share = oneway_share(lines), grade_km = sum(lines$km[grade]), n_grade = sum(grade),
         dead_share = if (n_int + n_dead > 0) n_dead / (n_int + n_dead) else NA_real_)
  }, error = function(e) NULL)
  if (is.null(res)) return(NULL)
  res$order <- orientation_order(segs)
  res
}
# How strongly streets follow one pair of perpendicular directions: 1 for a perfect grid, 0 for streets pointing every way equally.
# Ten-degree bins (cardinal directions at bin centers); the entropy of the length-weighted headings is compared with a perfect grid (two bins) and with no preference (all eighteen).
orientation_order <- function(segs) {
  if (is.null(segs) || nrow(segs) == 0 || sum(segs$len) <= 0) return(NA_real_)
  b <- as.integer(floor(((segs$deg + 5) %% 180) / 10)); w <- tapply(segs$len, factor(b, levels = 0:17), sum); w[is.na(w)] <- 0
  p <- w / sum(w); p <- p[p > 0]; H <- -sum(p * log(p))
  max(0, min(1, 1 - ((H - log(2)) / (log(18) - log(2)))^2))
}
net_tiles <- function(net) {
  if (is.null(net)) return(NULL)
  per <- net$int_per_km2 * uf()^2
  list(list(cls = "s-net", value = format(round(per), big.mark = ","), label = paste0("Intersections per ", if (ulab() == "mi") "square mile" else "square km"),
            note = paste0(format(net$n_int, big.mark = ","), " counted; more means a more walkable grid")),
       list(cls = "s-net", value = if (is.na(net$dead_share)) "n/a" else paste0(round(100 * net$dead_share), "%"), label = "Dead ends",
            note = "share of street ends that are cul-de-sacs"),
       list(cls = "s-net", value = if (is.null(net$oneway_share) || is.na(net$oneway_share)) "n/a" else paste0(round(100 * net$oneway_share), "%"), label = "One-way streets",
            note = if (isTRUE(net$n_grade > 0)) paste0("of street length, as tagged; ", format(net$n_grade, big.mark = ","), " bridge or tunnel pieces are not counted as junctions") else "of street length, as tagged"),
       list(cls = "s-net", value = if (is.na(net$order)) "n/a" else sprintf("%.2f", net$order), label = "Orderliness",
            note = "1 is a perfect grid, 0 has no preferred direction"))
}

center_of <- function(lon, lat, crs_m) st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_m)

# Everything a map type needs to know about the streets: the numbers for its subtitle, tiles, and legend.
view_data <- function(view, lines, radius_m, center_m, shape = NULL, detail = "all", center_txt = NULL, units = "km", net = NULL, paths = NULL) {
  old_u <- unit_env$u; unit_env$u <- as_unit(units); on.exit(unit_env$u <- old_u, add = TRUE)
  vd <- view_data_core(view, lines, radius_m, center_m, shape, center_txt, paths)
  vd$net_tiles <- net_tiles(net)
  if (identical(detail, "all")) return(vd)
  note <- if (identical(detail, "main")) "Local streets (residential and unclassified) are not included at this size, to keep the map fast."
          else "Only highways, arterials, and major collectors are included at this size, to keep the map fast."
  vd$caption <- paste0(vd$caption, "; ", tolower(substr(note, 1, 1)), substring(sub("\\.$", "", note), 2))
  if (vd$view == "hierarchy") {
    vd$subtitle <- paste0(round(uv(vd$stats$total_km)), " ", ulab(), " of through routes ", if (is.null(shape)) paste0("within ", radius_txt(radius_m), " ", ulab(), " of ", center_txt %||% "the center") else "inside the city limits",
                          ": ", round(uv(vd$stats$through_km)), " ", ulab(), " are highways, arterials, and major collectors. ", note)
    vd$tiles[[3]] <- list(cls = "s-local", value = "Not shown", label = "Local streets", note = "left out at this size")
  } else vd$subtitle <- paste(vd$subtitle, note)
  vd
}
view_data_core <- function(view, lines, radius_m, center_m, shape = NULL, center_txt = NULL, paths = NULL) {
  view <- get_view(view); r <- radius_txt(radius_m)
  where <- if (is.null(shape)) paste0("within ", r, " ", ulab(), " of ", center_txt %||% "the center") else "inside the city limits"   # used in subtitles and tiles
  edge <- if (is.null(shape)) "ring" else "boundary"
  scope <- if (is.null(shape)) { if (is.null(center_txt)) paste0("within ", r, " ", ulab(), " of the center of") else paste0("within ", r, " ", ulab(), " of ", center_txt, " in") } else "inside the city limits of"
  total_km <- sum(lines$km)
  tile <- function(cls, value, label, note) list(cls = cls, value = value, label = label, note = note)
  t_total <- tile("s-total", fmt_km(total_km), "Street mapped", where)

  if (view == "hierarchy") {
    s <- road_stats(lines)
    return(list(view = view, scope = scope, noun = "colored by road class", stats = s,
      subtitle = as.character(glue("{round(uv(s$total_km))} {ulab()} of street {where}. ",
                                   "{s$local_pct}% of it is local streets; the highways, arterials, and major collectors ",
                                   "that carry through traffic add up to {round(uv(s$through_km))} {ulab()}.")),
      caption = paste0("roles follow OpenStreetMap's highway classes (highways = motorway and trunk, ",
                       "arterials = primary, major collectors = secondary, minor collectors = tertiary, ",
                       "local = residential and unclassified)"),
      tiles = list(t_total, tile("s-through", fmt_km(s$through_km), "Through routes", "highways, arterials, major collectors"),
                   tile("s-local", paste0(s$local_pct, "%"), "Local streets", paste0(fmt_km(s$local_km), " of residential and unclassified")))))
  }

  if (view == "speed") {
    sp <- speed_of(lines$maxspeed)
    km_by <- tapply(lines$km, sp$class, sum); km_by[is.na(km_by)] <- 0
    mapped_km <- sum(km_by[levels(sp$class)[-length(levels(sp$class))]])
    cov_pct <- round(100 * mapped_km / total_km)
    has <- !is.na(sp$value) & sp$class != unmapped_label
    common <- if (any(has)) { v <- round(sp$value[has]); w <- tapply(lines$km[has], v, sum); as.numeric(names(w)[which.max(w)]) } else NA
    common_pct <- if (any(has)) round(100 * max(w) / mapped_km) else NA
    sub <- if (mapped_km == 0) paste0("OpenStreetMap has no posted speed limits for these ", round(uv(total_km)), " ", ulab(), " of street, so none can be shaded. ",
                                      "Try another map type, or add limits to OpenStreetMap.")
           else as.character(glue("{cov_pct}% of the {round(uv(total_km))} {ulab()} of street {where} has a posted limit in OpenStreetMap. ",
                                  "The most common is {common} {sp$unit}; the gray streets have no limit mapped."))
    return(list(view = view, scope = scope, noun = "colored by posted speed limit", sp = sp, km_by = km_by, unit = sp$unit, subtitle = sub,
      caption = "speed limits are the values tagged in OpenStreetMap; streets without a tag are gray and may still have a limit",
      tiles = list(t_total, tile("s-through", paste0(cov_pct, "%"), "Limits mapped", "share of street length with a posted limit"),
                   tile("s-local", if (is.na(common)) "None" else paste(common, sp$unit), "Most common limit",
                        if (is.na(common)) "no limits mapped" else paste0(common_pct, "% of the streets with a limit")))))
  }

  if (view == "direction") {
    segs <- segments_of(lines); tot <- sum(segs$len)
    sh8 <- tapply(segs$len, factor(dir_bin(segs$deg), levels = 1:8), sum); sh8[is.na(sh8)] <- 0; sh8 <- sh8 / tot
    main <- which.max(sh8)
    off_axis <- abs(((segs$deg + 45) %% 90) - 45)                          # degrees away from the nearest north-south or east-west line
    grid_pct <- round(100 * sum(segs$len[off_axis <= 10]) / tot)
    b36 <- as.integer(floor(((segs$deg + 2.5) %% 180) / 5))
    sh36 <- tapply(segs$len, factor(b36, levels = 0:35), sum); sh36[is.na(sh36)] <- 0; sh36 <- sh36 / tot
    ang5 <- 5 * (0:35)
    rose <- tibble(a = c(ang5, ang5 + 180), h = rep(as.numeric(sh36) / 2, 2), bin = rep(dir_bin(ang5), 2))
    return(list(view = view, scope = scope, noun = "colored by the direction each street runs", segs = segs, rose = rose, sh8 = sh8,
      subtitle = as.character(glue("{round(uv(total_km))} {ulab()} of street {where}. The most common direction is {dir_labels[main]}, ",
                                   "at {round(100 * sh8[main])}% of the length, and {grid_pct}% runs within 10 degrees of north\u2013south or east\u2013west.")),
      caption = "each street is colored by its average heading, weighted by length; opposite directions share a color",
      tiles = list(t_total, tile("s-through", dir_labels[main], "Main direction", paste0(round(100 * sh8[main]), "% of street length")),
                   tile("s-local", paste0(grid_pct, "%"), "Grid-aligned", "within 10\u00b0 of N\u2013S or E\u2013W"))))
  }

  if (view == "active") {
    cls <- active_class(lines); km_by <- tapply(lines$km, cls, sum); km_by[is.na(km_by)] <- 0
    pk <- function(k) if (is.null(paths)) 0 else sum(paths$km[paths$kind == k])
    km_by[["Walking path"]] <- pk(5L); km_by[["Bike path"]] <- pk(6L)
    walk_pct <- round(100 * sum(lines$km[lines$walk_tag]) / total_km); bike_pct <- round(100 * sum(lines$km[lines$bike_tag]) / total_km)
    bike_path_km <- pk(6L); walk_path_km <- pk(5L)
    return(list(view = view, scope = scope, noun = "colored by the sidewalks and bike facilities tagged in OpenStreetMap", km_by = km_by, levels = active_levels, has_paths = !is.null(paths),
      subtitle = as.character(glue("Of {round(uv(total_km))} {ulab()} of street {where}, {walk_pct}% is tagged with a sidewalk and {bike_pct}% with a bike lane or track. ",
                                   "Separate bike paths add {fmt_km(bike_path_km)} and walking paths and sidewalks {fmt_km(walk_path_km)}.")),
      caption = "these are tags volunteers have added, not a survey: a street shown untagged may well have a sidewalk or bike lane, and a shared lane marked only with arrows is not counted",
      tiles = list(t_total, tile("s-through", paste0(walk_pct, "%"), "Sidewalks tagged", "share of street length with a sidewalk tag"),
                   tile("s-local", paste0(bike_pct, "%"), "Bike lanes tagged", paste0("plus ", fmt_km(bike_path_km), " of separate bike paths")))))
  }

  if (view == "blocks") {
    pcs <- block_pieces(lines); sc <- block_scheme()
    if (is.null(pcs) || nrow(pcs) == 0) { pcs <- st_sf(len_m = as.numeric(lines$km) * 1000, geometry = st_geometry(lines)) }     # too many streets to cut: each mapped street counts as one
    pcs$cls <- block_class(pcs$len_m); km_by <- tapply(pcs$len_m / 1000, pcs$cls, sum); km_by[is.na(km_by)] <- 0
    med <- stats::median(pcs$len_m); med_txt <- if (identical(unit_env$u, "mi")) paste0(format(round(med * 3.28084 / 10) * 10, big.mark = ","), " ft") else paste0(round(med / 5) * 5, " m")
    short_pct <- round(100 * sum(pcs$len_m[pcs$len_m < sc$breaks_m[3]]) / sum(pcs$len_m))
    return(list(view = view, scope = scope, noun = "colored by block length", pieces = pcs, km_by = km_by, levels = sc$labels,
      subtitle = as.character(glue("Blocks of street {where} run a median of {med_txt} between junctions. {short_pct}% of the street length is in blocks under {sc$short}, ",
                                   "which tend to be easier to walk.")),
      caption = "a block is the stretch of street between two junctions, measured from the streets as mapped; blocks cut by the edge of the map look shorter than they are",
      tiles = list(t_total, tile("s-through", med_txt, "Median block", "distance between junctions"),
                   tile("s-local", paste0(short_pct, "%"), "Short blocks", paste0("of street length, in blocks under ", sc$short)))))
  }

  # density
  segs <- segments_of(lines)
  hx <- hex_density(segs, radius_m, center_m, shape)
  cells <- hx$cells; d <- cells$dens * uf()                 # miles per square mile is 1.609 times kilometers per square kilometer
  ds <- dens_scheme()
  # Fixed classes in round numbers, the same for every place, so two maps can be compared and each legend entry means something.
  dig <- if (max(d) >= 10) 0 else 1
  lab <- ds$labels
  cells$dens <- d
  cells$cls <- cut(d, breaks = ds$breaks, labels = lab, right = FALSE, include.lowest = TRUE)
  fmt_d <- function(x) format(round(x, dig), nsmall = dig)
  list(view = view, scope = scope, noun = "shaded by street density in hexagons", cells = cells, hex_w = hx$width_m, levels = lab,
       subtitle = as.character(glue("Street density runs from {fmt_d(min(d))} to {fmt_d(max(d))} {ulab()} of street per square {if (ulab() == 'mi') 'mile' else 'kilometer'} across {nrow(cells)} hexagons ",
                                    "about {short_len(hx$width_m)} wide; the median hexagon holds {fmt_d(median(d))}.")),
       caption = paste0("each hexagon is about ", short_len(hx$width_m), " across; density is street length divided by the area inside the ", edge, ", ",
                        "and hexagons mostly outside the ", edge, " are left out; the classes are the same for every place, so maps can be compared"),
       tiles = list(t_total, tile("s-through", paste0(fmt_d(max(d)), " ", ulab(), "/", ulab(), "\u00b2"), "Peak density", paste0(short_len(hx$width_m), " hexagons")),
                    tile("s-local", paste0(fmt_d(median(d)), " ", ulab(), "/", ulab(), "\u00b2"), "Median density", paste0("across ", nrow(cells), " hexagons"))))
}

# Totals by road role, used by the hierarchy subtitle and tiles.
# (road_stats is defined above.)

draw_roles <- function(lines, pal) {
  role_lw <- setNames(roles$lw, roles$role)
  map(rev(roles$role), function(r) {
    d <- filter(lines, role == r)
    if (nrow(d) == 0) return(NULL)
    casing <- if (r != roles$role[5])
      geom_sf(data = d, color = pal$plate, linewidth = role_lw[[r]] + 0.6, lineend = "butt") else NULL
    list(casing, geom_sf(data = d, aes(color = role, linewidth = role), lineend = "butt"))
  })
}

# Build the whole figure: a map type's layers inside the shared frame (title, subtitle, caption, paper).
# A round length for the scale bar: 1, 2, or 5 times a power of ten, no longer than the target.
nice_length <- function(target) {
  if (!is.finite(target) || target <= 0) return(NA_real_)
  p <- 10^floor(log10(target)); m <- c(1, 2, 5, 10) * p; max(m[m <= target * 1.0000001])
}
# Scale bar (bottom left) and north arrow (top right), set in the empty corners of the square the circle sits in, so they never touch a street.
# Every map is drawn with north up, so the arrow is fixed. The bar is in the unit the person chose.
map_furniture <- function(center_m, radius_m, pal) {
  cc <- as.numeric(sf::st_coordinates(center_m)[1, 1:2]); R <- radius_m
  u_len <- nice_length(0.3 * R / 1000 / uf()); if (is.na(u_len)) return(NULL)
  len_m <- u_len * uf() * 1000; x0 <- cc[1] - 0.95 * R; y0 <- cc[2] - 0.93 * R; tick <- 0.02 * R
  lab <- paste(format(u_len, big.mark = ",", scientific = FALSE), ulab())
  ax <- cc[1] + 0.84 * R; ay <- cc[2] + 0.72 * R; hw <- 0.045 * R
  list(annotate("segment", x = x0, xend = x0 + len_m, y = y0, yend = y0, color = pal$ink, linewidth = 0.7, lineend = "butt"),
       annotate("segment", x = c(x0, x0 + len_m), xend = c(x0, x0 + len_m), y = y0 - tick, yend = y0 + tick, color = pal$ink, linewidth = 0.7, lineend = "butt"),
       annotate("text", x = x0, y = y0 + 2.2 * tick, label = lab, hjust = 0, vjust = 0, size = label_mm * 1.05, color = pal$ink, family = body_font),
       annotate("polygon", x = c(ax, ax + hw, ax, ax - hw), y = c(ay + 0.16 * R, ay, ay + 0.055 * R, ay), fill = pal$ink, color = pal$ink, linewidth = 0.2),
       annotate("text", x = ax, y = ay + 0.185 * R, label = "N", hjust = 0.5, vjust = 0, size = label_mm * 1.05, color = pal$ink, family = body_font))
}
make_map <- function(lines, place, lon, lat, radius_m, crs_m, label_roads = TRUE, style = "classic", view = "hierarchy", show_local = TRUE, shape = NULL, inside = NULL, detail = "all", center_txt = NULL, units = "km", paths = NULL, note = NULL) {
  old_u <- unit_env$u; unit_env$u <- as_unit(units); on.exit(unit_env$u <- old_u, add = TRUE)
  pal <- get_style(style); view <- get_view(view)
  center_m <- center_of(lon, lat, crs_m)
  circle <- if (is.null(shape)) st_buffer(center_m, radius_m) else shape
  vd <- view_data(view, lines, radius_m, center_m, shape, detail, center_txt, unit_env$u, paths = paths)
  hide_local <- view == "hierarchy" && !isTRUE(show_local)   # only the drawing changes; the numbers and legend totals still count every street
  drawn <- if (hide_local) filter(lines, role != roles$role[5]) else lines
  legend_theme <- theme(legend.position = "bottom", legend.text = element_text(size = 11, color = pal$muted, margin = margin(l = 3, r = 6)),
                        legend.title = element_text(size = 11, color = pal$muted),
                        legend.key.width = unit(1.0, "cm"), legend.key.height = unit(0.5, "cm"), legend.margin = margin(14, 0, 0, 0), legend.box.margin = margin(0, 0, 0, 0))
  base <- ggplot() + geom_sf(data = circle, fill = pal$plate, color = pal$ring, linewidth = 0.6)
  # Circle with the city outlined: everything outside the boundary is veiled with the paper color, so it stays
  # visible as context but recedes. The veil goes on top of the streets, labels, and hexagons, and the ring is redrawn over it.
  veil <- NULL; dimmed <- FALSE   # dimmed: something outside the city is actually veiled (not so when the city covers the whole circle)
  if (!is.null(inside)) {
    inside_c <- tryCatch(suppressWarnings(st_intersection(st_make_valid(inside), circle)), error = function(e) NULL)
    outside <- tryCatch(suppressWarnings(st_difference(circle, st_make_valid(inside))), error = function(e) NULL)
    if (!is.null(inside_c) && length(inside_c) > 0 && !all(st_is_empty(inside_c))) {
      dimmed <- !is.null(outside) && length(outside) > 0 && !all(st_is_empty(outside))
      veil <- list(
        if (dimmed) geom_sf(data = outside, fill = pal$plate, alpha = if (view %in% c("direction", "density")) 0.84 else 0.74, color = NA),
        geom_sf(data = circle, fill = NA, color = pal$ring, linewidth = 0.6),
        geom_sf(data = inside_c, fill = NA, color = pal$muted, linewidth = 0.45, linetype = "22"))
    }
  }
  # When the map is centered on an address, a small ring marks it.
  marker <- if (!is.null(center_txt)) geom_sf(data = center_m, shape = 21, size = 3.4, stroke = 1.2, fill = pal$plate, color = pal$ink)
  frame <- list(veil, marker, map_furniture(center_m, radius_m, pal), coord_sf(crs = crs_m, datum = NA, expand = FALSE), theme_void(base_family = body_font), legend_theme)
  extra <- NULL

  if (view == "hierarchy") {
    s <- vd$stats; km_by_role <- s$by_role
    present_roles <- as.character(km_by_role$role[km_by_role$km > 0 & !(hide_local & km_by_role$role == roles$role[5])])
    leg_labels <- setNames(glue("{km_by_role$role}  \u00b7  {round(uv(km_by_role$km))} {ulab()}"), as.character(km_by_role$role))
    lab <- if (label_roads) make_labels(lines, radius_m, crs_m, center_m, shape) else NULL
    label_layers <- NULL
    if (!is.null(lab) && nrow(lab) > 0) {
      halo_m <- radius_m * 0.004
      halo <- map_dfr(seq(0, 2 * pi, length.out = 13)[-13], \(a) mutate(lab, x = x + halo_m * cos(a), y = y + halo_m * sin(a)))
      label_layers <- list(
        geom_text(data = halo, aes(x, y, label = name, angle = angle), size = label_mm, color = pal$plate, family = body_font),
        geom_text(data = lab, aes(x, y, label = name, angle = angle), size = label_mm, color = pal$ink, family = body_font))
    }
    map_plot <- base + draw_roles(drawn, pal) + label_layers +
      scale_color_manual(values = setNames(pal$cols, roles$role), labels = leg_labels, name = NULL, limits = present_roles) +
      scale_linewidth_manual(values = setNames(roles$lw, roles$role), labels = leg_labels, name = NULL, limits = present_roles) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) + frame
  } else if (view == "speed") {
    lv <- levels(vd$sp$class); cls_cols <- setNames(c(pal$speed, pal$cols[5]), lv)
    cls_lw <- setNames(c(0.7, 0.95, 1.25, 1.65, 2.1, 0.5), lv)
    lines$sp_class <- vd$sp$class
    present <- lv[vd$km_by[lv] > 0]
    leg_labels <- setNames(glue("{lv}{ifelse(lv == unmapped_label, '', paste0(' ', vd$unit))}  \u00b7  {round(uv(vd$km_by[lv]))} {ulab()}"), lv)
    layers <- map(lv[c(6, 1:5)], function(l) {   # unmapped first, then slow to fast so fast roads sit on top
      d <- filter(lines, sp_class == l)
      if (nrow(d) == 0) return(NULL)
      casing <- if (match(l, lv) >= 3 && l != unmapped_label) geom_sf(data = d, color = pal$plate, linewidth = cls_lw[[l]] + 0.55, lineend = "butt") else NULL
      list(casing, geom_sf(data = d, aes(color = sp_class, linewidth = sp_class), lineend = "butt"))
    })
    map_plot <- base + layers +
      scale_color_manual(values = cls_cols, labels = leg_labels, name = NULL, limits = present, drop = FALSE) +
      scale_linewidth_manual(values = cls_lw, labels = leg_labels, name = NULL, limits = present, drop = FALSE) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) + frame
  } else if (view == "direction") {
    segs <- vd$segs
    lines$bin <- factor(dir_bin(piece_heading(segs, nrow(lines))), levels = 1:8)
    lines <- lines[!is.na(lines$bin), ]
    # Streets that never touch the city are drawn in one quiet neutral, so the colors only mean something where the city is.
    in_city <- if (!is.null(inside)) lengths(suppressWarnings(st_intersects(lines, st_make_valid(inside)))) > 0 else rep(TRUE, nrow(lines))
    layers <- c(list(if (any(!in_city)) geom_sf(data = lines[!in_city, ], color = pal$cols[5], linewidth = 0.55, lineend = "round")),
                list(if (any(in_city)) geom_sf(data = lines[in_city, ], color = pal$plate, linewidth = 1.15, lineend = "round")),   # thin paper-colored edge so crossing streets stay separate
                map(1:8, function(b) { d <- filter(lines[in_city, ], bin == b); if (nrow(d) == 0) NULL else geom_sf(data = d, color = pal$dir[b], linewidth = 0.7, lineend = "round") }))
    map_plot <- base + layers + frame + theme(legend.position = "none")
    rose <- vd$rose; top <- max(rose$h)
    rose_plot <- ggplot(rose, aes(x = a, y = h, fill = factor(bin))) +
      geom_hline(yintercept = top * c(0.5, 1), color = pal$ring, linewidth = 0.35) +
      geom_col(width = 4.2) +
      annotate("text", x = c(0, 90, 180, 270), y = top * 1.3, label = c("N", "E", "S", "W"), family = body_font, size = 4.4, color = pal$muted) +
      scale_fill_manual(values = pal$dir, guide = "none") +
      scale_x_continuous(limits = c(-2.5, 357.5)) + scale_y_continuous(limits = c(0, top * 1.45)) +
      coord_polar(start = 0) + theme_void(base_family = body_font) + theme(plot.margin = margin(0, 0, 0, 0))
    bars <- share_bars(vd$sh8)
    bar_plot <- ggplot(bars, aes(pct, lab, fill = bin)) + geom_col(width = 0.7) +
      geom_text(aes(label = ifelse(pct < 0.5, "<1%", sprintf("%.0f%%", pct))), hjust = -0.2, size = 3.6, color = pal$muted, family = body_font) +
      scale_fill_manual(values = pal$dir, guide = "none") + scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
      labs(title = "Share of street length") +
      theme_void(base_family = body_font) +
      theme(plot.margin = margin(0, 0, 0, 0), axis.text.y = element_text(size = 10, color = pal$muted, hjust = 1, margin = margin(r = 6)),
            plot.title = element_text(size = 11, color = pal$muted, margin = margin(0, 0, 6, 0)))
    extra <- ((rose_plot | bar_plot) + patchwork::plot_layout(widths = c(1, 1.25))) & theme(plot.margin = margin(18, 8, 6, 8))
  } else if (view == "active") {
    lv <- active_levels
    cols <- setNames(c(pal$cols[5], pal$cols[4], pal$cols[2], pal$cols[3], pal$cols[4], pal$cols[2]), lv)          # blue family is walking, orange family is biking, dark is both
    lw <- setNames(c(0.5, 0.85, 0.85, 1.1, 0.8, 0.95), lv); lt <- setNames(c("solid", "solid", "solid", "solid", "22", "22"), lv)
    geoms <- list(st_sf(cls = active_class(lines), geometry = st_geometry(lines)))
    if (!is.null(paths) && nrow(paths) > 0) geoms[[2]] <- st_sf(cls = factor(active_levels[paths$kind], levels = lv), geometry = st_geometry(paths))
    d <- do.call(rbind, geoms); d <- d[order(d$cls), ]                                                       # plain streets first, paths last, so the facilities sit on top
    present <- lv[vd$km_by[lv] > 0]
    leg <- setNames(glue("{lv}  \u00b7  {round(uv(vd$km_by[lv]), 1)} {ulab()}"), lv)
    map_plot <- base + geom_sf(data = d, aes(color = cls, linewidth = cls, linetype = cls), lineend = "round") +
      scale_color_manual(values = cols, labels = leg, name = NULL, limits = present, drop = FALSE) +
      scale_linewidth_manual(values = lw, labels = leg, name = NULL, limits = present, drop = FALSE) +
      scale_linetype_manual(values = lt, labels = leg, name = NULL, limits = present, drop = FALSE) +
      guides(color = guide_legend(ncol = 2, byrow = TRUE)) + frame
  } else if (view == "blocks") {
    lv <- vd$levels
    cols <- setNames(c(pal$dens[6], pal$dens[5], pal$dens[3], pal$dens[2]), lv); lw <- setNames(c(0.85, 0.7, 0.6, 0.55), lv)
    d <- vd$pieces; d <- d[order(d$cls, decreasing = TRUE), ]                                                  # long blocks first, so the short ones sit on top
    present <- lv[vd$km_by[lv] > 0]
    leg <- setNames(glue("{lv}  \u00b7  {round(uv(vd$km_by[lv]), 1)} {ulab()}"), lv)
    map_plot <- base + geom_sf(data = d, aes(color = cls, linewidth = cls), lineend = "round") +
      scale_color_manual(values = cols, labels = leg, name = NULL, limits = present, drop = FALSE) +
      scale_linewidth_manual(values = lw, labels = leg, name = NULL, limits = present, drop = FALSE) +
      guides(color = guide_legend(ncol = 2, byrow = TRUE)) + frame
  } else {
    cells <- vd$cells
    fills <- setNames(pal$dens, vd$levels)                                   # class 1 to 6 always wears the same step, light to dark
    # A class with no hexagons gets no legend key from ggplot, so every class is given an invisible one-meter square: the legend always shows all six.
    cxy <- as.numeric(st_coordinates(center_m))
    keys <- st_sf(cls = factor(vd$levels, levels = vd$levels),
                  geometry = st_sfc(rep(list(st_polygon(list(cbind(cxy[1] + c(0, 1, 1, 0, 0), cxy[2] + c(0, 0, 1, 1, 0))))), length(vd$levels)), crs = st_crs(center_m)))
    map_plot <- base + geom_sf(data = cells, aes(fill = cls), color = pal$plate, linewidth = 0.4) +
      geom_sf(data = keys, aes(fill = cls), alpha = 0, color = NA, show.legend = TRUE) +
      scale_fill_manual(values = fills, limits = vd$levels, name = paste0(ulab(), " of street per ", ulab(), "\u00b2"), drop = FALSE) +
      guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom", title.hjust = 0.5,
                                 override.aes = list(colour = pal$ring, linewidth = 0.25, alpha = 1))) + frame +
      theme(legend.key.width = unit(1.7, "cm"), legend.key.height = unit(0.42, "cm"), legend.key.spacing.x = unit(0, "pt"),
            legend.margin = margin(16, 0, 14, 0), legend.title = element_text(size = 11, color = pal$muted, margin = margin(0, 0, 6, 0)),
            legend.text = element_text(size = 10, color = pal$muted, margin = margin(t = 4)))
  }

  caption <- paste0("Data \u00a9 OpenStreetMap contributors (ODbL), ", format(Sys.Date(), "%Y"), " \u00b7 ", vd$caption,
                    if (!is.null(shape)) "; city limits are the boundary mapped in OpenStreetMap" else "",
                    if (dimmed) "; the dashed line is the city limit mapped in OpenStreetMap, and streets outside it are dimmed"
                    else if (!is.null(veil)) "; the dashed line is the city limit mapped in OpenStreetMap" else "",
                    if (hide_local) paste0("; the ", round(uv(vd$stats$local_km)), " ", ulab(), " of local streets are not drawn") else "",
                    if (!is.null(center_txt)) paste0("; the small circle marks ", center_txt) else "",
                    if (!is.null(note)) paste0("; ", note) else "")
  composite <- if (is.null(extra)) map_plot else (map_plot / extra) + patchwork::plot_layout(heights = c(1, 0.4))
  composite + patchwork::plot_annotation(
    title = escape_markup(place), subtitle = vd$subtitle, caption = caption,
    theme = theme(
      plot.title = element_textbox_simple(family = head_font, size = 38, color = pal$ink, margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_textbox_simple(family = body_font, size = 13, color = pal$muted, lineheight = 1.3, margin = margin(0, 0, 12, 0)),
      plot.caption = element_textbox_simple(family = body_font, size = 8.5, color = pal$faint, lineheight = 1.3, margin = margin(if (view == "density") 22 else 10, 0, 0, 0)),
      plot.margin = margin(30, 34, 22, 34),
      plot.background = element_rect(fill = pal$bg, color = NA)
    ))
}

# Draw a finished figure to a file in any supported format. The same 8 x 10 inch layout is used
# for the on-screen preview and every download, so what you see is what you get.
# ---- Download counts: one line per file handed to a user, with no place names or personal data -------------------------
# The log lives in STREET_MAPS_DATA if set, otherwise in the user's R data folder, otherwise in the session's temp folder.
# It counts files this app generated for download; it cannot know whether the person then saved the file.
data_dir <- function() {
  d <- Sys.getenv("STREET_MAPS_DATA", unset = "")
  if (!nzchar(d)) d <- tryCatch(tools::R_user_dir("street-hierarchy-maps", "data"), error = function(e) "")
  ok <- nzchar(d) && tryCatch({ dir.create(d, recursive = TRUE, showWarnings = FALSE); file.access(d, 2) == 0 }, error = function(e) FALSE)
  if (ok) d else tempdir()
}
counts_file <- function() file.path(data_dir(), "download_log.csv")

log_download <- function(format, view, style, when = Sys.time(), file = counts_file()) {
  if (!isTRUE(format %in% export_formats) || !isTRUE(view %in% names(views)) || !isTRUE(style %in% names(styles))) return(invisible(FALSE))
  new_file <- !file.exists(file)          # decided before cat() opens, and so creates, the file
  ok <- tryCatch({
    cat(if (new_file) "time,format,view,style\n", format(when, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), ",", format, ",", view, ",", style, "\n",
        file = file, sep = "", append = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  invisible(ok)
}

count_cache <- new.env(parent = emptyenv())
read_counts <- function(file = counts_file()) {
  info <- file.info(file); sig <- paste(file, info$mtime, info$size)
  if (!is.na(info$size) && identical(count_cache$sig, sig)) return(count_cache$value)       # unchanged since the last read
  val <- read_counts_raw(file)
  if (!is.na(info$size)) { count_cache$sig <- sig; count_cache$value <- val }
  val
}
read_counts_raw <- function(file) {
  empty <- data.frame(time = as.POSIXct(character(), tz = "UTC"), format = character(), view = character(), style = character())
  if (!file.exists(file)) return(empty)
  d <- tryCatch(utils::read.csv(file, stringsAsFactors = FALSE, colClasses = "character"), error = function(e) NULL)
  if (is.null(d) || !all(c("time", "format", "view", "style") %in% names(d))) return(empty)
  d$time <- as.POSIXct(d$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  d <- d[!is.na(d$time) & d$format %in% export_formats, c("time", "format", "view", "style")]   # skip damaged lines
  if (nrow(d) == 0) empty else d
}

# Totals overall and for rolling windows (rolling windows need no time zone), plus a per-format table.
summarize_counts <- function(d, now = Sys.time()) {
  within <- function(h) sum(d$time > now - h * 3600)
  by_fmt <- vapply(unname(export_formats), function(f) sum(d$format == f), numeric(1))
  last7 <- vapply(unname(export_formats), function(f) sum(d$format == f & d$time > now - 7 * 86400), numeric(1))
  list(total = nrow(d), day = within(24), week = within(24 * 7), first = if (nrow(d)) min(d$time) else NA,
       table = data.frame(format = names(export_formats), all_time = unname(by_fmt), last7 = unname(last7), stringsAsFactors = FALSE))
}

# A finished export is kept for a short while, so downloading the same map and format again is instant.
export_dir <- function() { d <- file.path(tempdir(), "exports"); dir.create(d, showWarnings = FALSE, recursive = TRUE); d }
export_key <- function(...) rlang::hash(list(...))
prune_exports <- function(keep = 12) {
  f <- list.files(export_dir(), full.names = TRUE)
  if (length(f) <= keep) return(invisible(0))
  invisible(sum(file.remove(f[order(file.mtime(f), decreasing = TRUE)][-seq_len(keep)])))
}

# Print sizes. The map is always laid out 8 inches wide, so type and line weights grow with the picture instead of shrinking relative to it; only the
# height changes to match the paper (8 x 10 and 16 x 20 are 0.80, 11 x 14 is 0.79, 18 x 24 is 0.75). Pictures get the pixels for that size at 300 dpi, or fewer dpi
# for the largest sheets so a file stays under about 45 million pixels. PDF and SVG keep the same layout and are sharp at any size.
print_sizes <- list("8x10" = c(8, 10), "11x14" = c(11, 14), "16x20" = c(16, 20), "18x24" = c(18, 24), "24x30" = c(24, 30))
print_size_labels <- c("8 \u00d7 10 inches (default)" = "8x10", "11 \u00d7 14 inches" = "11x14", "16 \u00d7 20 inches" = "16x20", "18 \u00d7 24 inches" = "18x24", "24 \u00d7 30 inches" = "24x30")
max_export_px <- 45e6
print_plan <- function(key = "8x10", format = "png") {
  wh <- print_sizes[[if (isTRUE(key %in% names(print_sizes))) key else "8x10"]]
  layout <- c(8, 8 * wh[2] / wh[1])
  if (format %in% c("pdf", "svg")) return(list(layout = layout, size_in = wh, dpi = NA_real_, px = NULL))
  dpi <- min(print_dpi, floor(sqrt(max_export_px / (wh[1] * wh[2]))))
  list(layout = layout, size_in = wh, dpi = dpi, px = round(wh * dpi))
}
draw_figure <- function(p, file, format = "png", style = "classic", preview = FALSE, size = fig_in, print_key = NULL) {
  if (!is.character(format) || length(format) != 1 || !format %in% export_formats)
    stop("Unsupported file format.")
  pal <- get_style(style)
  dpi <- if (preview) preview_dpi else print_dpi
  w <- size[1]; h <- size[2]
  if (!is.null(print_key) && !preview) {                       # a print size: same 8-inch layout, more pixels per inch
    pl <- print_plan(print_key, format); w <- pl$layout[1]; h <- pl$layout[2]
    if (!is.na(pl$dpi)) dpi <- pl$dpi * pl$size_in[1] / pl$layout[1]
  }
  showtext_opts(dpi = if (format %in% c("pdf", "svg")) vector_dpi else dpi)
  on.exit(showtext_opts(dpi = preview_dpi), add = TRUE)
  switch(format,
    png  = ragg::agg_png(file, width = w, height = h, units = "in", res = dpi, background = pal$bg),
    jpeg = ragg::agg_jpeg(file, width = w, height = h, units = "in", res = dpi, quality = 95, background = pal$bg),
    tiff = ragg::agg_tiff(file, width = w, height = h, units = "in", res = dpi, compression = "lzw", background = pal$bg),
    pdf  = grDevices::cairo_pdf(file, width = w, height = h, bg = pal$bg),
    svg  = grDevices::svg(file, width = w, height = h, bg = pal$bg))
  dev_id <- dev.cur()
  on.exit(if (dev_id %in% dev.list()) dev.off(dev_id), add = TRUE)   # never leave a device open if drawing fails
  print(p)
  dev.off(dev_id)
  invisible(file)
}

# File name stem from a place title: "Elkins, West Virginia" -> "elkins_west_virginia".
file_slug <- function(x) {
  x <- name_key(as.character(x %||% "")[1])
  x <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", x))
  if (is.na(x) || !nzchar(x)) "map" else substr(x, 1, 40)
}

# Plain-language description of the figure for screen readers.
fig_alt <- function(place, vd, radius_m) {
  scope <- vd$scope %||% paste0("within ", radius_txt(radius_m), " ", ulab(), " of the center of")
  paste0("Map of the streets ", scope, " ", place, ", ", vd$noun, ". ", vd$subtitle)
}

# The title is drawn by ggtext, which interprets Markdown and HTML (including <img src=...>, which
# can embed a local file). Place names come from outside data, so every special character is escaped
# and the name is always shown literally.
escape_markup <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("([\\\\`*_{}\\[\\]()#+!~|])", "\\\\\\1", x, perl = TRUE)
}

# Short title from a Nominatim display name: "Elkins, Randolph County, West Virginia, USA" -> "Elkins, West Virginia"
short_title <- function(label) {
  parts <- str_trim(str_split(label, ",")[[1]])
  if (length(parts) <= 2) return(paste(parts, collapse = ", "))
  paste(parts[1], parts[length(parts) - 1], sep = ", ")
}

# ---- UI ---------------------------------------------------------------------
# Fonts: the same families as the map. If Google Fonts cannot be reached, the system fonts are used.
make_theme <- function(web_fonts) {
  bs_theme(
    version = 5, bg = "#f4f1e9", fg = "#1c1f24", primary = "#1f3a5a", secondary = "#5a626b",
    "border-radius" = "0.65rem", "btn-border-radius" = "0.65rem", "input-border-radius" = "0.55rem",
    base_font = if (web_fonts) font_google("Fira Sans", local = TRUE) else font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"),
    heading_font = if (web_fonts) font_google("Oswald", local = TRUE) else font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif")
  )
}
# Built once, on first use; the compile step is where a failed font download would show up.
get_theme <- local({
  th <- NULL
  function() {
    if (is.null(th)) th <<- tryCatch({ t <- make_theme(fonts_ok); bs_theme_dependencies(t); t },
                                     error = function(e) make_theme(FALSE))
    th
  }
})

# A quiet copy of the icon, a plan of the Konigsberg islands inside a circle like the map itself, sits behind the page until a map is drawn.
watermark_svg <- r"---(<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><defs><clipPath id="wm"><circle cx="256" cy="256" r="250"/></clipPath></defs><g clip-path="url(#wm)"><defs><linearGradient id="wwat" gradientUnits="userSpaceOnUse" x1="0" y1="90" x2="512" y2="430"><stop offset="0" stop-color="#cddbdc"/><stop offset="1" stop-color="#bccfd3"/></linearGradient><clipPath id="wca"><polygon points="80.0,256.0 83.1,237.8 88.0,221.2 94.3,206.1 102.1,192.7 111.1,180.8 121.3,170.5 132.4,161.8 144.3,154.7 156.9,149.1 170.0,145.2 183.5,142.8 197.2,142.0 211.1,142.8 224.9,145.2 238.5,149.1 251.7,154.7 264.5,161.8 276.6,170.5 287.9,180.8 298.4,192.7 307.7,206.1 315.9,221.2 322.7,237.8 328.0,256.0 328.0,256.0 322.7,274.2 315.9,290.8 307.7,305.9 298.4,319.3 287.9,331.2 276.6,341.5 264.5,350.2 251.7,357.3 238.5,362.9 224.9,366.8 211.1,369.2 197.2,370.0 183.5,369.2 170.0,366.8 156.9,362.9 144.3,357.3 132.4,350.2 121.3,341.5 111.1,331.2 102.1,319.3 94.3,305.9 88.0,290.8 83.1,274.2 80.0,256.0"/></clipPath><clipPath id="wcd"><polygon points="314.0,256.0 318.7,255.5 323.1,254.2 327.4,252.0 331.6,249.1 335.8,245.5 340.0,241.3 344.2,236.6 348.6,231.4 353.2,225.9 358.0,220.0 363.1,213.9 368.6,207.6 374.5,201.2 380.8,194.8 387.7,188.5 395.2,182.3 403.3,176.2 412.1,170.5 421.6,165.0 432.0,160.0 445.2,154.0 458.8,147.4 472.9,140.3 487.6,132.7 502.8,124.6 518.6,116.1 535.2,107.2 552.4,98.0 570.5,88.4 589.4,78.5 609.2,68.4 630.0,58.0 640.0,100.0 640.0,412.0 630.0,454.0 609.2,443.6 589.4,433.5 570.5,423.6 552.4,414.0 535.2,404.8 518.6,395.9 502.8,387.4 487.6,379.3 472.9,371.7 458.8,364.6 445.2,358.0 432.0,352.0 421.6,347.0 412.1,341.5 403.3,335.8 395.2,329.7 387.7,323.5 380.8,317.2 374.5,310.8 368.6,304.4 363.1,298.1 358.0,292.0 353.2,286.1 348.6,280.6 344.2,275.4 340.0,270.7 335.8,266.5 331.6,262.9 327.4,260.0 323.1,257.8 318.7,256.5 314.0,256.0"/></clipPath></defs><rect width="512" height="512" fill="#efece1"/><g><path d="M-77 317L-35 311L-29 352L-70 359ZM-68 371L-27 364L-20 406L-62 412ZM-60 424L-18 418L-12 459L-53 466ZM-74 -11L-33 -18L-26 24L-67 30ZM-66 42L-24 36L-18 77L-59 84ZM-49 149L-7 142L-1 184L-42 190ZM-40 202L1 196L8 237L-34 244ZM-23 309L18 302L25 344L-17 350ZM-6 416L35 409L42 451L0 457ZM10 522L52 516L58 557L17 564ZM-4 87L38 81L44 122L3 129ZM22 247L63 241L70 282L28 289ZM47 407L88 401L95 442L53 449ZM55 461L97 454L103 495L62 502ZM33 -28L74 -34L81 7L39 14ZM75 239L116 232L123 274L81 280ZM92 345L133 339L140 380L98 387ZM117 505L159 499L165 540L124 547ZM86 -36L127 -43L134 -1L93 5ZM94 17L136 10L142 52L101 58ZM111 124L153 117L159 159L118 165ZM128 230L170 224L176 265L135 272ZM137 284L178 277L185 319L143 325ZM145 337L187 330L193 372L152 378ZM154 390L195 384L202 425L160 432ZM190 275L231 269L238 310L197 317ZM193 -53L234 -60L241 -18L199 -12ZM218 107L259 100L266 142L225 148ZM226 160L268 154L274 195L233 202ZM252 320L293 314L300 355L258 362ZM269 427L310 420L317 462L275 468ZM277 480L319 474L325 515L284 522ZM286 533L327 527L334 568L292 575ZM263 45L304 38L311 80L269 86ZM280 152L321 145L328 187L286 193ZM297 258L338 252L345 293L303 300ZM322 418L363 412L370 453L329 460ZM339 525L380 518L387 560L345 566ZM299 -70L341 -77L347 -35L306 -29ZM316 37L358 30L364 71L323 78ZM325 90L366 83L373 125L331 131ZM333 143L375 137L381 178L340 185ZM350 250L391 243L398 285L357 291ZM358 303L400 297L406 338L365 345ZM375 410L417 403L423 445L382 451ZM361 -25L403 -32L409 10L368 16ZM370 28L411 22L418 63L376 70ZM378 81L419 75L426 116L385 123ZM429 401L470 395L477 436L435 443ZM437 455L479 448L485 490L444 496ZM414 -34L456 -40L462 1L421 8ZM423 20L464 13L471 55L429 61ZM440 126L481 120L488 161L446 168ZM448 180L490 173L496 215L455 221ZM457 233L498 226L505 268L463 274ZM465 286L507 280L513 321L472 328ZM482 393L523 386L530 428L489 434ZM468 -42L509 -49L516 -7L474 -1ZM485 65L526 58L533 99L491 106ZM502 171L543 165L550 206L508 213ZM527 331L568 325L575 366L533 373ZM535 385L577 378L583 419L542 426ZM538 56L579 50L586 91L545 98Z" fill="#e4dfcd" stroke="#e4dfcd" stroke-width="5" stroke-linejoin="round"/><path d="M-51 477L-10 471L-3 512L-45 519ZM-43 531L-1 524L5 566L-36 572ZM2 469L43 462L50 504L9 511ZM-21 -19L21 -26L27 15L-14 22ZM5 141L46 134L53 175L11 182ZM13 194L55 187L61 229L20 235ZM30 301L71 294L78 335L37 342ZM38 354L80 347L86 389L45 395ZM41 25L83 19L89 60L48 67ZM58 132L99 126L106 167L65 174ZM120 177L161 170L168 212L126 218ZM162 444L203 437L210 479L169 485ZM170 497L212 490L218 532L177 538ZM148 9L189 2L196 43L154 50ZM182 222L223 215L230 257L188 263ZM224 489L265 482L272 523L230 530ZM232 542L274 535L280 577L239 583ZM210 53L251 47L258 88L216 95ZM246 -62L287 -68L294 -27L253 -20ZM271 98L313 92L319 133L278 140ZM288 205L330 198L336 240L295 246ZM308 -17L349 -23L356 18L314 25ZM342 197L383 190L390 231L348 238ZM386 135L428 128L434 170L393 176ZM395 188L436 182L443 223L401 230ZM403 241L445 235L451 276L410 283ZM412 295L453 288L460 330L418 336ZM446 508L487 502L494 543L452 550ZM474 340L515 333L522 375L480 381ZM490 446L532 440L539 481L497 488ZM499 500L540 493L547 535L505 541ZM476 11L518 5L524 46L483 53ZM493 118L535 111L541 153L500 159ZM510 225L551 218L558 259L517 266ZM518 278L560 271L566 313L525 319ZM521 -51L563 -57L569 -16L528 -9Z" fill="#e1dbc6" stroke="#e1dbc6" stroke-width="5" stroke-linejoin="round"/><path d="M-57 96L-16 89L-9 131L-51 137ZM-15 362L27 356L33 397L-8 404ZM-12 34L29 27L36 69L-6 75ZM64 514L105 507L112 549L70 555ZM50 79L91 72L98 114L56 120ZM66 185L108 179L114 220L73 227ZM83 292L125 286L131 327L90 334ZM109 452L150 446L157 487L115 494ZM139 -45L181 -51L187 -10L146 -3ZM156 62L198 55L204 97L163 103ZM165 115L206 109L213 150L171 157ZM198 329L240 322L246 363L205 370ZM215 435L257 429L263 470L222 477ZM201 0L243 -6L249 35L208 42ZM235 213L276 207L283 248L241 255ZM260 373L302 367L308 408L267 415ZM254 -8L296 -15L302 27L261 33ZM305 312L347 305L353 347L312 353ZM367 357L408 350L415 391L373 398ZM392 517L434 510L440 551L399 558Z" fill="#e8e4d4" stroke="#e8e4d4" stroke-width="5" stroke-linejoin="round"/><path d="M-32 256L10 249L16 291L-25 297ZM100 399L142 392L148 434L107 440ZM103 70L144 64L151 105L109 112ZM173 169L215 162L221 203L180 210ZM207 382L248 375L255 417L213 423ZM243 267L285 260L291 302L250 308ZM314 365L355 358L362 400L320 406ZM330 472L372 465L378 507L337 513ZM384 463L425 457L432 498L390 505ZM420 348L462 342L468 383L427 390ZM431 73L473 66L479 108L438 114ZM530 3L571 -4L578 38L536 44Z" fill="#dbe3cc" stroke="#dbe3cc" stroke-width="5" stroke-linejoin="round"/></g><g clip-path="url(#wca)"><polygon points="80.0,256.0 83.1,237.8 88.0,221.2 94.3,206.1 102.1,192.7 111.1,180.8 121.3,170.5 132.4,161.8 144.3,154.7 156.9,149.1 170.0,145.2 183.5,142.8 197.2,142.0 211.1,142.8 224.9,145.2 238.5,149.1 251.7,154.7 264.5,161.8 276.6,170.5 287.9,180.8 298.4,192.7 307.7,206.1 315.9,221.2 322.7,237.8 328.0,256.0 328.0,256.0 322.7,274.2 315.9,290.8 307.7,305.9 298.4,319.3 287.9,331.2 276.6,341.5 264.5,350.2 251.7,357.3 238.5,362.9 224.9,366.8 211.1,369.2 197.2,370.0 183.5,369.2 170.0,366.8 156.9,362.9 144.3,357.3 132.4,350.2 121.3,341.5 111.1,331.2 102.1,319.3 94.3,305.9 88.0,290.8 83.1,274.2 80.0,256.0" fill="#efece1"/><path d="M-17 -42L2 -32L-8 -12L-27 -22ZM-33 -11L-13 -1L-23 18L-43 8ZM13 -26L33 -16L23 3L3 -7ZM-18 34L2 44L-8 64L-28 54ZM13 50L32 60L22 79L3 69ZM58 35L78 45L68 64L48 54ZM-19 186L1 196L-9 216L-29 206ZM135 -41L155 -31L145 -11L125 -21ZM104 20L124 30L114 50L94 40ZM150 5L169 15L159 35L140 25ZM103 96L123 106L113 126L93 116ZM-35 369L-16 379L-26 398L-45 388ZM195 -10L215 0L205 20L186 10ZM87 202L107 212L97 232L77 222ZM26 324L45 333L35 353L16 343ZM-21 414L-1 424L-11 444L-31 434ZM241 -25L261 -15L251 5L231 -5ZM149 157L168 167L158 187L139 177ZM56 339L76 349L66 369L46 359ZM25 400L45 410L35 429L15 419ZM10 430L29 440L19 459L0 449ZM256 21L276 31L266 51L246 41ZM102 324L121 334L111 354L92 344ZM302 6L321 16L311 36L292 26ZM147 309L167 319L157 339L137 329ZM117 370L136 380L126 399L107 389ZM101 400L121 410L111 430L91 420ZM255 173L275 183L265 203L245 193ZM131 416L151 426L141 445L121 435ZM378 7L397 17L387 36L368 26ZM332 98L351 108L341 127L322 117ZM316 128L336 138L326 158L306 148ZM301 158L320 168L310 188L291 178ZM285 189L305 199L295 218L275 208ZM270 219L289 229L279 249L260 239ZM239 280L259 289L249 309L229 299ZM193 370L212 380L202 400L183 390ZM177 401L197 411L187 430L167 420ZM131 492L150 502L140 521L121 511ZM115 522L135 532L125 551L105 541ZM393 53L412 63L402 82L383 72ZM346 143L366 153L356 173L336 163ZM331 174L351 184L341 203L321 193ZM300 234L320 244L310 264L290 254ZM285 265L304 275L294 294L275 284ZM207 416L227 426L217 446L197 436ZM392 129L412 139L402 158L382 148ZM377 159L396 169L386 188L367 179ZM361 189L381 199L371 219L351 209ZM330 250L350 260L340 279L320 269ZM315 280L335 290L325 310L305 300ZM300 310L319 320L309 340L290 330ZM269 371L288 381L278 401L259 391ZM253 401L273 411L263 431L243 421ZM238 432L257 442L247 461L228 451ZM222 462L242 472L232 491L212 481ZM500 -7L519 3L509 22L490 12ZM469 53L488 63L478 83L459 73ZM453 83L473 93L463 113L443 103ZM268 447L288 457L278 477L258 467ZM253 477L272 487L262 507L243 497ZM391 281L411 291L401 310L381 300ZM329 402L349 412L339 431L319 421ZM529 84L549 94L539 114L519 104ZM483 175L503 185L493 205L473 195ZM468 205L487 215L477 235L458 225ZM421 296L441 306L431 326L411 316ZM344 448L364 458L354 477L334 467ZM329 478L348 488L338 507L319 497ZM529 160L548 170L538 190L519 180ZM374 463L394 473L384 493L364 483ZM405 478L424 488L414 508L395 498ZM512 342L532 352L522 372L502 362ZM420 524L439 534L429 554L410 544ZM526 464L546 474L536 494L516 484ZM496 525L515 535L505 554L486 544Z" fill="#e1dbc6" stroke="#e1dbc6" stroke-width="5" stroke-linejoin="round"/><path d="M-2 4L17 14L7 34L-12 24ZM28 19L48 29L38 49L18 39ZM11 202L31 212L21 231L1 221ZM-35 293L-15 303L-25 322L-45 312ZM119 66L138 76L129 95L109 85ZM88 126L108 136L98 156L78 146ZM57 263L76 273L66 293L47 283ZM179 97L199 107L189 126L169 116ZM118 218L137 228L127 247L108 237ZM-37 521L-17 531L-27 550L-47 540ZM86 354L106 364L96 384L76 374ZM40 445L60 455L50 475L30 465ZM132 340L152 350L142 369L122 359ZM332 22L352 32L342 51L322 41ZM301 82L321 92L311 112L291 102ZM162 355L182 365L172 385L152 375ZM147 385L166 395L156 415L137 405ZM146 461L166 471L156 491L136 481ZM192 446L212 456L202 476L182 466ZM177 477L196 487L186 506L167 496ZM423 68L443 78L433 98L413 88ZM314 432L333 442L323 462L304 452ZM452 236L472 245L462 265L442 255ZM437 266L456 276L446 295L427 285ZM436 342L456 352L446 371L426 361ZM405 402L425 412L415 432L395 422ZM344 524L363 534L353 553L334 543ZM482 327L501 337L491 357L472 347ZM435 494L455 504L445 523L425 513ZM481 479L500 489L490 509L471 499Z" fill="#dbe3cc" stroke="#dbe3cc" stroke-width="5" stroke-linejoin="round"/><path d="M-33 65L-14 75L-24 94L-43 84ZM-3 80L17 90L7 110L-13 100ZM-3 156L16 166L6 186L-13 176ZM58 111L77 121L67 140L48 130ZM-4 232L16 242L6 262L-14 252ZM73 157L92 167L82 186L63 176ZM149 81L169 91L159 111L139 101ZM10 354L30 364L20 383L0 373ZM133 187L153 197L143 217L123 207ZM-6 460L14 470L4 490L-16 480ZM241 51L260 61L250 81L231 71ZM210 112L229 122L219 142L200 132ZM286 37L306 47L296 66L276 56ZM225 158L244 168L234 187L215 177ZM178 249L198 259L188 278L168 268ZM39 521L59 531L49 551L29 541ZM317 52L336 62L326 82L307 72ZM362 37L382 47L372 67L352 57ZM362 113L381 123L371 143L352 133ZM269 295L289 305L279 325L259 315ZM238 356L258 366L248 385L228 375ZM223 386L243 396L233 415L213 405ZM438 38L458 48L448 67L428 57ZM530 8L549 18L540 38L520 28ZM437 190L457 200L447 219L427 209ZM298 462L318 472L308 492L288 482ZM267 523L287 533L277 553L257 543ZM390 357L410 367L400 386L380 376ZM513 190L533 200L523 220L503 210ZM390 433L409 443L399 462L380 452ZM497 373L516 383L506 402L487 392Z" fill="#e8e4d4" stroke="#e8e4d4" stroke-width="5" stroke-linejoin="round"/><path d="M59 -41L78 -31L68 -12L49 -22ZM43 -11L63 -1L53 19L33 9ZM-18 110L1 120L-9 140L-28 130ZM-34 141L-14 151L-24 170L-44 160ZM89 -26L109 -16L99 4L79 -6ZM74 5L93 15L83 34L64 24ZM43 65L62 75L52 95L33 85ZM27 95L47 105L37 125L17 115ZM12 126L32 136L22 155L2 145ZM-34 217L-15 227L-25 246L-44 236ZM119 -10L139 0L129 19L109 9ZM89 50L108 60L98 80L79 70ZM73 81L93 91L83 110L63 100ZM42 141L62 151L52 171L32 161ZM27 171L46 181L36 201L17 191ZM-19 262L0 272L-10 292L-29 282ZM165 -25L185 -15L175 4L155 -6ZM134 35L154 45L144 65L124 55ZM57 187L77 197L67 216L47 206ZM42 217L61 227L51 247L32 237ZM26 247L46 257L36 277L16 267ZM11 278L30 288L20 307L1 297ZM-5 308L15 318L5 338L-15 328ZM-20 338L0 348L-10 368L-30 358ZM211 -40L231 -30L221 -10L201 -20ZM180 21L200 31L190 50L170 40ZM165 51L184 61L174 80L155 70ZM134 111L153 121L143 141L124 131ZM118 142L138 152L128 171L108 161ZM103 172L122 182L112 202L93 192ZM72 233L92 243L82 262L62 252ZM41 293L61 303L51 323L31 313ZM-5 384L14 394L4 414L-15 404ZM-36 445L-16 455L-26 474L-46 464ZM226 6L245 16L235 35L216 25ZM210 36L230 46L220 66L200 56ZM195 66L215 76L205 96L185 86ZM164 127L184 137L174 156L154 146ZM102 248L122 258L112 278L92 268ZM87 278L106 288L96 308L77 298ZM71 309L91 319L81 338L61 328ZM41 369L60 379L50 399L31 389ZM-21 490L-2 500L-12 520L-31 510ZM287 -39L307 -29L297 -10L277 -20ZM272 -9L291 1L281 20L262 10ZM225 82L245 92L235 111L215 101ZM194 142L214 152L204 172L184 162ZM179 173L199 183L189 202L169 192ZM163 203L183 213L173 232L153 223ZM148 233L168 243L158 263L138 253ZM133 263L152 273L142 293L123 283ZM117 294L137 304L127 323L107 313ZM71 385L90 395L80 414L61 404ZM55 415L75 425L65 445L45 435ZM25 476L44 486L34 505L15 495ZM9 506L29 516L19 535L-1 525ZM317 -24L337 -14L327 6L307 -4ZM271 67L291 77L281 96L261 86ZM256 97L275 107L265 127L246 117ZM240 127L260 137L250 157L230 147ZM209 188L229 198L219 218L199 208ZM194 218L213 228L203 248L184 238ZM163 279L182 289L173 309L153 299ZM86 430L105 440L95 460L76 450ZM70 461L90 471L80 490L60 480ZM55 491L74 501L64 521L45 511ZM363 -39L383 -29L373 -9L353 -19ZM348 -9L367 1L357 21L338 11ZM286 113L305 123L295 142L276 132ZM270 143L290 153L280 172L260 162ZM239 203L259 213L249 233L230 223ZM224 234L244 244L234 263L214 253ZM209 264L228 274L218 294L199 284ZM193 294L213 304L203 324L183 314ZM178 325L197 335L187 354L168 344ZM116 446L136 456L126 475L106 465ZM101 476L120 486L110 506L91 496ZM85 506L105 516L95 536L75 526ZM393 -23L413 -13L403 6L383 -4ZM347 67L367 77L357 97L337 87ZM254 249L274 259L264 279L244 269ZM223 310L243 320L233 339L213 329ZM208 340L228 350L218 370L198 360ZM162 431L181 441L171 461L152 451ZM439 -38L459 -28L449 -9L429 -19ZM424 -8L443 2L433 22L414 12ZM408 22L428 32L418 52L398 42ZM377 83L397 93L387 112L367 102ZM316 204L335 214L325 234L306 224ZM254 325L273 335L263 355L244 345ZM161 507L181 517L171 537L151 527ZM469 -23L489 -13L479 7L459 -3ZM454 7L473 17L463 37L444 27ZM408 98L427 108L417 128L398 118ZM346 219L365 229L355 249L336 239ZM284 341L304 351L294 370L274 360ZM207 492L226 502L217 522L197 512ZM191 522L211 532L201 552L181 542ZM515 -38L535 -28L525 -8L505 -18ZM484 23L504 33L494 52L474 42ZM438 114L457 124L447 143L428 133ZM422 144L442 154L432 174L412 164ZM407 174L427 184L417 204L397 194ZM392 205L411 215L401 234L382 224ZM376 235L396 245L386 265L366 255ZM361 265L380 275L370 295L351 285ZM345 296L365 306L355 325L335 315ZM330 326L349 336L339 355L320 345ZM314 356L334 366L324 386L304 376ZM299 386L319 396L309 416L289 406ZM283 417L303 427L293 446L274 436ZM237 508L257 518L247 537L227 527ZM514 38L534 48L524 68L504 58ZM499 69L519 79L509 98L489 88ZM484 99L503 109L493 128L474 118ZM468 129L488 139L478 159L458 149ZM453 159L472 169L462 189L443 179ZM422 220L441 230L431 250L412 240ZM406 250L426 260L416 280L396 270ZM376 311L395 321L385 341L366 331ZM360 341L380 351L370 371L350 361ZM345 372L364 382L354 401L335 391ZM283 493L303 503L293 522L273 512ZM514 114L533 124L523 144L504 134ZM498 145L518 155L508 174L488 164ZM406 326L425 336L415 356L396 346ZM375 387L395 397L385 417L365 407ZM360 417L379 427L369 447L350 437ZM313 508L333 518L323 538L303 528ZM498 221L517 231L507 250L488 240ZM482 251L502 261L492 281L472 271ZM467 281L487 291L477 311L457 301ZM452 312L471 322L461 341L442 331ZM421 372L440 382L430 402L411 392ZM359 493L379 503L369 523L349 513ZM528 236L548 246L538 266L518 256ZM513 266L532 276L522 296L503 286ZM497 297L517 307L507 326L487 316ZM466 357L486 367L476 387L456 377ZM451 388L471 398L461 417L441 407ZM436 418L455 428L445 447L426 437ZM420 448L440 458L430 478L410 468ZM389 509L409 519L399 538L379 528ZM528 312L547 322L537 342L518 332ZM481 403L501 413L491 433L471 423ZM466 433L485 443L475 463L456 453ZM450 464L470 474L460 493L440 483ZM527 388L547 398L537 418L517 408ZM512 418L531 428L521 448L502 438ZM496 449L516 459L506 478L486 468ZM465 509L485 519L475 539L455 529ZM511 494L531 504L521 524L501 514Z" fill="#e4dfcd" stroke="#e4dfcd" stroke-width="5" stroke-linejoin="round"/></g><g clip-path="url(#wcd)"><polygon points="314.0,256.0 318.7,255.5 323.1,254.2 327.4,252.0 331.6,249.1 335.8,245.5 340.0,241.3 344.2,236.6 348.6,231.4 353.2,225.9 358.0,220.0 363.1,213.9 368.6,207.6 374.5,201.2 380.8,194.8 387.7,188.5 395.2,182.3 403.3,176.2 412.1,170.5 421.6,165.0 432.0,160.0 445.2,154.0 458.8,147.4 472.9,140.3 487.6,132.7 502.8,124.6 518.6,116.1 535.2,107.2 552.4,98.0 570.5,88.4 589.4,78.5 609.2,68.4 630.0,58.0 640.0,100.0 640.0,412.0 630.0,454.0 609.2,443.6 589.4,433.5 570.5,423.6 552.4,414.0 535.2,404.8 518.6,395.9 502.8,387.4 487.6,379.3 472.9,371.7 458.8,364.6 445.2,358.0 432.0,352.0 421.6,347.0 412.1,341.5 403.3,335.8 395.2,329.7 387.7,323.5 380.8,317.2 374.5,310.8 368.6,304.4 363.1,298.1 358.0,292.0 353.2,286.1 348.6,280.6 344.2,275.4 340.0,270.7 335.8,266.5 331.6,262.9 327.4,260.0 323.1,257.8 318.7,256.5 314.0,256.0" fill="#efece1"/><path d="M-51 267L-27 257L-16 280L-40 291ZM-35 302L-12 291L-1 315L-25 326ZM-4 371L19 361L30 384L6 395ZM11 406L35 395L45 419L22 430ZM26 441L50 430L61 454L37 464ZM42 475L66 465L76 489L52 499ZM57 510L81 500L92 523L68 534ZM-47 182L-23 172L-13 195L-36 206ZM-32 217L-8 206L3 230L-21 241ZM-16 252L8 241L18 265L-6 275ZM-1 286L23 276L34 300L10 310ZM15 321L39 311L49 334L25 345ZM46 391L69 380L80 404L56 414ZM61 425L85 415L96 438L72 449ZM-43 97L-19 87L-9 111L-33 121ZM-12 167L11 156L22 180L-2 191ZM19 236L42 226L53 249L29 260ZM34 271L58 260L68 284L45 295ZM50 306L73 295L84 319L60 329ZM65 340L89 330L99 354L76 364ZM96 410L120 399L130 423L106 434ZM142 514L166 503L177 527L153 538ZM-55 -22L-31 -33L-21 -9L-44 2ZM-39 12L-16 2L-5 26L-29 36ZM-24 47L0 37L10 60L-13 71ZM-9 82L15 71L26 95L2 106ZM22 151L46 141L57 165L33 175ZM38 186L62 175L72 199L48 210ZM69 255L93 245L103 269L79 279ZM84 290L108 280L119 303L95 314ZM131 394L154 384L165 408L141 418ZM162 464L185 453L196 477L172 488ZM-20 -38L4 -48L14 -25L-10 -14ZM11 32L35 21L45 45L21 55ZM26 66L50 56L61 80L37 90ZM57 136L81 125L91 149L68 160ZM73 171L96 160L107 184L83 194ZM88 205L112 195L122 218L99 229ZM165 379L189 368L200 392L176 403ZM227 518L251 507L261 531L238 541ZM61 51L85 40L95 64L72 75ZM92 120L116 110L126 134L102 144ZM169 294L193 283L203 307L180 318ZM185 329L208 318L219 342L195 352ZM215 398L239 388L250 411L226 422ZM231 433L255 422L265 446L242 457ZM246 468L270 457L281 481L257 491ZM262 502L286 492L296 515L272 526ZM277 537L301 526L312 550L288 561ZM80 1L104 -10L115 14L91 25ZM96 36L119 25L130 49L106 59ZM111 70L135 60L145 83L122 94ZM142 140L166 129L176 153L153 163ZM157 174L181 164L192 188L168 198ZM173 209L197 199L207 222L183 233ZM235 348L259 337L269 361L245 372ZM250 383L274 372L285 396L261 406ZM266 417L289 407L300 431L276 441ZM312 522L336 511L346 535L323 545ZM115 -15L139 -25L149 -1L125 9ZM130 20L154 10L165 33L141 44ZM146 55L170 44L180 68L156 79ZM161 90L185 79L196 103L172 113ZM192 159L216 148L227 172L203 183ZM208 194L231 183L242 207L218 217ZM223 228L247 218L257 242L234 252ZM239 263L262 253L273 276L249 287ZM300 402L324 391L335 415L311 426ZM347 506L370 496L381 519L357 530ZM196 74L220 63L230 87L207 98ZM211 109L235 98L246 122L222 133ZM227 143L251 133L261 157L237 167ZM258 213L282 202L292 226L268 237ZM273 248L297 237L308 261L284 271ZM289 282L312 272L323 296L299 306ZM304 317L328 306L339 330L315 341ZM320 352L343 341L354 365L330 376ZM335 386L359 376L369 400L346 410ZM351 421L374 411L385 434L361 445ZM366 456L390 445L400 469L377 480ZM381 491L405 480L416 504L392 514ZM397 525L421 515L431 539L407 549ZM200 -11L224 -21L234 2L210 13ZM246 93L270 83L280 106L257 117ZM262 128L285 117L296 141L272 152ZM277 163L301 152L311 176L288 186ZM293 197L316 187L327 211L303 221ZM308 232L332 222L342 245L319 256ZM354 336L378 326L389 349L365 360ZM370 371L394 360L404 384L380 395ZM385 406L409 395L420 419L396 430ZM401 440L424 430L435 454L411 464ZM416 475L440 465L450 488L427 499ZM432 510L455 499L466 523L442 534ZM250 8L274 -2L284 22L261 32ZM265 43L289 33L300 56L276 67ZM296 113L320 102L331 126L307 136ZM312 147L336 137L346 160L322 171ZM327 182L351 171L362 195L338 206ZM343 217L366 206L377 230L353 240ZM358 251L382 241L392 265L369 275ZM420 390L444 380L454 403L431 414ZM435 425L459 414L470 438L446 449ZM451 460L475 449L485 473L461 483ZM482 529L506 519L516 542L492 553ZM285 -7L308 -18L319 6L295 17ZM300 28L324 17L334 41L311 51ZM408 271L432 260L443 284L419 294ZM424 305L448 295L458 319L434 329ZM439 340L463 330L474 353L450 364ZM455 375L478 364L489 388L465 399ZM470 410L494 399L504 423L481 433ZM486 444L509 434L520 457L496 468ZM501 479L525 468L535 492L512 503ZM335 12L359 2L369 25L345 36ZM397 151L420 141L431 164L407 175ZM412 186L436 175L446 199L423 210ZM428 221L451 210L462 234L438 244ZM443 255L467 245L477 268L454 279ZM458 290L482 279L493 303L469 314ZM520 429L544 418L555 442L531 453ZM385 31L409 21L419 45L396 55ZM462 205L486 194L497 218L473 229ZM478 240L502 229L512 253L488 264ZM509 309L532 299L543 322L519 333ZM420 16L443 5L454 29L430 40ZM435 51L459 40L470 64L446 75ZM482 155L505 144L516 168L492 179ZM497 190L521 179L531 203L508 213ZM512 224L536 214L547 238L523 248ZM454 1L478 -10L489 14L465 24ZM516 139L540 129L551 153L527 163ZM532 174L555 164L566 187L542 198ZM489 -15L513 -25L523 -2L500 9ZM505 20L528 9L539 33L515 44Z" fill="#e4dfcd" stroke="#e4dfcd" stroke-width="5" stroke-linejoin="round"/><path d="M-20 337L4 326L14 350L-9 360ZM77 460L100 449L111 473L87 484ZM108 529L131 519L142 543L118 553ZM111 445L135 434L146 458L122 468ZM127 479L151 469L161 492L137 503ZM7 117L31 106L41 130L18 140ZM53 221L77 210L88 234L64 245ZM100 325L123 314L134 338L110 349ZM115 360L139 349L149 373L126 383ZM146 429L170 418L180 442L157 453ZM192 533L216 523L227 546L203 557ZM42 101L65 91L76 114L52 125ZM103 240L127 229L138 253L114 264ZM119 275L143 264L153 288L130 298ZM134 309L158 299L169 323L145 333ZM181 414L205 403L215 427L191 437ZM196 448L220 438L231 461L207 472ZM212 483L235 472L246 496L222 507ZM76 86L100 75L111 99L87 109ZM107 155L131 145L142 168L118 179ZM123 190L147 179L157 203L133 214ZM200 363L224 353L234 377L211 387ZM65 -34L88 -44L99 -21L75 -10ZM177 124L200 114L211 137L187 148ZM254 298L278 287L288 311L265 322ZM285 367L309 357L319 380L295 391ZM316 437L340 426L350 450L326 460ZM331 471L355 461L366 485L342 495ZM150 -30L173 -41L184 -17L160 -6ZM181 39L204 29L215 53L191 63ZM242 178L266 168L277 191L253 202ZM231 59L254 48L265 72L241 82ZM405 356L428 345L439 369L415 379ZM466 494L490 484L501 508L477 518ZM269 -42L293 -52L304 -29L280 -18ZM393 236L417 225L427 249L403 260ZM517 514L540 503L551 527L527 537ZM474 325L498 314L508 338L485 348ZM354 -38L378 -49L388 -25L365 -14ZM370 -3L393 -14L404 10L380 21ZM400 66L424 56L435 79L411 90ZM431 136L455 125L466 149L442 159ZM447 170L471 160L481 184L457 194ZM493 274L517 264L528 288L504 298ZM466 120L490 110L500 133L477 144ZM501 105L525 94L535 118L511 128ZM524 -30L548 -41L558 -17L534 -7Z" fill="#e1dbc6" stroke="#e1dbc6" stroke-width="5" stroke-linejoin="round"/><path d="M30 356L54 345L65 369L41 380ZM3 202L27 191L37 215L14 225ZM80 375L104 365L115 388L91 399ZM177 498L201 488L211 512L188 522ZM150 344L174 334L184 357L160 368ZM30 -18L54 -29L64 -5L41 5ZM154 259L177 249L188 272L164 283ZM127 105L150 94L161 118L137 129ZM188 244L212 233L223 257L199 268ZM204 279L228 268L238 292L214 302ZM219 313L243 303L254 326L230 337ZM281 452L305 442L315 465L292 476ZM362 541L386 530L397 554L373 565ZM165 5L189 -6L199 18L176 28ZM215 24L239 13L250 37L226 48ZM323 267L347 256L358 280L334 291ZM235 -26L258 -37L269 -13L245 -3ZM389 321L413 310L423 334L400 345ZM316 62L339 52L350 76L326 86ZM346 132L370 121L381 145L357 156ZM362 167L386 156L396 180L373 190ZM377 201L401 191L412 214L388 225ZM489 359L513 349L524 373L500 383ZM505 394L529 384L539 407L515 418ZM416 101L440 90L450 114L427 125ZM524 344L548 333L558 357L535 368ZM528 259L552 248L562 272L538 283ZM439 -34L463 -45L473 -21L450 -10ZM470 35L494 25L504 48L480 59Z" fill="#e8e4d4" stroke="#e8e4d4" stroke-width="5" stroke-linejoin="round"/><path d="M92 495L116 484L126 508L103 518ZM-28 132L-4 122L7 145L-17 156ZM-5 -3L19 -14L30 10L6 21ZM45 16L69 6L80 29L56 40ZM138 225L162 214L173 238L149 248ZM297 487L320 476L331 500L307 511ZM269 333L293 322L304 346L280 356ZM339 302L363 291L373 315L349 325ZM281 78L305 67L315 91L291 102ZM374 286L397 276L408 299L384 310ZM331 97L355 87L365 110L342 121ZM319 -22L343 -33L354 -9L330 1ZM350 47L374 36L385 60L361 71ZM366 82L390 71L400 95L376 105ZM381 116L405 106L416 130L392 140ZM404 -19L428 -29L439 -5L415 5ZM451 85L474 75L485 99L461 109ZM485 70L509 59L520 83L496 94ZM520 55L544 44L554 68L531 78Z" fill="#dbe3cc" stroke="#dbe3cc" stroke-width="5" stroke-linejoin="round"/></g><path d="M-60 256 C0 256 20 256 66 256" stroke="#cdd9d6" stroke-width="78" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 104 292 104 328 256" stroke="#cdd9d6" stroke-width="60" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 408 292 408 328 256" stroke="#cdd9d6" stroke-width="60" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 192 432 160" stroke="#cdd9d6" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 320 432 352" stroke="#cdd9d6" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 160 C484 137 545 100 630 58" stroke="#cdd9d6" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 352 C484 375 545 412 630 454" stroke="#cdd9d6" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M-60 256 C0 256 20 256 66 256" stroke="url(#wwat)" stroke-width="66" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 104 292 104 328 256" stroke="url(#wwat)" stroke-width="48" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 408 292 408 328 256" stroke="url(#wwat)" stroke-width="48" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 192 432 160" stroke="url(#wwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 320 432 352" stroke="url(#wwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 160 C484 137 545 100 630 58" stroke="url(#wwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 352 C484 375 545 412 630 454" stroke="url(#wwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M-60 256 C0 256 20 256 66 256" transform="translate(0 -14.5)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M-60 256 C0 256 20 256 66 256" transform="translate(0 14.5)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><path d="M80 256 C98 104 292 104 328 256" transform="translate(0 -10.6)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M80 256 C98 104 292 104 328 256" transform="translate(0 10.6)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><path d="M80 256 C98 408 292 408 328 256" transform="translate(0 -10.6)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M80 256 C98 408 292 408 328 256" transform="translate(0 10.6)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><path d="M314 256 C346 256 360 192 432 160" transform="translate(0 -9.7)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M314 256 C346 256 360 192 432 160" transform="translate(0 9.7)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><path d="M314 256 C346 256 360 320 432 352" transform="translate(0 -9.7)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M314 256 C346 256 360 320 432 352" transform="translate(0 9.7)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><g transform="translate(118.8 172.8) rotate(-43.5)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#f6efd9"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/></g><g transform="translate(273.7 168.3) rotate(37.4)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#f6efd9"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/></g><g transform="translate(118.8 339.2) rotate(43.5)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#f6efd9"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/></g><g transform="translate(273.7 343.7) rotate(-37.4)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#f6efd9"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/></g><g transform="translate(380.8 194.8) rotate(-44.0)"><rect x="-8.5" y="-42.0" width="17" height="84" fill="#f6efd9"/><rect x="-9.5" y="-27.0" width="3.4" height="54" fill="#c9bd9a"/><rect x="6.1" y="-27.0" width="3.4" height="54" fill="#c9bd9a"/></g><g transform="translate(380.8 317.2) rotate(44.0)"><rect x="-8.5" y="-42.0" width="17" height="84" fill="#f6efd9"/><rect x="-9.5" y="-27.0" width="3.4" height="54" fill="#c9bd9a"/><rect x="6.1" y="-27.0" width="3.4" height="54" fill="#c9bd9a"/></g><g transform="translate(327.0 256.0) rotate(90.0)"><rect x="-8.5" y="-47.0" width="17" height="94" fill="#f6efd9"/><rect x="-9.5" y="-33.0" width="3.4" height="66" fill="#c9bd9a"/><rect x="6.1" y="-33.0" width="3.4" height="66" fill="#c9bd9a"/></g><circle cx="136.9" cy="155.6" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="136.9" y="161.1" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">a</text><circle cx="293.6" cy="183.4" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="293.6" y="188.9" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">b</text><circle cx="136.9" cy="356.4" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="136.9" y="361.9" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">c</text><circle cx="293.6" cy="328.6" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="293.6" y="334.1" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">d</text><circle cx="398.8" cy="177.5" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="398.8" y="183.0" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">e</text><circle cx="398.8" cy="334.5" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="398.8" y="340.0" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">f</text><circle cx="327.0" cy="230.0" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="327.0" y="235.5" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">g</text><text x="206" y="268" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="46" fill="#bdb393" opacity=".9">A</text><text x="190" y="66" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="46" fill="#bdb393" opacity=".9">B</text><text x="190" y="462" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="46" fill="#bdb393" opacity=".9">C</text><text x="468" y="262" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="46" fill="#bdb393" opacity=".9">D</text></g><circle cx="256" cy="256" r="250" fill="none" stroke="#cfc8b6" stroke-width="4"/></svg>)---"
app_css <- "
:root { --line: #e2ddcf; --stage: #e9e5d9; --paper: #fbfaf6; --head: 'Oswald', system-ui, sans-serif; }
.bslib-page-title { background: var(--paper); border-bottom: 1px solid var(--line); box-shadow: none; }
.brand { display: flex; align-items: center; gap: .75rem; width: 100%; }
.brand svg { width: 34px; height: 34px; flex: none; }
.history-btn { margin-left: auto; white-space: nowrap; font-size: .85rem; padding: .3rem .7rem; }
.history-btn + .expand-btn { margin-left: .5rem; }
.history-modal .modal-body { min-height: 60vh; } .history-map img { border-radius: .5rem; border: 1px solid var(--line); max-height: calc(68vh - 3rem); width: auto !important; max-width: 100%; display: block; margin: 0 auto; }
.h-wrap { position: relative; } .h-full-btn { position: absolute; top: 0; right: 0; z-index: 2; white-space: nowrap; font-size: .85rem; padding: .3rem .7rem; }
.h-full-btn .when-expanded, .history-full .h-full-btn .when-normal { display: none; } .history-full .h-full-btn .when-expanded { display: inline; }
.history-cols p.fine:first-child { margin-bottom: 1rem; } #shiny-modal:has(.history-modal) .modal-header { padding-bottom: .25rem; border-bottom: 0; } #shiny-modal:has(.history-modal) .modal-title { margin-bottom: 0; }
.history-cols { height: 68vh; min-height: 480px; --_sidebar-bg: transparent; } .history-cols > .main { overflow: auto; } .history-map { align-self: start; } .history-map .fine { margin-bottom: .6rem; }
.modal-dialog.history-full { max-width: none; width: 100vw; height: 100vh; margin: 0; } .history-full .modal-content { min-height: 100vh; border-radius: 0; }
.history-full .history-cols { height: calc(100vh - 11rem); } .history-full .history-map img { max-height: none; max-width: min(100%, 1000px); }      /* full screen: as wide as the column allows, and the window scrolls, so the text is as large as it can be */ .history-full .history-modal-fine, .history-full .fine { font-size: .95rem; }
.expand-btn { margin-left: auto; white-space: nowrap; font-size: .85rem; padding: .3rem .7rem; }
.expand-btn .when-expanded, .is-expanded .expand-btn .when-normal { display: none; }
.is-expanded .expand-btn .when-expanded { display: inline; }
/* Expanded: the header shrinks to the Exit button, floating at the top right, and the map takes the space the header and padding used. */
.is-expanded .navbar { position: fixed; top: 0; right: 0; left: auto; width: auto; background: transparent; border: 0; padding: .45rem .6rem; z-index: 1040; }
.is-expanded .navbar .container-fluid { padding: 0; }
.is-expanded .brand > :not(.expand-btn) { display: none; }
.is-expanded .brand { width: auto; }
.is-expanded .expand-btn, .is-expanded .expand-btn:hover, .is-expanded .expand-btn:focus, .is-expanded .expand-btn:active { background: var(--paper); color: var(--bs-body-color); border-color: var(--bs-secondary-color); }
.is-expanded .bslib-sidebar-layout > .main { padding: .5rem !important; }
.is-expanded .stage { padding-top: 0; }
.is-expanded .stat-row { padding-top: 2.4rem; }
.count-row { display: flex; gap: .5rem; margin-bottom: .7rem; }
.count-cell { flex: 1; background: var(--bs-body-bg); border: 1px solid var(--line); border-radius: 8px; padding: .5rem .6rem; display: flex; flex-direction: column; }
.count-num { font-family: var(--head); font-size: 1.35rem; line-height: 1.1; }
.count-lab { font-size: .72rem; color: var(--bs-secondary-color); }
.counts-table { width: 100%; font-size: .85rem; margin-bottom: .5rem; }
.counts-table th { font-weight: 600; font-size: .72rem; text-transform: uppercase; letter-spacing: .04em; color: var(--bs-secondary-color); padding: .2rem 0; }
.counts-table td { padding: .2rem 0; border-top: 1px solid var(--line); }
.counts-table th:not(:first-child), .counts-table td:not(:first-child) { text-align: right; }
.brand-name { font-family: var(--head); font-size: 1.45rem; letter-spacing: .03em; font-weight: 500; line-height: 1; }
.brand-tag { color: var(--bs-secondary-color); font-size: .9rem; padding-left: .85rem; border-left: 1px solid var(--line); line-height: 1.2; }
@media (max-width: 760px) { .brand-tag { display: none; } }
.bslib-sidebar-layout { --_sidebar-bg: var(--paper); }
.bslib-sidebar-layout > .sidebar { border-right: 1px solid var(--line); }
.ctl { margin-bottom: .9rem; }
.ctl-head { display: flex; align-items: center; gap: .55rem; margin-bottom: .65rem; font-size: .72rem; font-weight: 600;
            letter-spacing: .09em; text-transform: uppercase; color: var(--bs-secondary-color); }
.ctl-head .step { display: inline-grid; place-items: center; width: 1.45rem; height: 1.45rem; border-radius: 50%;
                  background: var(--bs-primary); color: #fff; font-size: .72rem; letter-spacing: 0; }
.sidebar .form-label, .sidebar label.control-label { font-size: .85rem; font-weight: 500; margin-bottom: .25rem; }
.sidebar .form-group, .sidebar .shiny-input-container { margin-bottom: .55rem; width: 100%; }
.sidebar .btn { width: 100%; font-weight: 500; }
#draw { padding: .7rem 1rem; font-size: 1.02rem; box-shadow: 0 2px 0 rgba(0,0,0,.12); }
.find-row { display: flex; gap: .5rem; align-items: flex-start; margin-bottom: .55rem; }
.find-row .form-group, .find-row .shiny-input-container { flex: 1; min-width: 0; margin: 0 !important; width: auto !important; }
.find-row .form-control, .find-row .btn { height: 2.5rem; padding-top: 0; padding-bottom: 0; }
.find-row .btn { width: auto; flex: none; padding-left: 1.1rem; padding-right: 1.1rem; display: inline-flex; align-items: center; }
.sidebar .form-control, .sidebar .form-select { min-height: 2.5rem; }
.find-row label.control-label { position: absolute !important; width: 1px; height: 1px; margin: -1px; padding: 0; overflow: hidden; clip: rect(0 0 0 0); white-space: nowrap; border: 0; }
.style-picker .shiny-options-group { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: .5rem; }
.style-picker .form-check, .style-picker .radio { padding: 0; margin: 0; min-height: 0; }
.style-picker input[type=radio] { position: absolute; opacity: 0; pointer-events: none; }
.style-picker .shiny-options-group label { display: flex; flex-direction: column; align-items: stretch; width: 100%; height: 100%; margin: 0; padding: .45rem !important; border: 1.5px solid var(--line);
                      border-radius: .65rem; background: #fff; cursor: pointer; text-align: center; font-size: .74rem; font-weight: 500; white-space: nowrap; }
.style-picker .shiny-options-group label > span { display: flex; flex-direction: column; align-items: stretch; width: 100%; min-width: 0; margin: 0; padding: 0; }
.style-picker .shiny-options-group label > input { position: absolute; }
.style-picker input:checked + label, .style-picker .shiny-options-group label:has(input:checked) { border-color: var(--bs-primary); box-shadow: 0 0 0 2px rgba(31,58,90,.18); }
.style-picker input:focus-visible + label, .style-picker .shiny-options-group label:has(input:focus-visible) { outline: 2px solid var(--bs-primary); outline-offset: 2px; }
.sw { display: flex; flex-direction: column; justify-content: center; gap: 3px; width: 100%; height: 34px; box-sizing: border-box; margin-bottom: .4rem; padding: 0 .6rem;
      border: 1px solid; border-radius: .4rem; }
.sw i { display: block; border-radius: 2px; }
.fine { font-size: .75rem; color: var(--bs-secondary-color); margin: 0; }
.addr-msg { margin: 0 0 .5rem; }
.sugg { list-style: none; margin: .2rem 0 .7rem; padding: .25rem; border: 1px solid var(--bs-border-color); border-radius: .5rem; background: var(--bs-body-bg); }
.sugg li { margin: 0; }
.sugg-item { display: block; width: 100%; min-height: 44px; padding: .5rem .65rem; border: 0; border-radius: .35rem; background: transparent; color: var(--bs-body-color); text-align: left; font-size: .92rem; line-height: 1.25; cursor: pointer; }
.sugg-item:hover, .sugg-item:focus-visible { background: rgba(31, 58, 90, .10); outline: 2px solid transparent; box-shadow: inset 0 0 0 2px #1f3a5a; }
.need-draw { margin: .7rem 0 0; padding: .5rem .7rem; border-left: 3px solid var(--sm-accent, #d9822b); background: rgba(217, 130, 43, .09); border-radius: 0 .4rem .4rem 0; color: var(--bs-body-color); }
.accordion { --bs-accordion-bg: transparent; --bs-accordion-border-color: var(--line); font-size: .85rem; }
.accordion-button { padding: .6rem .8rem; font-size: .85rem; font-weight: 500; }
.stage { display: grid; grid-template-columns: minmax(0, 1fr) 17rem; grid-template-rows: minmax(0, 1fr); gap: .85rem; background: var(--stage); }
.stage > *, .map-card .card-body, #map { min-width: 0; }
main.bslib-page-main, .bslib-sidebar-layout > .main { min-width: 0 !important; }
.stage { width: 100%; max-width: 100%; padding-top: 24px; }   /* the same 24 px of stage above the paper and stats as below, so the page looks like a sheet on a desk */
.stats-slot { display: block !important; grid-column: 2; grid-row: 1; min-height: 0; overflow-y: auto; background: transparent; }
.map-card { grid-column: 1; grid-row: 1; }
.area-picker .shiny-options-group { display: flex; flex-direction: column; gap: .1rem; }
.area-picker .form-check, .area-picker .radio { margin: 0; min-height: 0; }
.area-picker .form-check-label, .area-picker .radio label { font-size: .88rem; }
.view-picker .shiny-options-group { display: grid; grid-template-columns: 1fr 1fr; gap: .5rem; }
.view-picker .form-check, .view-picker .radio { padding: 0; margin: 0; min-height: 0; }
.view-picker input[type=radio] { position: absolute; opacity: 0; pointer-events: none; }
.view-picker .shiny-options-group label { display: flex; flex-direction: column; gap: .15rem; width: 100%; height: 100%; margin: 0; padding: .55rem .6rem !important; border: 1.5px solid var(--line);
  border-radius: .65rem; background: #fff; cursor: pointer; }
.view-picker .vt, .view-picker .vb { display: block; }
.view-picker .vt { font-weight: 600; font-size: .86rem; line-height: 1.15; }
.view-picker .vb { font-size: .72rem; line-height: 1.25; color: var(--bs-secondary-color); }
.view-picker .shiny-options-group label:has(input:checked) { border-color: var(--bs-primary); box-shadow: 0 0 0 2px rgba(31,58,90,.18); }
.view-picker .shiny-options-group label:has(input:focus-visible) { outline: 2px solid var(--bs-primary); outline-offset: 2px; }
.stat-row { display: grid; grid-template-columns: 1fr; gap: .75rem; align-content: start; }
.stat { background: #fff; border: 1px solid var(--line); border-radius: .8rem; padding: .7rem 1rem .75rem; border-left: 5px solid var(--accent, var(--bs-primary)); }
.stat-value { font-family: var(--head); font-size: 2rem; line-height: 1.05; letter-spacing: .01em; }
.stat-label { font-size: .7rem; text-transform: uppercase; letter-spacing: .09em; font-weight: 600; color: var(--bs-secondary-color); margin-top: .15rem; }
.stat-note { font-size: .78rem; color: var(--bs-secondary-color); }
.zoom-hint { margin-top: .85rem; }
.cmp-table td:not(:first-child), .cmp-table th:not(:first-child) { text-align: right; } .cmp-table td:first-child { color: var(--bs-secondary-color); }
body:has(#inspect:checked) #map img { cursor: crosshair; }
.hl-case { fill: none; stroke: #fff; stroke-width: 7px; stroke-linecap: round; stroke-linejoin: round; }
.hl-line { fill: none; stroke: #d9480f; stroke-width: 3.5px; stroke-linecap: round; stroke-linejoin: round; }
#map { position: relative; }
.street-found ul { margin: .25rem 0 0 1rem; padding: 0; }
.inspect { background: #fff; border: 1px solid var(--line); border-radius: .8rem; padding: .7rem 1rem .8rem; border-left: 5px solid var(--bs-secondary-color); }
.inspect-title { font-family: var(--head); font-size: 1.35rem; line-height: 1.1; margin-bottom: .35rem; }
.inspect-row { display: flex; justify-content: space-between; gap: .75rem; font-size: .82rem; padding: .12rem 0; border-top: 1px solid var(--line); }
.inspect-k { color: var(--bs-secondary-color); } .inspect-v { text-align: right; }
.draw-row { display: flex; gap: .5rem; } .draw-row .btn { flex: 1; white-space: nowrap; padding-left: .5rem; padding-right: .5rem; }
.stat-head { font-size: .7rem; text-transform: uppercase; letter-spacing: .09em; font-weight: 600; color: var(--bs-secondary-color); margin: .3rem 0 -.2rem; }
.stat-mini .stat { padding: .45rem .8rem .5rem; border-left-width: 3px; }
.stat-mini .stat-value { font-size: 1.35rem; }
.stat-mini .stat-label { text-transform: none; letter-spacing: .01em; font-size: .78rem; }
.stat-mini .stat-note { font-size: .72rem; }
.s-total { --accent: #1f3a5a; } .s-local { --accent: #9aa8b6; } .s-through { --accent: #a6372a; }
@media (max-width: 1100px) {
  .stage { grid-template-columns: minmax(0, 1fr); grid-template-rows: auto minmax(0, 1fr); }
  .stats-slot { grid-column: 1; grid-row: 1; overflow: visible; }
  .map-card { grid-column: 1; grid-row: 2; }
  .stat-row { grid-template-columns: repeat(3, minmax(0, 1fr)); }
  .stat-value { font-size: 1.6rem; }
  .zoom-hint { display: none; }
}
@media (max-width: 760px) { .stat-row { grid-template-columns: 1fr; } .stat-note { display: none; } }
.map-card { border: 0; background: transparent; box-shadow: none; flex: 1 1 auto; min-height: 0; }
.map-card .card-body { padding: 0; display: flex; align-items: center; justify-content: center; min-height: 0; }
#map { width: 100%; height: 100%; display: flex; align-items: center; justify-content: center; min-height: 0; }
#map img { cursor: zoom-in; display: block; max-width: 100%; max-height: 100%; width: auto; height: auto; object-fit: contain; min-height: 0;
           border-radius: .35rem; box-shadow: 0 1px 2px rgba(0,0,0,.14), 0 10px 30px -8px rgba(40,34,20,.35); }
.card[data-full-screen=true] .card-body { background: var(--stage); }
.card[data-full-screen=true].zoom-full .card-body { overflow: auto; display: block; }
.card[data-full-screen=true].zoom-full #map { display: block; width: max-content; height: auto; margin: 0 auto; }
.card[data-full-screen=true].zoom-full #map img { max-width: none; max-height: none; width: 1400px; cursor: zoom-out; }
#map.shiny-output-error-validation, #map .shiny-output-error-validation { color: var(--bs-secondary-color); font-size: 1.05rem; text-align: center; padding: 2rem; max-width: 34rem; }
#map:empty { flex-direction: column; justify-content: center; color: var(--bs-secondary-color);
  background: url(\"data:image/svg+xml,__WM__\") center 42% / min(70vmin, 520px) no-repeat; }
#map:empty::after { content: 'Choose a place on the left, then draw its map.'; margin-top: min(78vmin, 600px); max-width: 24rem; text-align: center; font-size: 1.05rem; }
/* Side panel */
.place-card { margin: .1rem 0 .75rem; padding: .6rem .7rem; border: 1px solid var(--line); border-radius: .6rem; background: rgba(255,255,255,.55); }
.place-line { display: flex; gap: .55rem; align-items: flex-start; }
.place-check { color: #2f7d5b; font-size: 1.05rem; line-height: 1.35; }
.place-text { min-width: 0; }
.place-name { font-weight: 600; font-size: .95rem; line-height: 1.3; overflow-wrap: anywhere; }
.place-where { font-size: .78rem; color: var(--bs-secondary-color); line-height: 1.3; overflow-wrap: anywhere; }
.place-card .form-group, .place-card .shiny-input-container { margin: .6rem 0 0; }
.radius-head { display: flex; align-items: center; justify-content: space-between; gap: .75rem; margin: .1rem 0 .5rem; }
.radius-title { font-size: .85rem; font-weight: 500; }
.radius-head .form-group, .radius-head .shiny-input-container { margin: 0 !important; width: auto !important; }
.radius-head .shiny-options-group { display: flex; gap: 1rem; }
.radius-head .form-check-label, .radius-head .radio label { font-size: .88rem; }
.view-picker .shiny-options-group > div:last-child:nth-child(odd) { grid-column: 1 / -1; }
.sidebar .irs-with-grid, .sidebar .irs { margin-top: -.15rem; }
.sidebar .form-select { font-size: .92rem; line-height: 1.35; color: var(--bs-body-color); background-color: #fffdf8; border: 1.5px solid #a79f88; border-radius: .5rem;
  padding: .45rem 2.2rem .45rem .7rem; min-height: 2.6rem; text-overflow: ellipsis; }
.sidebar .form-select:focus { border-color: var(--bs-primary); box-shadow: 0 0 0 3px rgba(31,58,90,.2); }
.sidebar .form-select option { color: #1d1d1b; background: #fffdf8; font-size: .92rem; }
.dl-grid { display: grid; gap: .1rem; }
.dl-grid .form-group, .dl-grid .shiny-input-container { margin-bottom: .6rem; }
.more-panels { margin-top: 1.1rem; }
.more-panels .accordion-body { padding: .8rem .85rem 1rem; }
.more-panels .accordion-body .remember { margin-top: .9rem; padding-top: .8rem; border-top: 1px solid var(--line); }
.sidebar .ctl { margin-bottom: 1.15rem; }
.sidebar .ctl-head { margin-bottom: .75rem; }
.sidebar .shiny-input-container > label.control-label { margin-bottom: .35rem; }
.sidebar .bslib-input-switch, .sidebar .form-switch { margin-bottom: .45rem; }

"

app_css <- sub("__WM__", utils::URLencode(watermark_svg, reserved = TRUE), app_css, fixed = TRUE)
app_js <- "
// Expand: give the map the whole window. The sidebar folds away (the app does that when it hears the state change) and, where the
// browser allows it, the page goes full screen too. Nothing here opens a new window, so popup blockers never get involved.
var streetMapsUsedFullscreen = false;
function streetMapsSetExpanded(on) {
  document.documentElement.classList.toggle('is-expanded', on);
  var b = document.getElementById('expand-btn'); if (b) { b.setAttribute('aria-pressed', on ? 'true' : 'false'); }
  if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('expanded', on, { priority: 'event' }); }
}
// The history window can fill the whole browser window, so the map and the text are larger.
function overmapsHistoryFull() {
  var d = document.querySelector('#shiny-modal .modal-dialog'); if (!d || !document.querySelector('.history-modal')) return;
  var on = !d.classList.contains('history-full');
  d.classList.toggle('history-full', on); d.classList.toggle('modal-fullscreen', on); d.classList.toggle('modal-xl', !on);
  var b = document.getElementById('h-full-btn'); if (b) b.setAttribute('aria-pressed', on ? 'true' : 'false');
  setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 50);
}
function streetMapsToggleExpand() {
  var on = !document.documentElement.classList.contains('is-expanded');
  if (on) {
    var el = document.documentElement;
    if (el.requestFullscreen) { try { var p = el.requestFullscreen(); streetMapsUsedFullscreen = true; if (p && p.catch) { p.catch(function () { streetMapsUsedFullscreen = false; }); } } catch (e) { streetMapsUsedFullscreen = false; } }
  } else if (document.fullscreenElement && document.exitFullscreen) { try { document.exitFullscreen(); } catch (e) {} }
  streetMapsSetExpanded(on);
}
// Leaving full screen with Esc also leaves the expanded layout, so the two never disagree.
document.addEventListener('fullscreenchange', function () {
  if (!document.fullscreenElement && streetMapsUsedFullscreen) { streetMapsUsedFullscreen = false; streetMapsSetExpanded(false); }
});
$(document).on('keydown', function (e) {
  if (e.key === 'Escape' && document.documentElement.classList.contains('is-expanded') && !document.fullscreenElement && !$('.zoom-full, [data-full-screen=\"true\"].bslib-full-screen').length) { streetMapsSetExpanded(false); }
});
$(document).on('keyup', '#query', function (e) { if (e.which === 13) $('#find').click(); });
function streetMapsCopyLink() {
  var f = document.getElementById('share_url'), b = document.getElementById('share_copy'); if (!f) return;
  var done = function () { if (b) { b.textContent = 'Copied'; setTimeout(function () { b.textContent = 'Copy'; }, 1800); } };
  f.select();
  if (navigator.clipboard && navigator.clipboard.writeText) { navigator.clipboard.writeText(f.value).then(done, function () { try { document.execCommand('copy'); done(); } catch (e) {} }); }
  else { try { document.execCommand('copy'); done(); } catch (e) {} }
}
Shiny.addCustomMessageHandler('radiusUnit', function (u) { var s = $('#radius').data('ionRangeSlider'); if (s) s.update({postfix: ' ' + u}); $('#draw').prop('disabled', false); });
// While the server converts the radius to the new unit, hold the Draw button so a quick click cannot send the old number under the new unit.
$(document).on('change', 'input[name=units]', function () { $('#draw').prop('disabled', true); setTimeout(function () { $('#draw').prop('disabled', false); }, 4000); });
$(document).on('keyup', '#addr', function (e) { if (e.which === 13) $('#find_addr').click(); });
// Suggestions while typing: send the text after a short pause, pick with a click or the keyboard.
var streetMapsTimers = {};
var streetMapsPicked = {};
function streetMapsTyped(kind, el) {
  clearTimeout(streetMapsTimers[kind]);
  if (streetMapsPicked[kind] === el.value.trim()) return;          // the box holds a suggestion that was just clicked
  streetMapsTimers[kind] = setTimeout(function () { Shiny.setInputValue('typed', {kind: kind, text: el.value, t: Date.now()}, {priority: 'event'}); }, 450);
}
$(document).on('input', '#query', function () { streetMapsTyped('place', this); });
$(document).on('input', '#addr', function () { streetMapsTyped('address', this); });
$(document).on('click', '.sugg-item', function () {
  var kind = $(this).data('kind'), idx = $(this).data('i'), box = $(kind === 'place' ? '#query' : '#addr');
  clearTimeout(streetMapsTimers[kind]);                            // a pause that has not finished yet must not bring the list back
  streetMapsPicked[kind] = $(this).text().trim(); box.val(streetMapsPicked[kind]);
  $(this).closest('.sugg').remove();                               // the list closes at once, not after the server answers
  Shiny.setInputValue('pick', {kind: kind, i: idx, t: Date.now()}, {priority: 'event'});
});
$(document).on('click', function (e) { if (!$(e.target).closest('.sugg, #query, #addr').length) $('.sugg').hide(); });
$(document).on('click', '#find', function () { clearTimeout(streetMapsTimers.place); streetMapsPicked.place = $('#query').val().trim(); $('#place_sugg .sugg').remove(); });
$(document).on('keyup', '#query', function (e) { if (e.which === 13) $('#find').click(); });
$(document).on('keydown', '#query, #addr', function (e) {
  var kind = this.id === 'query' ? 'place' : 'address', items = $('.sugg-item[data-kind=' + kind + ']');
  if (e.key === 'ArrowDown' && items.length) { e.preventDefault(); items.first().focus(); }
  if (e.key === 'Escape') { items.closest('.sugg').hide(); }
});
$(document).on('keydown', '.sugg-item', function (e) {
  var items = $('.sugg-item[data-kind=' + $(this).data('kind') + ']'), k = items.index(this);
  if (e.key === 'ArrowDown') { e.preventDefault(); items.eq(Math.min(k + 1, items.length - 1)).focus(); }
  if (e.key === 'ArrowUp') { e.preventDefault(); if (k === 0) { $(this).data('kind') === 'place' ? $('#query').focus() : $('#addr').focus(); } else items.eq(k - 1).focus(); }
  if (e.key === 'Escape') { $(this).data('kind') === 'place' ? $('#query').focus() : $('#addr').focus(); $(this).closest('.sugg').hide(); }
});
// Saved settings live in this browser only, and only when the switch is on.
Shiny.addCustomMessageHandler('saveSettings', function (m) {
  try { if (m.qs) localStorage.setItem('overmaps.settings', m.qs); else localStorage.removeItem('overmaps.settings'); } catch (e) {}
});
$(document).on('shiny:connected', function () {
  try { var v = localStorage.getItem('overmaps.settings'); if (v) Shiny.setInputValue('saved_settings', v, {priority: 'event'}); } catch (e) {}
});
// Highlight streets over the map picture. The server sends polylines as fractions of the picture; the overlay follows the picture when it resizes.
var hlLines = [];
function hlDraw() {
  $('#map .hl-svg').remove();
  var img = document.querySelector('#map img'); if (!img || !hlLines.length) return;
  var ns = 'http://www.w3.org/2000/svg', svg = document.createElementNS(ns, 'svg');
  svg.setAttribute('class', 'hl-svg'); svg.setAttribute('viewBox', '0 0 1 1'); svg.setAttribute('preserveAspectRatio', 'none'); svg.setAttribute('aria-hidden', 'true');
  svg.style.cssText = 'position:absolute;pointer-events:none;left:' + img.offsetLeft + 'px;top:' + img.offsetTop + 'px;width:' + img.offsetWidth + 'px;height:' + img.offsetHeight + 'px;';
  ['hl-case', 'hl-line'].forEach(function (cls) {
    hlLines.forEach(function (pts) {
      var p = document.createElementNS(ns, 'polyline'); p.setAttribute('points', pts.join(' ')); p.setAttribute('class', cls); p.setAttribute('vector-effect', 'non-scaling-stroke'); svg.appendChild(p);
    });
  });
  img.parentNode.appendChild(svg);
}
Shiny.addCustomMessageHandler('hl', function (m) { hlLines = (m.lines || []).map(function (l) { return Array.isArray(l) ? l : [l]; }); hlDraw(); });
$(window).on('resize', hlDraw);
$(document).on('shiny:value', function (e) { if (e.name === 'map') setTimeout(function () { hlLines = []; $('#map .hl-svg').remove(); }, 0); });
$(document).on('click', '#map img', function () { setTimeout(hlDraw, 350); });
// Click the map to open it at full resolution (scroll to pan); click again to fit the window; Esc closes.
$(document).on('click', '#map img', function (e) {
  if ($('#inspect').is(':checked')) {
    var r = this.getBoundingClientRect();
    Shiny.setInputValue('map_click', {fx: (e.clientX - r.left) / r.width, fy: (e.clientY - r.top) / r.height, t: Date.now()}, {priority: 'event'});
    e.stopImmediatePropagation(); return;
  }
  var card = $(this).closest('.card');
  if (card.attr('data-full-screen') === 'true') { card.toggleClass('zoom-full'); }
  else { card.addClass('zoom-full'); card.find('.bslib-full-screen-enter').trigger('click'); }
});
"

# The mark is a small street map of the Konigsberg islands: the river Pregel, two islands, and the seven bridges Euler turned into graph theory.
brand_svg <- r"---(<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" aria-hidden="true" focusable="false"><defs><clipPath id="kmt"><rect width="512" height="512" rx="116"/></clipPath><radialGradient id="kmvig" cx=".5" cy=".5" r=".72"><stop offset=".62" stop-color="#2a1a05" stop-opacity="0"/><stop offset="1" stop-color="#2a1a05" stop-opacity="0.14"/></radialGradient><linearGradient id="kmgl" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#fff" stop-opacity="0.2"/><stop offset=".45" stop-color="#fff" stop-opacity="0"/></linearGradient></defs><g clip-path="url(#kmt)"><defs><linearGradient id="kmwat" gradientUnits="userSpaceOnUse" x1="0" y1="90" x2="512" y2="430"><stop offset="0" stop-color="#86c1dc"/><stop offset="1" stop-color="#4f9bc4"/></linearGradient><clipPath id="kmca"><polygon points="80.0,256.0 83.1,237.8 88.0,221.2 94.3,206.1 102.1,192.7 111.1,180.8 121.3,170.5 132.4,161.8 144.3,154.7 156.9,149.1 170.0,145.2 183.5,142.8 197.2,142.0 211.1,142.8 224.9,145.2 238.5,149.1 251.7,154.7 264.5,161.8 276.6,170.5 287.9,180.8 298.4,192.7 307.7,206.1 315.9,221.2 322.7,237.8 328.0,256.0 328.0,256.0 322.7,274.2 315.9,290.8 307.7,305.9 298.4,319.3 287.9,331.2 276.6,341.5 264.5,350.2 251.7,357.3 238.5,362.9 224.9,366.8 211.1,369.2 197.2,370.0 183.5,369.2 170.0,366.8 156.9,362.9 144.3,357.3 132.4,350.2 121.3,341.5 111.1,331.2 102.1,319.3 94.3,305.9 88.0,290.8 83.1,274.2 80.0,256.0"/></clipPath><clipPath id="kmcd"><polygon points="314.0,256.0 318.7,255.5 323.1,254.2 327.4,252.0 331.6,249.1 335.8,245.5 340.0,241.3 344.2,236.6 348.6,231.4 353.2,225.9 358.0,220.0 363.1,213.9 368.6,207.6 374.5,201.2 380.8,194.8 387.7,188.5 395.2,182.3 403.3,176.2 412.1,170.5 421.6,165.0 432.0,160.0 445.2,154.0 458.8,147.4 472.9,140.3 487.6,132.7 502.8,124.6 518.6,116.1 535.2,107.2 552.4,98.0 570.5,88.4 589.4,78.5 609.2,68.4 630.0,58.0 640.0,100.0 640.0,412.0 630.0,454.0 609.2,443.6 589.4,433.5 570.5,423.6 552.4,414.0 535.2,404.8 518.6,395.9 502.8,387.4 487.6,379.3 472.9,371.7 458.8,364.6 445.2,358.0 432.0,352.0 421.6,347.0 412.1,341.5 403.3,335.8 395.2,329.7 387.7,323.5 380.8,317.2 374.5,310.8 368.6,304.4 363.1,298.1 358.0,292.0 353.2,286.1 348.6,280.6 344.2,275.4 340.0,270.7 335.8,266.5 331.6,262.9 327.4,260.0 323.1,257.8 318.7,256.5 314.0,256.0"/></clipPath></defs><rect width="512" height="512" fill="#fdfaf1"/><g><path d="M-132 480L-55 468L-43 545L-120 557ZM-118 571L-41 559L-29 636L-105 648ZM-127 -78L-51 -90L-38 -13L-115 -1ZM-84 194L-7 182L5 259L-72 271ZM-70 285L7 272L19 349L-58 361ZM-41 466L36 454L48 531L-29 543ZM-27 557L50 544L62 621L-15 633ZM-22 -2L54 -14L67 63L-10 75ZM6 180L83 167L95 244L19 256ZM35 361L112 349L124 426L47 438ZM54 -107L131 -119L143 -42L66 -30ZM97 165L174 153L186 230L109 242ZM140 437L217 425L229 502L152 514ZM154 528L231 516L243 593L167 605ZM159 -30L236 -43L248 34L171 46ZM231 423L308 411L320 488L243 500ZM250 -45L326 -57L339 20L262 32ZM293 227L370 215L382 292L305 304ZM307 318L384 306L396 382L319 395ZM321 408L398 396L410 473L334 485ZM340 -59L417 -71L429 5L352 18ZM369 122L446 110L458 187L381 199ZM383 213L460 201L472 277L396 290ZM398 303L475 291L487 368L410 380ZM412 394L489 382L501 459L424 471ZM532 561L608 549L621 626L544 638Z" fill="#eadfc4" stroke="#eadfc4" stroke-width="5" stroke-linejoin="round"/><path d="M-113 13L-36 0L-24 77L-101 89ZM-99 103L-22 91L-10 168L-86 180ZM21 270L98 258L110 335L33 347ZM49 452L126 439L138 516L62 528ZM68 -16L145 -28L157 49L80 61ZM83 75L160 62L172 139L95 151ZM111 256L188 244L200 321L124 333ZM126 347L203 334L215 411L138 423ZM173 60L250 48L262 125L186 137ZM202 242L279 229L291 306L214 318ZM355 31L432 19L444 96L367 108ZM426 485L503 473L515 549L439 562ZM431 -74L508 -86L520 -9L443 3ZM460 108L537 96L549 172L472 185ZM517 470L594 458L606 535L529 547Z" fill="#e3d6b4" stroke="#e3d6b4" stroke-width="5" stroke-linejoin="round"/><path d="M-56 375L21 363L33 440L-43 452ZM-8 89L69 77L81 154L4 166ZM64 542L141 530L153 607L76 619ZM145 -121L221 -133L234 -56L157 -44ZM188 151L265 139L277 216L200 228ZM216 332L293 320L305 397L229 409ZM245 514L322 501L334 578L257 590ZM278 136L355 124L367 201L291 213ZM445 17L522 5L534 82L458 94ZM474 198L551 186L563 263L486 275ZM488 289L565 277L577 354L501 366ZM522 -88L598 -100L611 -23L534 -11ZM550 93L627 81L639 158L563 170Z" fill="#efe6cf" stroke="#efe6cf" stroke-width="5" stroke-linejoin="round"/><path d="M-37 -92L40 -105L52 -28L-25 -16ZM264 46L341 34L353 110L276 123ZM336 499L413 487L425 564L348 576ZM503 380L580 368L592 444L515 457ZM536 3L613 -9L625 67L548 80Z" fill="#c8dfae" stroke="#c8dfae" stroke-width="5" stroke-linejoin="round"/></g><g clip-path="url(#kmca)"><polygon points="80.0,256.0 83.1,237.8 88.0,221.2 94.3,206.1 102.1,192.7 111.1,180.8 121.3,170.5 132.4,161.8 144.3,154.7 156.9,149.1 170.0,145.2 183.5,142.8 197.2,142.0 211.1,142.8 224.9,145.2 238.5,149.1 251.7,154.7 264.5,161.8 276.6,170.5 287.9,180.8 298.4,192.7 307.7,206.1 315.9,221.2 322.7,237.8 328.0,256.0 328.0,256.0 322.7,274.2 315.9,290.8 307.7,305.9 298.4,319.3 287.9,331.2 276.6,341.5 264.5,350.2 251.7,357.3 238.5,362.9 224.9,366.8 211.1,369.2 197.2,370.0 183.5,369.2 170.0,366.8 156.9,362.9 144.3,357.3 132.4,350.2 121.3,341.5 111.1,331.2 102.1,319.3 94.3,305.9 88.0,290.8 83.1,274.2 80.0,256.0" fill="#fdfaf1"/><path d="M-21 -47L18 -27L-2 12L-41 -8ZM-47 5L-8 25L-28 64L-67 44ZM57 -72L96 -52L76 -13L37 -33ZM4 31L43 51L23 90L-16 70ZM56 57L95 77L75 116L36 96ZM160 -19L199 1L179 40L140 20ZM28 238L67 258L47 297L8 277ZM-24 341L15 361L-5 400L-44 380ZM237 -45L276 -25L256 14L217 -6ZM-51 522L-12 542L-32 581L-71 561ZM263 33L302 53L282 92L243 72ZM26 497L65 517L45 556L6 536ZM340 8L379 28L359 67L320 47ZM157 368L196 388L176 427L137 407ZM444 -69L483 -49L463 -10L424 -30ZM366 86L405 106L385 145L346 125ZM313 189L352 209L332 248L293 228ZM156 498L195 518L175 557L136 537ZM365 215L404 235L384 274L345 254ZM312 318L351 338L331 377L292 357ZM286 369L325 389L305 428L266 408ZM495 87L534 107L514 146L475 126ZM520 164L559 184L539 223L500 203ZM545 242L584 262L564 301L525 281ZM543 501L582 520L562 560L523 540Z" fill="#e3d6b4" stroke="#e3d6b4" stroke-width="5" stroke-linejoin="round"/><path d="M30 -20L69 0L49 39L10 19ZM82 6L121 26L101 65L62 45ZM80 264L119 284L99 323L60 303ZM1 419L40 439L20 478L-19 458ZM289 -18L328 2L308 41L269 21ZM236 85L275 105L255 144L216 124ZM104 471L143 491L123 530L84 510ZM208 395L247 415L227 454L188 434ZM470 9L509 29L489 68L450 48ZM207 524L246 544L226 583L187 563ZM494 216L533 236L513 275L474 255ZM415 370L454 390L434 429L395 409Z" fill="#c8dfae" stroke="#c8dfae" stroke-width="5" stroke-linejoin="round"/><path d="M-22 83L17 103L-3 142L-42 122ZM29 109L68 129L48 168L9 148ZM55 187L94 207L74 246L35 226ZM159 110L198 130L178 169L139 149ZM54 316L93 336L73 375L34 355ZM210 136L249 156L229 195L190 175ZM262 162L301 182L281 221L242 201ZM418 -17L457 3L437 42L398 22ZM496 -43L535 -23L515 16L476 -4ZM260 421L299 441L279 480L240 460ZM469 138L508 158L488 197L449 177ZM416 241L455 261L435 300L396 280ZM519 294L558 314L538 353L499 333ZM414 500L453 520L433 559L394 539ZM492 474L531 494L511 533L472 513Z" fill="#efe6cf" stroke="#efe6cf" stroke-width="5" stroke-linejoin="round"/><path d="M-48 134L-9 154L-29 193L-68 173ZM108 -46L147 -26L127 13L88 -7ZM3 160L42 180L22 219L-17 199ZM-23 212L16 232L-4 271L-43 251ZM-49 263L-10 283L-30 322L-69 302ZM186 -71L225 -51L205 -12L166 -32ZM133 32L172 52L152 91L113 71ZM107 84L146 104L126 143L87 123ZM81 135L120 155L100 194L61 174ZM2 290L41 310L21 349L-18 329ZM-50 393L-11 413L-31 452L-70 432ZM211 7L250 27L230 66L191 46ZM185 58L224 78L204 117L165 97ZM132 161L171 181L151 220L112 200ZM106 213L145 233L125 272L86 252ZM27 367L66 387L46 426L7 406ZM-25 470L14 490L-6 529L-45 509ZM315 -70L354 -50L334 -11L295 -31ZM184 188L223 208L203 247L164 227ZM158 239L197 259L177 298L138 278ZM131 291L170 311L150 350L111 330ZM105 342L144 362L124 401L85 381ZM79 394L118 414L98 453L59 433ZM53 445L92 465L72 504L33 484ZM367 -44L406 -24L386 15L347 -5ZM314 59L353 79L333 118L294 98ZM288 111L327 131L307 170L268 150ZM235 214L274 234L254 273L215 253ZM209 265L248 285L228 324L189 304ZM183 317L222 337L202 376L163 356ZM130 420L169 440L149 479L110 459ZM78 523L117 543L97 582L58 562ZM392 34L431 54L411 93L372 73ZM339 137L378 157L358 196L319 176ZM287 240L326 260L306 299L267 279ZM261 292L300 312L280 351L241 331ZM234 343L273 363L253 402L214 382ZM182 446L221 466L201 505L162 485ZM443 60L482 80L462 119L423 99ZM417 112L456 132L436 171L397 151ZM391 163L430 183L410 222L371 202ZM338 266L377 286L357 325L318 305ZM233 472L272 492L252 531L213 511ZM547 -16L586 4L566 43L527 23ZM521 35L560 55L540 94L501 74ZM442 190L481 210L461 249L422 229ZM390 293L429 313L409 352L370 332ZM364 344L403 364L383 403L344 383ZM337 396L376 416L356 455L317 435ZM311 447L350 467L330 506L291 486ZM285 499L324 519L304 558L265 538ZM546 113L585 133L565 172L526 152ZM468 267L507 287L487 326L448 306ZM441 319L480 339L460 378L421 358ZM389 422L428 442L408 481L369 461ZM363 473L402 493L382 532L343 512ZM336 525L375 545L355 584L316 564ZM493 345L532 365L512 404L473 384ZM467 397L506 417L486 456L447 436ZM440 448L479 468L459 507L420 487ZM544 371L583 391L563 430L524 410ZM518 423L557 443L537 482L498 462ZM466 526L505 546L485 585L446 565Z" fill="#eadfc4" stroke="#eadfc4" stroke-width="5" stroke-linejoin="round"/></g><g clip-path="url(#kmcd)"><polygon points="314.0,256.0 318.7,255.5 323.1,254.2 327.4,252.0 331.6,249.1 335.8,245.5 340.0,241.3 344.2,236.6 348.6,231.4 353.2,225.9 358.0,220.0 363.1,213.9 368.6,207.6 374.5,201.2 380.8,194.8 387.7,188.5 395.2,182.3 403.3,176.2 412.1,170.5 421.6,165.0 432.0,160.0 445.2,154.0 458.8,147.4 472.9,140.3 487.6,132.7 502.8,124.6 518.6,116.1 535.2,107.2 552.4,98.0 570.5,88.4 589.4,78.5 609.2,68.4 630.0,58.0 640.0,100.0 640.0,412.0 630.0,454.0 609.2,443.6 589.4,433.5 570.5,423.6 552.4,414.0 535.2,404.8 518.6,395.9 502.8,387.4 487.6,379.3 472.9,371.7 458.8,364.6 445.2,358.0 432.0,352.0 421.6,347.0 412.1,341.5 403.3,335.8 395.2,329.7 387.7,323.5 380.8,317.2 374.5,310.8 368.6,304.4 363.1,298.1 358.0,292.0 353.2,286.1 348.6,280.6 344.2,275.4 340.0,270.7 335.8,266.5 331.6,262.9 327.4,260.0 323.1,257.8 318.7,256.5 314.0,256.0" fill="#fdfaf1"/><path d="M-87 463L-41 443L-20 489L-67 510ZM-61 522L-15 502L6 548L-40 569ZM-54 378L-8 357L12 404L-34 424ZM-28 437L18 416L39 463L-8 483ZM-2 496L44 475L65 522L19 542ZM24 555L71 534L91 581L45 601ZM-74 175L-28 154L-7 200L-54 221ZM-48 234L-2 213L19 259L-27 280ZM-22 293L25 272L45 318L-1 339ZM5 352L51 331L71 377L25 398ZM31 411L77 390L98 436L51 457ZM57 470L103 449L124 495L78 516ZM-94 -29L-48 -49L-27 -3L-73 18ZM-68 30L-22 10L-1 56L-47 77ZM37 266L84 246L104 292L58 313ZM90 384L136 364L157 410L110 431ZM142 502L189 482L209 528L163 549ZM169 562L215 541L235 587L189 608ZM-35 -55L11 -76L32 -29L-14 -9ZM-9 4L37 -16L58 30L12 50ZM44 122L90 102L111 148L64 168ZM123 299L169 279L189 325L143 345ZM149 358L195 338L216 384L169 404ZM175 417L221 397L242 443L196 463ZM201 476L248 456L268 502L222 522ZM228 535L274 515L295 561L248 581ZM77 37L123 16L143 62L97 83ZM103 96L149 75L170 121L123 142ZM155 214L202 193L222 240L176 260ZM182 273L228 252L248 299L202 319ZM260 450L307 429L327 476L281 496ZM109 -48L155 -69L176 -23L130 -2ZM188 129L234 108L255 154L209 175ZM241 247L287 226L307 272L261 293ZM267 306L313 285L334 331L287 352ZM319 424L366 403L386 449L340 470ZM346 483L392 462L413 508L366 529ZM372 542L418 521L439 567L393 588ZM273 161L320 141L340 187L294 208ZM378 397L425 377L445 423L399 444ZM254 -42L300 -63L320 -16L274 4ZM306 76L352 55L373 102L327 122ZM437 371L484 351L504 397L458 417ZM464 430L510 410L531 456L484 476ZM516 548L563 528L583 574L537 594ZM313 -68L359 -89L379 -43L333 -22ZM339 -9L385 -30L406 16L359 37ZM365 50L411 29L432 75L386 96ZM391 109L438 88L458 134L412 155ZM444 227L490 206L511 252L465 273ZM470 286L516 265L537 311L491 332ZM497 345L543 324L563 370L517 391ZM398 -36L444 -56L465 -10L418 11ZM424 23L470 3L491 49L445 70ZM450 83L497 62L517 108L471 129ZM457 -62L503 -82L524 -36L477 -16ZM483 -3L529 -23L550 23L504 43ZM509 56L556 36L576 82L530 102Z" fill="#eadfc4" stroke="#eadfc4" stroke-width="5" stroke-linejoin="round"/><path d="M-81 319L-34 298L-14 345L-60 365ZM-42 89L5 69L25 115L-21 136ZM11 207L57 187L78 233L32 254ZM70 181L116 161L137 207L91 227ZM96 240L143 220L163 266L117 286ZM50 -22L96 -43L117 3L71 24ZM129 155L175 134L196 181L150 201ZM208 332L254 311L275 358L228 378ZM234 391L280 370L301 417L255 437ZM287 509L333 488L354 535L307 555ZM162 70L208 49L229 95L182 116ZM293 365L339 344L360 390L314 411ZM168 -75L214 -95L235 -49L189 -29ZM195 -16L241 -36L261 10L215 31ZM221 43L267 23L288 69L241 90ZM300 220L346 200L366 246L320 267ZM326 279L372 259L393 305L347 326ZM352 338L398 318L419 364L373 385ZM280 17L326 -4L347 43L300 63ZM332 135L379 114L399 161L353 181ZM359 194L405 173L425 220L379 240ZM490 489L536 469L557 515L511 535ZM418 168L464 147L484 193L438 214Z" fill="#e3d6b4" stroke="#e3d6b4" stroke-width="5" stroke-linejoin="round"/><path d="M83 529L130 508L150 554L104 575ZM116 443L162 423L183 469L137 490ZM18 63L64 43L84 89L38 109ZM136 11L182 -10L202 36L156 57ZM247 102L293 82L314 128L268 149ZM405 456L451 436L472 482L425 503ZM411 312L457 292L478 338L432 358ZM523 404L569 383L590 429L543 450ZM477 142L523 121L543 167L497 188ZM503 201L549 180L570 226L524 247ZM529 260L575 239L596 285L550 306ZM536 115L582 95L603 141L556 161Z" fill="#efe6cf" stroke="#efe6cf" stroke-width="5" stroke-linejoin="round"/><path d="M-15 148L31 128L52 174L5 195ZM64 325L110 305L130 351L84 372ZM214 188L261 167L281 213L235 234ZM431 515L477 495L498 541L452 562ZM385 253L431 233L452 279L406 299ZM542 -29L588 -50L609 -3L563 17Z" fill="#c8dfae" stroke="#c8dfae" stroke-width="5" stroke-linejoin="round"/></g><path d="M-60 256 C0 256 20 256 66 256" stroke="#a9c6d3" stroke-width="78" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 104 292 104 328 256" stroke="#a9c6d3" stroke-width="60" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 408 292 408 328 256" stroke="#a9c6d3" stroke-width="60" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 192 432 160" stroke="#a9c6d3" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 320 432 352" stroke="#a9c6d3" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 160 C484 137 545 100 630 58" stroke="#a9c6d3" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 352 C484 375 545 412 630 454" stroke="#a9c6d3" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M-60 256 C0 256 20 256 66 256" stroke="url(#kmwat)" stroke-width="66" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 104 292 104 328 256" stroke="url(#kmwat)" stroke-width="48" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 408 292 408 328 256" stroke="url(#kmwat)" stroke-width="48" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 192 432 160" stroke="url(#kmwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 320 432 352" stroke="url(#kmwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 160 C484 137 545 100 630 58" stroke="url(#kmwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 352 C484 375 545 412 630 454" stroke="url(#kmwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><g transform="translate(118.8 172.8) rotate(-43.5)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#fffdf6"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#6c5f43"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#6c5f43"/></g><g transform="translate(273.7 168.3) rotate(37.4)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#fffdf6"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#6c5f43"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#6c5f43"/></g><g transform="translate(118.8 339.2) rotate(43.5)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#fffdf6"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#6c5f43"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#6c5f43"/></g><g transform="translate(273.7 343.7) rotate(-37.4)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#fffdf6"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#6c5f43"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#6c5f43"/></g><g transform="translate(380.8 194.8) rotate(-44.0)"><rect x="-8.5" y="-42.0" width="17" height="84" fill="#fffdf6"/><rect x="-9.5" y="-27.0" width="3.4" height="54" fill="#6c5f43"/><rect x="6.1" y="-27.0" width="3.4" height="54" fill="#6c5f43"/></g><g transform="translate(380.8 317.2) rotate(44.0)"><rect x="-8.5" y="-42.0" width="17" height="84" fill="#fffdf6"/><rect x="-9.5" y="-27.0" width="3.4" height="54" fill="#6c5f43"/><rect x="6.1" y="-27.0" width="3.4" height="54" fill="#6c5f43"/></g><g transform="translate(327.0 256.0) rotate(90.0)"><rect x="-8.5" y="-47.0" width="17" height="94" fill="#fffdf6"/><rect x="-9.5" y="-33.0" width="3.4" height="66" fill="#6c5f43"/><rect x="6.1" y="-33.0" width="3.4" height="66" fill="#6c5f43"/></g><rect width="512" height="512" fill="url(#kmvig)"/><rect width="512" height="286" fill="url(#kmgl)"/><rect x="13" y="13" width="486" height="486" rx="103" fill="none" stroke="#fff9e6" stroke-width="5" opacity=".9"/><rect x="22" y="22" width="468" height="468" rx="94" fill="none" stroke="#cdbb8a" stroke-width="2" opacity=".8"/></g><rect x="1.5" y="1.5" width="509" height="509" rx="116" fill="none" stroke="#4a3a22" stroke-width="3" opacity=".55"/></svg>)---"
brand_mark <- HTML(brand_svg)
favicon_href <- paste0("data:image/svg+xml,", utils::URLencode(brand_svg, reserved = TRUE))

style_choice_names <- lapply(names(styles), function(k) {
  p <- styles[[k]]
  tagList(div(class = "sw", style = sprintf("background:%s;border-color:%s", p$bg, p$ring),
              tags$i(style = sprintf("height:4px;background:%s", p$cols[1])),
              tags$i(style = sprintf("height:3px;background:%s", p$cols[2])),
              tags$i(style = sprintf("height:2px;background:%s", p$cols[3])),
              tags$i(style = sprintf("height:1px;background:%s", p$cols[5]))),
          p$label)
})

view_choice_names <- lapply(names(views), function(k)
  tagList(span(class = "vt", views[[k]]), span(class = "vb", view_blurbs[[k]])))

section <- function(n, title, ...) div(class = "ctl", div(class = "ctl-head", span(class = "step", n), title), ...)

ui <- function(request) {
  page_sidebar(
    title = div(class = "brand", brand_mark,
                span(class = "brand-name", "Overmaps"),
                span(class = "brand-tag", "Geek out on streets"),
                actionButton("history_open", tagList(icon("landmark"), " Explore history"), class = "btn-sm btn-outline-secondary history-btn", title = "See the roads of ancient civilizations"),
                tags$button(type = "button", id = "expand-btn", class = "btn btn-sm btn-outline-secondary expand-btn", onclick = "streetMapsToggleExpand()",
                            `aria-pressed` = "false", title = "Give the map the whole window (Esc to go back)",
                            span(class = "when-normal", icon("expand"), " Expand"), span(class = "when-expanded", icon("compress"), " Exit expand"))),
    window_title = "Overmaps",
    theme = get_theme(),
    fillable = TRUE,
    class = "app-page",
    tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = favicon_href), tags$style(HTML(app_css)), tags$script(HTML(app_js))),
    sidebar = sidebar(
      id = "sb", width = 360, resizable = TRUE,
      open = list(desktop = "open", mobile = "always-above"),
      section(1, "Choose a place",
              div(class = "find-row",
                  textInput("query", tags$span(class = "visually-hidden", "Place"), placeholder = "Town, city, or street address", width = "100%"),
                  actionButton("find", "Find", class = "btn-outline-primary")),
              uiOutput("place_sugg"),
              conditionalPanel("input.suggest", p(class = "fine", "Suggestions use Photon, an OpenStreetMap service, and for U.S. addresses the Census Bureau. See Settings below.")),
              uiOutput("place_card"),
              div(class = "addr-internal", hidden = NA,        # the address is read from the one box above; these hold what it found and are never shown
                  input_switch("use_addr", "Use the address", value = FALSE),
                  div(class = "find-row", textInput("addr", NULL, value = "", width = "100%"), actionButton("find_addr", "Locate")),
                  uiOutput("addr_sugg"), uiOutput("addr_status"))),
      section(2, "Shape the map",
              div(class = "view-picker",
                  radioButtons("view", "Map type", choiceNames = view_choice_names, choiceValues = names(views),
                               selected = "hierarchy", width = "100%")),
              div(class = "area-picker",
                  radioButtons("area", "Area", choices = c("Circle, city limits outlined" = "dim", "City limits only" = "city", "Circle only" = "circle"), selected = "dim", width = "100%")),
              div(class = "radius-head", span(class = "radius-title", "Units"),
                  radioButtons("units", NULL, choices = c("Kilometers" = "km", "Miles" = "mi"), selected = "km", inline = TRUE, width = "auto")),
              conditionalPanel("input.area != 'city'",
                               sliderInput("radius", "Radius", min = 1, max = 5, value = default_radius_km, step = 0.5, post = " km", ticks = FALSE)),
              conditionalPanel("input.area == 'city' || (input.units == 'mi' ? input.radius * 1.609344 : input.radius) > 6",
                               input_switch("all_streets", "Include every street (slower)", value = FALSE),
                               uiOutput("detail_help")),
              conditionalPanel("input.view == 'hierarchy'",
                               input_switch("locals", "Show local streets", value = FALSE),
                               input_switch("labels", "Label through routes", value = TRUE))),
      section(3, "Choose a look",
              div(class = "style-picker",
                  radioButtons("style", NULL, choiceNames = style_choice_names, choiceValues = names(styles),
                               selected = "classic", inline = TRUE, width = "100%"))),
      div(class = "draw-row",
          actionButton("draw", "Draw map", icon = icon("map"), class = "btn-primary"),
          actionButton("reset", "Reset options", icon = icon("rotate-left"), class = "btn-outline-secondary")),
      uiOutput("need_draw"),
      conditionalPanel("output.has_map == 'yes'",
        div(class = "ctl", style = "margin-top: 1.1rem;",
            div(class = "ctl-head", span(class = "step", 4), "Download"),
            div(class = "dl-grid",
                selectInput("format", "File type", choices = export_formats, selected = "png", selectize = FALSE),
                selectInput("print_size", "Print size", choices = print_size_labels, selected = "8x10", selectize = FALSE)),
            downloadButton("download", "Download", class = "btn-outline-primary"),
            uiOutput("print_note")),
        div(class = "ctl", div(class = "ctl-head", span(class = "step", 5), "Share"), uiOutput("share_box"))),
      accordion(open = FALSE, class = "more-panels",
        accordion_panel("Explore this map",
          input_switch("inspect", "Click a street to inspect it", value = FALSE),
          conditionalPanel("input.inspect", input_switch("hl_all", "Highlight every piece of the street", value = TRUE)),
          div(class = "find-row street-find",
              textInput("street_q", tags$span(class = "visually-hidden", "Find a street by name"), placeholder = "Find a street by name", width = "100%"),
              actionButton("street_go", "Find", class = "btn-outline-primary")),
          uiOutput("street_found")),
        accordion_panel("Compare places",
          p(class = "fine", "Draws this place and up to three others side by side at the same radius, map type, and style, with the numbers underneath."),
          div(class = "find-row",
              textInput("cmp_query", tags$span(class = "visually-hidden", "Other places"), placeholder = "e.g., Morgantown, WV; Savannah, GA", width = "100%"),
              actionButton("cmp_go", "Compare", class = "btn-outline-primary")),
          selectInput("cmp_preset", "Or add a well-known street plan", choices = compare_presets, selected = "", selectize = FALSE, width = "100%"),
          p(class = "fine", "Separate places with a semicolon, up to ", max_compare_extra, ". The first match for each name is used, so add the state or country for a small town. Draw the first map before comparing.")),
        accordion_panel("Settings and privacy",
          input_switch("suggest", "Suggest places and addresses as I type", value = TRUE),
          p(class = "fine", "Suggestions come from Photon, a free OpenStreetMap search service. When you type a U.S. street address with a city and state, the U.S. Census Bureau address file is asked too. Only the text typed so far is sent, and only while this is on."),
          div(class = "remember",
              input_switch("remember", "Remember my settings on this device", value = FALSE),
              p(class = "fine", "Keeps the place, radius, map type, style, and switches in this browser so they are filled in next time. Nothing is sent anywhere, and the address is never saved. ",
                actionLink("forget", "Forget saved settings"), "."))),
        accordion_panel("Download counts", uiOutput("counts")),
        accordion_panel("About this map",
          p(class = "fine", "Every map type is drawn from how the community has tagged streets in OpenStreetMap, not from measured traffic. ",
            "Speed limits in particular are patchy in many places, and the map says how much is missing. ",
            "The city limit is the boundary mapped in OpenStreetMap, which can differ from the legal one. The circle view dims everything outside it; ",
            "\"City limits only\" draws just the inside, up to ", max_city_km2, " square kilometers. Without a boundary, or for a larger place, the map is a plain circle. Large areas in dense cities can be slow."),
          p(class = "fine", "Map data \u00a9 OpenStreetMap contributors."),
          p(class = "fine", paste0("Overmaps ", OVERMAPS_VERSION, ". Copyright Abhik Roy. Released under the "),
            tags$a(href = "https://polyformproject.org/licenses/noncommercial/1.0.0", target = "_blank", rel = "noopener", "PolyForm Noncommercial License 1.0.0"),
            ". The newest version is at ", tags$a(href = "https://github.com/drabhikroy/overmaps/releases/latest", target = "_blank", rel = "noopener", "github.com/drabhikroy/overmaps/releases/latest"), "."))
      )
    ),
    div(class = "stage", style = "height: 100%; min-height: 0;",
        uiOutput("stats", class = "stats-slot", fill = FALSE),
        card(class = "map-card", full_screen = TRUE,
             card_body(fillable = TRUE, imageOutput("map", width = "100%", height = "100%", fill = TRUE))))
  )
}

# ---- Inspect a street -------------------------------------------------------
# Where the map panel sits on the picture. The figure is drawn once on a picture device exactly as it is for the screen, and the layout the drawing
# produced is read back from grid: the map is the largest panel, and its corners are converted to fractions of the picture. Reading the layout, rather than
# a differently drawn copy, matters because text wraps and measures a little differently on other devices, which moved the panel by up to 0.03 of the height.
# Returns the panel's edges as fractions of the picture (from the top left) and the map coordinates (meters) at those edges.
map_panel <- function(p, style = "classic", size = fig_in, dpi = preview_dpi) {
  showtext_opts(dpi = dpi); on.exit(showtext_opts(dpi = preview_dpi), add = TRUE)
  f <- tempfile(fileext = ".png"); on.exit(unlink(f), add = TRUE)
  ragg::agg_png(f, width = size[1], height = size[2], units = "in", res = dpi, background = get_style(style)$bg)
  dev_id <- dev.cur(); on.exit(if (dev_id %in% dev.list()) dev.off(dev_id), add = TRUE)
  box <- tryCatch({
    print(p)
    grid::grid.force()
    vps <- grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
    hit <- which(vps$type == "vpListing" & grepl("^panel\\.[0-9]+-[0-9]+-[0-9]+-[0-9]+$", vps$name))
    best <- NULL; area <- -1
    for (i in hit) {
      parts <- c(strsplit(as.character(vps$vpPath[i]), "::", fixed = TRUE)[[1]], vps$name[i]); parts <- parts[nzchar(parts) & parts != "ROOT"]
      grid::upViewport(0); ok <- tryCatch({ grid::downViewport(do.call(grid::vpPath, as.list(parts))); TRUE }, error = function(e) FALSE); if (!ok) next
      x <- grid::deviceLoc(grid::unit(c(0, 1), "npc"), grid::unit(c(0, 1), "npc"), valueOnly = TRUE, device = TRUE)
      a <- abs(diff(x$x) * diff(x$y)); if (is.finite(a) && a > area) { area <- a; best <- x }
    }
    grid::upViewport(0); best
  }, error = function(e) NULL)
  if (is.null(box)) return(NULL)
  pp <- ggplot2::ggplot_build(p[[1]])$layout$panel_params[[1]]
  if (is.null(pp$x_range) || is.null(pp$y_range)) return(NULL)
  W <- size[1] * dpi; H <- size[2] * dpi                          # device locations are in pixels, y counted from the top
  list(x0 = min(box$x) / W, x1 = max(box$x) / W, y0 = min(box$y) / H, y1 = max(box$y) / H, xr = pp$x_range, yr = pp$y_range)
}
# A point on the picture (fractions from the top left) to map coordinates, or NULL if it is outside the map.
panel_xy <- function(panel, fx, fy) {
  if (is.null(panel) || !is.finite(fx) || !is.finite(fy)) return(NULL)
  u <- (fx - panel$x0) / (panel$x1 - panel$x0); v <- (fy - panel$y0) / (panel$y1 - panel$y0)
  if (u < 0 || u > 1 || v < 0 || v > 1) return(NULL)
  c(x = panel$xr[1] + u * diff(panel$xr), y = panel$yr[2] - v * diff(panel$yr))
}
# The street nearest to a click, with what OpenStreetMap says about it. A click farther than a small distance from any street finds none.
inspect_street <- function(lines, panel, fx, fy, radius_m, crs_m, keep = NULL) {
  xy <- panel_xy(panel, fx, fy)
  if (is.null(xy)) return(list(status = "outside"))
  cand <- if (is.null(keep)) seq_len(nrow(lines)) else keep                # only streets that are drawn can be picked
  if (!length(cand)) return(list(status = "none", tol_m = max(15, radius_m * 0.015)))
  dd <- as.numeric(st_distance(st_sfc(st_point(xy), crs = crs_m), st_geometry(lines[cand, ])))
  k <- which.min(dd); i <- cand[k]; d <- rep(NA_real_, nrow(lines)); d[cand] <- dd; tol <- max(15, radius_m * 0.015)
  if (!length(i) || dd[k] > tol) return(list(status = "none", tol_m = tol))
  row <- lines[i, ]; nm <- as.character(row$name)
  same <- if (!is.na(nm) && nzchar(nm)) intersect(which(lines$name == nm & lines$highway == row$highway), cand) else i
  segs <- segments_of(row); h <- if (nrow(segs)) piece_heading(segs, 1) else NA_real_
  sp <- parse_speed(row$maxspeed)
  list(status = "ok", name = if (is.na(nm) || !nzchar(nm)) NA_character_ else nm, kind = gsub("_", " ", as.character(row$highway)), role = as.character(row$role),
       speed = if (is.na(sp$value)) NA_character_ else paste(sp$value, if (sp$mph) "mph" else "km/h"), heading = if (is.na(h)) NA_character_ else dir_labels[dir_bin(h)],
       piece_km = row$km, street_km = sum(lines$km[same]), n_pieces = length(same), dist_m = d[i], idx = same, piece = i, walk = isTRUE(row$walk_tag), bike = isTRUE(row$bike_tag))
}
fmt_len_km <- function(km) {
  if (identical(unit_env$u, "mi")) { mi <- km / km_per_mile; if (mi >= 0.5) paste0(format(round(mi, 1), nsmall = 1), " mi") else paste0(format(round(km * 3280.84 / 10) * 10, big.mark = ","), " ft") }
  else if (km >= 1) paste0(format(round(km, 1), nsmall = 1), " km") else paste0(round(km * 1000), " m")
}
# Rows of the street table that are actually drawn for a map type (local streets are hidden in the hierarchy map unless asked for).
drawn_rows <- function(lines, view, show_local) {
  if (get_view(view) == "hierarchy" && !isTRUE(show_local)) which(lines$role != roles$role[5]) else seq_len(nrow(lines))
}
# Streets as polylines in picture fractions (from the top left), ready for the browser to draw over the map. Long streets are simplified a little first.
hl_polylines <- function(lines, idx, panel, radius_m, max_lines = 1500) {
  if (is.null(panel) || !length(idx)) return(list())
  idx <- utils::head(idx, max_lines)
  g <- suppressWarnings(st_simplify(st_geometry(lines[idx, ]), dTolerance = max(0.5, radius_m * 0.0008)))
  out <- lapply(seq_along(g), function(k) {
    m <- tryCatch(st_coordinates(g[[k]])[, 1:2, drop = FALSE], error = function(e) NULL)
    if (is.null(m) || nrow(m) < 2) return(NULL)
    fx <- panel$x0 + (m[, 1] - panel$xr[1]) / diff(panel$xr) * (panel$x1 - panel$x0)
    fy <- panel$y0 + (panel$yr[2] - m[, 2]) / diff(panel$yr) * (panel$y1 - panel$y0)
    paste(round(fx, 5), round(fy, 5), sep = ",")
  })
  Filter(Negate(is.null), out)
}
# Find streets by name: every word typed must appear in the name, ignoring case and accents. keep = the rows that are drawn.
name_key <- function(x) tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII"))   # stringi comes with stringr; iconv gives NA for accents in some locales
street_search <- function(lines, q, keep = NULL) {
  q <- trimws(as.character(q %||% ""))
  if (nchar(q) < 2) return(list(status = "short"))
  if (nchar(q) > 80) return(list(status = "long"))
  words <- strsplit(name_key(q), "\\s+")[[1]]; words <- words[nzchar(words)]
  nk <- name_key(lines$name); nk[is.na(lines$name)] <- ""
  hit <- nzchar(nk) & Reduce(`&`, lapply(words, function(w) grepl(w, nk, fixed = TRUE)))
  cand <- if (is.null(keep)) seq_len(nrow(lines)) else keep
  shown <- intersect(which(hit), cand)
  if (!length(shown)) return(list(status = if (any(hit)) "hidden" else "none", q = q))
  nm <- as.character(lines$name[shown]); by <- split(shown, nm)
  items <- tibble(name = names(by), km = vapply(by, function(i) sum(lines$km[i]), 0), n = lengths(by))
  items <- items[order(-items$km), ]
  list(status = "ok", q = q, idx = shown, items = items)
}
inspect_card <- function(res, units = "km") {
  with_units(units, {
    if (is.null(res) || !is.null(res$status) && res$status %in% c("outside", "none")) return(NULL)      # the card shows only a street that is selected; a click on nothing leaves no card
    row <- function(k, v) div(class = "inspect-row", span(class = "inspect-k", k), span(class = "inspect-v", v))
    div(class = "inspect",
        div(class = "inspect-title", if (is.na(res$name)) "Unnamed street" else res$name),
        row("Kind", paste0(res$kind, " (", tolower(res$role), ")")),
        row("Speed limit", if (is.na(res$speed)) "not mapped" else res$speed),
        row("Runs", if (is.na(res$heading)) "unknown" else res$heading),
        row("Sidewalk", if (isTRUE(res$walk)) "tagged" else "not tagged"), row("Bike lane", if (isTRUE(res$bike)) "tagged" else "not tagged"),
        row("This piece", fmt_len_km(res$piece_km)),
        if (res$n_pieces > 1) row("Whole street", paste0(fmt_len_km(res$street_km), " in ", res$n_pieces, " pieces")))
  })
}

# ---- Explore history: the roads of the Roman Empire -------------------------------------------------------------------------------
# Real scholarly data, bundled with the app (data/itinere_simplified.rds): the Itiner-e road atlas (de Soto, Pazout, Brughmans and Vahlstrup, CC BY 4.0), with each
# segment's travel time from Rome added from Hotham's "By Road and by Sea" (CC BY 4.0). Nothing here is downloaded while the app runs.
history_cache <- new.env()
history_file <- function(name) {
  for (d in c(Sys.getenv("OVERMAPS_DATA", unset = ""), "data", file.path(shiny::getShinyOption("appDir", "."), "data")))
    if (nzchar(d) && file.exists(file.path(d, name))) return(file.path(d, name))
  NULL
}
roman_roads <- function() {
  if (!is.null(history_cache$roads)) return(history_cache$roads)
  f <- history_file("itinere_simplified.rds"); if (is.null(f)) return(NULL)
  r <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.null(r) && !all(c("kind", "status", "slope") %in% names(r))) r <- NULL
  history_cache$roads <- r; r
}
# Approximate boxes (west, south, east, north) for looking at one part of the empire at a time.
roman_regions <- list(
  empire = list(label = "Whole empire", bb = c(-10, 22, 42, 56)),
  italy = list(label = "Italy", bb = c(6.5, 36, 19, 47.5)),
  britain = list(label = "Britain", bb = c(-6.5, 49.8, 2, 56.5)),
  gaul = list(label = "Gaul and the Rhine", bb = c(-5.5, 42, 10, 52)),
  iberia = list(label = "Iberia", bb = c(-10, 35.5, 4, 44)),
  danube = list(label = "Balkans and the Danube", bb = c(12, 41, 30, 49)),
  greece = list(label = "Greece and the Aegean", bb = c(19, 34.5, 29.5, 42)),
  anatolia = list(label = "Anatolia", bb = c(26, 35.5, 45, 42.5)),
  levant = list(label = "Levant and Egypt", bb = c(24, 22, 40, 37.5)),
  africa = list(label = "North Africa", bb = c(-10, 28, 25, 38)))
roman_region_choices <- setNames(names(roman_regions), vapply(roman_regions, function(x) x$label, ""))
roman_views <- c(kind = "Main and secondary roads", evidence = "Strength of the evidence", slope = "Steepness", days = "Days from Rome")
roman_view_blurbs <- c(kind = "Which routes the atlas counts as main roads.",
                       evidence = "How sure the scholars are that the road ran there.",
                       slope = "Average slope along each stretch, from elevation data.",
                       days = "How long it took to reach each stretch from Rome.")
roman_how <- c(foot = "On foot", cart = "By cart", courier = "By courier")
# Day-count class breaks for each way of travelling, so the colors mean something in each case.
roman_day_breaks <- list(foot_road = c(15, 30, 60, 120), cart_road = c(8, 15, 30, 60), courier_road = c(5, 10, 20, 40),
                         foot_sea = c(5, 10, 20, 40), cart_sea = c(4, 8, 15, 30), courier_sea = c(3, 6, 12, 24))
roman_pick <- function(x, choices, default) { x <- as.character(x %||% default)[1]; if (isTRUE(x %in% choices)) x else default }
roman_crs <- function(bb) sprintf("+proj=laea +lat_0=%.3f +lon_0=%.3f +datum=WGS84 +units=m", mean(bb[c(2, 4)]), mean(bb[c(1, 3)]))
roman_classes <- function(r, view, how, sea) {
  if (view == "kind") return(list(cls = factor(ifelse(r$kind == "main", "Main road", "Secondary road"), c("Main road", "Secondary road")), legend = "Road"))
  if (view == "evidence") return(list(cls = factor(c(certain = "Certain", conjectured = "Conjectured", hypothetical = "Hypothetical")[r$status], c("Certain", "Conjectured", "Hypothetical")), legend = "Where the road ran"))
  if (view == "slope") {
    br <- c(1, 2, 4, 8); lab <- c("Under 1", "1 to 2", "2 to 4", "4 to 8", "8 or more")
    return(list(cls = factor(lab[findInterval(r$slope, br) + 1], lab), legend = "Average slope, as recorded in Itiner-e"))
  }
  key <- paste0(how, "_", if (isTRUE(sea)) "sea" else "road"); br <- roman_day_breaks[[key]]
  lab <- c(paste("Under", br[1]), paste(br[1:3], "to", br[2:4]), paste(br[4], "or more"))
  v <- r[[paste0("d_", key)]]
  list(cls = factor(lab[findInterval(v, br) + 1], lab), legend = paste0("Days from Rome ", tolower(roman_how[[how]]), if (isTRUE(sea)) ", using the sea in summer" else ", by road only"))
}
roman_data <- function(region = "empire", view = "kind", how = "foot", sea = FALSE) {
  region <- roman_pick(region, names(roman_regions), "empire"); view <- roman_pick(view, names(roman_views), "kind")
  how <- roman_pick(how, names(roman_how), "foot"); sea <- isTRUE(sea)
  r <- roman_roads(); if (is.null(r)) return(NULL)
  bb <- roman_regions[[region]]$bb
  box <- st_bbox(c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4]), crs = 4326)
  r <- r[lengths(suppressMessages(st_intersects(r, st_as_sfc(box)))) > 0, ]      # whole stretches that touch the box, so no road is cut off in a straight line
  if (nrow(r) == 0) return(NULL)
  crs <- roman_crs(bb); r <- st_transform(r, crs); r$km <- as.numeric(st_length(r)) / 1000
  cl <- roman_classes(r, view, how, sea); r$cls <- cl$cls
  tot <- sum(r$km); share <- function(i) 100 * sum(r$km[i]) / tot
  list(roads = r, region = region, view = view, how = how, sea = sea, crs = crs, legend = cl$legend, total_km = tot, n = nrow(r),
       main_pct = share(r$kind == "main"), certain_pct = share(r$status == "certain"), conj_pct = share(r$status == "conjectured"), hyp_pct = share(r$status == "hypothetical"),
       slope_med = stats::median(r$slope, na.rm = TRUE), slope_p90 = unname(stats::quantile(r$slope, .9, na.rm = TRUE)),
       days_med = if (view == "days") stats::median(r[[paste0("d_", how, "_", if (sea) "sea" else "road")]], na.rm = TRUE) else NA_real_,
       unreached = if (view == "days") share(is.na(r$cls)) else 0)
}
roman_pct <- function(x) if (is.na(x)) "" else if (x > 0 && x < 1) "under 1%" else paste0(round(x), "%")
roman_in <- function(d) if (d$region == "empire") "across the whole empire" else paste("in", roman_regions[[d$region]]$label)
roman_words <- function(d) {
  fk <- function(x) format(round(x), big.mark = ",")
  switch(d$view,
    kind = sprintf("%s km of road %s, in %s mapped stretches. %s of the length is classed as main road and the rest as secondary.", fk(d$total_km), roman_in(d), fk(d$n), roman_pct(d$main_pct)),
    evidence = sprintf("By length, %s of these roads is certain, %s conjectured, and %s hypothetical. Most of the empire's road map is scholars' best reconstruction.", roman_pct(d$certain_pct), roman_pct(d$conj_pct), roman_pct(d$hyp_pct)),
    slope = sprintf("The typical stretch has an average slope of %s, and the steepest tenth is above %s. Values are those recorded in Itiner-e, taken from elevation data.", format(round(d$slope_med, 1), nsmall = 1), format(round(d$slope_p90, 1), nsmall = 1)),
    days = sprintf("The typical stretch is %s days from Rome %s%s.%s", round(d$days_med), tolower(roman_how[[d$how]]), if (d$sea) ", using the sea in summer" else ", by road only",
                   if (d$unreached > 0.5) sprintf(" About %s of the length could not be linked to the travel model and is drawn gray.", roman_pct(d$unreached)) else ""))
}
roman_caption <- function(d) paste0("Roads: Itiner-e, the digital atlas of ancient roads (de Soto, Pažout, Brughmans and Vahlstrup, 2024 static version), CC BY 4.0, simplified for display; it covers the empire about 150 CE and includes conjectured and hypothesized routes",
  if (d$view == "days") "; travel times: Hotham, By Road and by Sea (2026), CC BY 4.0, which models walking, carts, and couriers, and summer sailing" else "", ". Region boxes are approximate.")
roman_map <- function(d, style = "classic") {
  pal <- get_style(style); r <- d$roads; lv <- levels(r$cls); present <- lv[lv %in% unique(as.character(r$cls))]
  cols <- switch(d$view, kind = pal$cols[c(1, 3)], evidence = pal$cols[c(3, 4, 5)], slope = pal$dens[2:6], days = pal$speed)
  lw <- c(main = 0.5, secondary = 0.28)[r$kind]
  r$lw <- unname(lw)
  rome <- st_transform(st_sfc(st_point(c(12.4964, 41.9028)), crs = 4326), d$crs)
  in_view <- length(st_intersects(rome, st_as_sfc(st_bbox(r)))[[1]]) > 0
  base <- ggplot()
  if (d$view == "evidence") {
    base <- base + geom_sf(data = r, aes(color = cls, linewidth = lw, linetype = cls), lineend = "round") +
      scale_linetype_manual(values = c(Certain = "solid", Conjectured = "solid", Hypothetical = "22"), limits = lv, guide = "none")
  } else base <- base + geom_sf(data = r, aes(color = cls, linewidth = lw), lineend = "round")
  p <- base + scale_linewidth_identity() +
    scale_color_manual(values = setNames(cols, lv), limits = present, name = d$legend, na.value = pal$faint) +
    guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5, override.aes = list(linewidth = 1.6)))
  if (d$view == "days" && in_view)
    p <- p + geom_sf(data = st_sf(geometry = rome), shape = 21, size = 3.2, stroke = 1, fill = pal$bg, color = pal$ink) +
      annotate("text", x = st_coordinates(rome)[1] + 0.012 * diff(st_bbox(r)[c("xmin", "xmax")]), y = st_coordinates(rome)[2], label = "Rome", hjust = 0, size = 3.6, color = pal$ink, family = body_font)
  bb <- st_bbox(r)
  # Pad the view to the shape of the page's map area, so a tall region (Britain) or a wide one still fills the width and the title can run across it.
  w <- diff(bb[c("xmin", "xmax")]); h <- diff(bb[c("ymin", "ymax")]); target <- 0.95            # height over width
  if (h / w > target) { w2 <- h / target; xl <- mean(bb[c("xmin", "xmax")]) + c(-1, 1) * w2 / 2; yl <- bb[c("ymin", "ymax")] }
  else { h2 <- w * target; yl <- mean(bb[c("ymin", "ymax")]) + c(-1, 1) * h2 / 2; xl <- bb[c("xmin", "xmax")] }
  p <- p + coord_sf(crs = d$crs, datum = NA, xlim = xl, ylim = yl, expand = TRUE) +
    theme_void(base_family = body_font) +
    theme(legend.position = "bottom", legend.text = element_text(size = 11, color = pal$muted, margin = margin(l = 3, r = 8)), legend.title = element_text(size = 11, color = pal$muted, margin = margin(b = 5)),
          legend.key.width = unit(1.1, "cm"), legend.margin = margin(14, 0, 0, 0))
  p + patchwork::plot_annotation(
    title = "Roads of the Roman Empire", subtitle = paste0(if (d$region == "empire") "" else paste0(roman_regions[[d$region]]$label, ". "), roman_words(d)), caption = roman_caption(d),
    theme = theme(
      plot.title = element_textbox_simple(family = head_font, size = 38, color = pal$ink, margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_textbox_simple(family = body_font, size = 13, color = pal$muted, lineheight = 1.3, margin = margin(0, 0, 12, 0)),
      plot.caption = element_textbox_simple(family = body_font, size = 8.5, color = pal$faint, lineheight = 1.3, margin = margin(10, 0, 0, 0)),
      plot.margin = margin(30, 34, 22, 34), plot.background = element_rect(fill = pal$bg, color = NA)))
}
roman_alt <- function(d) paste0("Map of Roman roads ", roman_in(d), ", colored by ", tolower(roman_views[[d$view]]), ". ", roman_words(d))
# ---- Explore history: streets of ancient cities ----------------------------------------------------------------------------------
# Real roads for other civilizations are not published as open data the way the Roman atlas is, so a city plan here is what volunteers have mapped in
# OpenStreetMap today: the excavated streets and paths of the site. That is evidence, not a reconstruction, and how much of a site is mapped varies, so
# every plan reports how many streets it found. The centers and radii frame each site; the notes are one plain sentence each.
ancient_sites <- tibble::tribble(
  ~id,           ~name,                    ~civ,                  ~lat,     ~lon,      ~radius_km, ~note,
  "pompeii",     "Pompeii, Italy",         "Roman",               40.7506,  14.4869,   0.8, "Buried by the eruption of Vesuvius in 79 CE and excavated since the 1700s.",
  "ostia",       "Ostia Antica, Italy",    "Roman",               41.7557,  12.2909,   1.0, "The port city of Rome, at the mouth of the Tiber.",
  "timgad",      "Timgad, Algeria",        "Roman",               35.4844,   6.4686,   0.5, "A Roman colony founded around 100 CE and laid out on a grid.",
  "leptis",      "Leptis Magna, Libya",    "Roman",               32.6385,  14.2900,   1.0, "A Phoenician and later Roman port city on the Mediterranean coast.",
  "ephesus",     "Ephesus, Turkey",        "Greek and Roman",     37.9394,  27.3417,   1.0, "A Greek and then Roman city near the Aegean coast.",
  "priene",      "Priene, Turkey",         "Greek",               37.6600,  27.2970,   0.4, "A Greek city laid out on a regular grid on a hillside.",
  "delos",       "Delos, Greece",          "Greek",               37.3960,  25.2680,   0.6, "A sacred island and trading port in the Cyclades.",
  "knossos",     "Knossos, Greece",        "Minoan",              35.2980,  25.1631,   0.4, "The palace center of Bronze Age Crete.",
  "petra",       "Petra, Jordan",          "Nabataean",           30.3285,  35.4444,   1.2, "The Nabataean capital, cut into rock around a colonnaded street.",
  "palmyra",     "Palmyra, Syria",         "Palmyrene",           34.5504,  38.2684,   1.0, "A caravan city in the Syrian desert with a long colonnaded main street.",
  "mohenjo",     "Mohenjo-daro, Pakistan", "Indus Valley",        27.3292,  68.1389,   0.8, "A city of the Indus Valley civilization from the third millennium BCE.",
  "angkor_thom", "Angkor Thom, Cambodia",  "Khmer",               13.4412, 103.8590,   1.8, "The walled Khmer capital founded in the late 12th century, centered on the Bayon.",
  "teotihuacan", "Teotihuacan, Mexico",    "Teotihuacan",         19.6925, -98.8438,   2.5, "A great city of central Mexico, laid out along the Avenue of the Dead.",
  "tikal",       "Tikal, Guatemala",       "Maya",                17.2220, -89.6237,   1.5, "A Maya city in the rainforest, linked by raised causeways.",
  "machu",       "Machu Picchu, Peru",     "Inca",               -13.1631, -72.5450,   0.6, "An Inca royal estate in the Andes, built on terraces in the 15th century.")
ancient_site_choices <- lapply(split(ancient_sites, factor(ancient_sites$civ, levels = unique(ancient_sites$civ))), function(d) setNames(d$id, d$name))
ancient_site <- function(id) { r <- ancient_sites[ancient_sites$id == as.character(id %||% "")[1], ]; if (nrow(r) == 1) as.list(r) else NULL }
# A plan reads the way the main map types do, without the road hierarchy, which means nothing for streets that predate the idea of road classes.
city_views <- c(direction = "Direction", blocks = "Block length", density = "Street density")
city_view_blurbs <- c(direction = "Which way the streets run, with a compass rose. Grids show as two colors.",
                      blocks = "Distance between junctions: short, even blocks mean a planned grid.",
                      density = "Street length per square kilometer, in hexagons.")
city_view_names <- lapply(names(city_views), function(k) tagList(span(class = "vt", city_views[[k]]), span(class = "vb", city_view_blurbs[[k]])))
roman_view_names <- lapply(names(roman_views), function(k) tagList(span(class = "vt", roman_views[[k]]), span(class = "vb", roman_view_blurbs[[k]])))
# What a download found, in words: how many mapped streets, how long, and a warning when the site is thinly mapped.
city_coverage <- function(lines) {
  if (is.null(lines) || nrow(lines) == 0) return(list(n = 0L, km = 0, text = "OpenStreetMap has no mapped streets or paths here, so there is nothing to draw."))
  n <- if ("osm_id" %in% names(lines)) dplyr::n_distinct(lines$osm_id) else nrow(lines); km <- sum(lines$km)
  thin <- n < 25
  list(n = n, km = km,
       text = paste0("OpenStreetMap has ", format(n, big.mark = ","), " mapped streets and paths here, ", format(round(km, 1), nsmall = 1), " km in all.",
                     if (thin) " That is few, so the drawing shows what has been mapped, which may be much less than the real site." else ""))
}
city_alt <- function(d) paste0("Map of the mapped streets of ", d$site$name, ", colored by ", tolower(city_views[[d$view]]), ". ", city_coverage(d$lines)$text)
city_map <- function(d, style = "classic") {
  s <- d$site
  make_map(d$lines, s$name, s$lon, s$lat, round(s$radius_km * 1000), d$crs, label_roads = FALSE, style = style, view = d$view, show_local = TRUE, detail = "all", units = "km",
           note = paste0("the streets and paths shown are the site as it is today, drawn by OpenStreetMap volunteers from what has been excavated, ",
                         "so they show the surviving remains and not a reconstruction of the whole ancient city"))
}

get_city_view <- function(x) { x <- as.character(x %||% "direction")[1]; if (!is.na(x) && x %in% names(city_views)) x else "direction" }
history_title <- function(tab) if (identical(tab, "cities")) "Explore history: Streets of ancient cities" else "Explore history: The roads of the Roman Empire"
history_modal <- function(style = "classic") {
  have <- !is.null(roman_roads())
  roads_ui <- if (!have) p(class = "fine", "The road data file (data/itinere_simplified.rds) was not found next to the app, so this map cannot be drawn.")
  else layout_sidebar(class = "history-cols", fillable = FALSE,
    sidebar = sidebar(width = 360, resizable = TRUE, open = "open", title = NULL,
      p(class = "fine", "Real roads from a scholarly atlas, not a drawing: about 294,000 km of routes, drawn where the evidence, or the best guess, places them."),
      section(1, "Choose a region", selectInput("h_region", NULL, roman_region_choices, selected = "empire", selectize = FALSE)),
      section(2, "Choose a map type",
              div(class = "view-picker", radioButtons("h_view", NULL, choiceNames = roman_view_names, choiceValues = names(roman_views), selected = "kind", width = "100%")),
              conditionalPanel("input.h_view == 'days'",
                radioButtons("h_how", "How you travel", setNames(names(roman_how), roman_how), selected = "foot", inline = TRUE),
                input_switch("h_sea", "Use the sea in summer", value = FALSE))),
      section(3, "Choose a look", div(class = "style-picker", radioButtons("h_style", NULL, choiceNames = style_choice_names, choiceValues = names(styles), selected = style, inline = TRUE, width = "100%"))),
      section(4, "Download",
              div(class = "dl-grid", selectInput("h_format", "File type", export_formats, selected = "png", selectize = FALSE)),
              downloadButton("h_download", "Download", class = "btn-outline-primary")),
      p(class = "fine", style = "margin-top:.8rem;", "Roads: ", tags$a(href = "https://itiner-e.org", target = "_blank", rel = "noopener", "Itiner-e"),
        " (CC BY 4.0). Travel times: Tristan Hotham, ", tags$em("By Road and by Sea"), " (CC BY 4.0). Simplified for display.")),
    div(class = "history-map", imageOutput("h_map", width = "100%", height = "auto")))
  cities_ui <- layout_sidebar(class = "history-cols", fillable = FALSE,
    sidebar = sidebar(width = 360, resizable = TRUE, open = "open", title = NULL,
      p(class = "fine", "The streets of ancient cities as they are mapped today. Volunteers draw the excavated remains into OpenStreetMap, so this is what survives and has been mapped, not a reconstruction. Some sites are mapped far more fully than others, and each plan says how much it found."),
      section(1, "Choose a city", selectInput("c_site", NULL, ancient_site_choices, selected = "pompeii", selectize = FALSE), uiOutput("c_note")),
      section(2, "Choose a map type", div(class = "view-picker", radioButtons("c_view", NULL, choiceNames = city_view_names, choiceValues = names(city_views), selected = "direction", width = "100%"))),
      section(3, "Choose a look", div(class = "style-picker", radioButtons("c_style", NULL, choiceNames = style_choice_names, choiceValues = names(styles), selected = style, inline = TRUE, width = "100%"))),
      div(class = "draw-row", actionButton("c_draw", "Draw map", icon = icon("map"), class = "btn-primary")),
      conditionalPanel("output.c_has_map == 'yes'",
        div(class = "ctl", style = "margin-top: 1.1rem;",
            div(class = "ctl-head", span(class = "step", 4), "Download"),
            div(class = "dl-grid", selectInput("c_format", "File type", export_formats, selected = "png", selectize = FALSE)),
            downloadButton("c_download", "Download", class = "btn-outline-primary"))),
      p(class = "fine", style = "margin-top:.8rem;", "Map data © OpenStreetMap contributors (ODbL).")),
    div(class = "history-map", uiOutput("c_status"), imageOutput("c_map", width = "100%", height = "auto")))
  modalDialog(
    title = textOutput("h_title", inline = TRUE),
    size = "xl", easyClose = TRUE, class = "history-modal",
    div(class = "h-wrap",
        tags$button(type = "button", id = "h-full-btn", class = "btn btn-sm btn-outline-secondary h-full-btn", onclick = "overmapsHistoryFull()", `aria-pressed` = "false",
                    title = "Fill the whole window so the map and text are larger",
                    span(class = "when-normal", icon("expand"), " Full screen"), span(class = "when-expanded", icon("compress"), " Exit full screen")),
        navset_pill(id = "h_tab", nav_panel("Roman roads", value = "roads", roads_ui), nav_panel("Ancient cities", value = "cities", cities_ui))),
    footer = modalButton("Close"))
}

# ---- Compare two places -----------------------------------------------------
# Draws the place on screen and a second place side by side, at the same radius, with the same map type, style, and units, so the two can be read against each other.
# The second place is the first match for the text typed. Both are drawn as plain circles (no city outline), so the comparison is like for like.
compare_size <- c(16, 10)
max_compare_extra <- 3        # the open map plus up to three more makes a sheet of four
compare_size_for <- function(n) if (n <= 2) compare_size else c(16, 20)      # two across for two places, a 2 x 2 sheet for three or four
# Places typed in one box, separated by semicolons or new lines.
compare_split <- function(x) { x <- trimws(unlist(strsplit(as.character(x %||% ""), "[;\n]"))); unique(x[nzchar(x)]) }
# Well-known street plans to add to a comparison. Each value is the text sent to the place search; a name that is too vague finds the wrong spot,
# so each carries its city and country. Only the text is stored here; where it lands is up to the search.
compare_presets <- list(
  "Add a well-known street plan..." = "",
  "Planned grids" = c("Midtown Manhattan, New York" = "Midtown Manhattan, New York, United States", "Eixample, Barcelona" = "Eixample, Barcelona, Spain",
                      "Savannah, Georgia" = "Savannah, Georgia, United States", "Salt Lake City, Utah" = "Salt Lake City, Utah, United States", "Kyoto, Japan" = "Kyoto, Japan"),
  "Suburbs and curves" = c("Levittown, New York" = "Levittown, New York, United States", "Irvine, California" = "Irvine, California, United States", "Brasilia, Brazil" = "Plano Piloto, Brasilia, Brazil"),
  "Grown over time" = c("Venice, Italy" = "Venice, Italy", "Fez el-Bali, Morocco" = "Fes el-Bali, Fez, Morocco", "Boston, Massachusetts" = "Boston, Massachusetts, United States"))
compare_add <- function(current, add) {
  cur <- compare_split(current); add <- trimws(as.character(add %||% "")[1])
  if (!nzchar(add) || add %in% cur) return(list(text = paste(cur, collapse = "; "), full = FALSE))
  if (length(cur) >= max_compare_extra) return(list(text = paste(cur, collapse = "; "), full = TRUE))
  list(text = paste(c(cur, add), collapse = "; "), full = FALSE)
}
compare_build <- function(g, query_b, view = "hierarchy", style = "classic", units = "km", all_streets = FALSE, show_local = FALSE, label_roads = TRUE) {
  fail <- function(msg) list(error = msg)
  qs <- compare_split(query_b)
  if (!length(qs)) return(fail("Type the second place first."))
  if (any(nchar(qs) > max_query_chars)) return(fail(paste0("Please keep each place name under ", max_query_chars, " characters.")))
  if (length(qs) > max_compare_extra) return(fail(paste0("Compare up to ", max_compare_extra, " other places at a time.")))
  single <- length(qs) == 1
  radius_m <- g$radius_m; detail <- detail_for(radius_m, all_streets)
  found <- list()
  for (q in qs) {
    who <- if (single) "the second place" else paste0("\"", q, "\"")
    hits <- tryCatch(geocode(q), error = function(e) structure(list(), class = "lookup_failed"))
    if (inherits(hits, "lookup_failed")) return(fail("The place lookup did not answer. Please try again in a moment."))
    if (is.null(hits) || nrow(hits) == 0) return(fail(if (single) "No place found for the second name. Try adding the state or country." else paste0("No place found for ", who, ". Try adding the state or country.")))
    lon_b <- hits$lon[1]; lat_b <- hits$lat[1]
    if (!utm_supported(lat_b)) return(fail("Maps are not available this close to the poles."))
    crs_b <- utm_epsg(lon_b, lat_b)
    lines_b <- tryCatch(prep_lines(fetch_roads(lon_b, lat_b, radius_m, crs_b, NULL, detail = detail), lon_b, lat_b, radius_m, crs_b),
                        error = function(e) { message("compare: ", conditionMessage(e)); structure(list(), class = "lookup_failed") })
    if (inherits(lines_b, "lookup_failed")) return(fail(paste0("OpenStreetMap did not answer for ", who, ". Please try again in a moment.")))
    if (is.null(lines_b)) return(fail(paste0("OpenStreetMap has no mapped streets for ", who, " at this size.")))
    found[[length(found) + 1]] <- list(lines = lines_b, place = short_title(hits$label[1]), label = hits$label[1], lon = lon_b, lat = lat_b, crs = crs_b)
  }
  lines_a <- if (is.null(g$shape)) g$lines else {
    prep_lines(fetch_roads(g$lon, g$lat, radius_m, g$crs_m, NULL, detail = detail), g$lon, g$lat, radius_m, g$crs_m) }
  if (is.null(lines_a)) return(fail(paste0("OpenStreetMap has no mapped streets for ", g$place, " at this size.")))
  one <- function(lines, place, lon, lat, crs_m, center_txt) {
    pth <- if (identical(get_view(view), "active")) load_paths(lon, lat, radius_m, crs_m) else NULL
    pl <- make_map(lines, place, lon, lat, radius_m, crs_m, label_roads = label_roads, style = style, view = view, show_local = show_local, detail = detail, center_txt = center_txt, units = units, paths = pth)
    center_m <- center_of(lon, lat, crs_m); clip <- st_buffer(center_m, radius_m * 0.988)
    net <- network_stats(lines, segments_of(lines), clip, as.numeric(st_area(clip)) / 1e6, radius_m * 0.03)
    vh <- view_data("hierarchy", lines, radius_m, center_m, NULL, detail, NULL, units); vdir <- view_data("direction", lines, radius_m, center_m, NULL, detail, NULL, units)
    list(map = pl, rows = compare_rows(lines, vh, vdir, net, units, detail), place = place)
  }
  all <- c(list(one(lines_a, g$place, g$lon, g$lat, g$crs_m, g$center_txt)), lapply(found, function(f) one(f$lines, f$place, f$lon, f$lat, f$crs, NULL)))
  pal <- get_style(style)
  maps <- lapply(all, function(x) patchwork::wrap_elements(full = x$map)); if (length(maps) == 3) maps[[4]] <- patchwork::plot_spacer()
  fig <- patchwork::wrap_plots(maps, ncol = 2) & theme(plot.background = element_rect(fill = pal$bg, color = NA))
  list(fig = fig, a = all[[1]], b = all[[2]], places = all, label_b = found[[1]]$label, labels = vapply(found, `[[`, "", "label"), size = compare_size_for(length(all)),
       radius_txt = with_units(units, radius_txt(radius_m)), unit = as_unit(units))
}
# The numbers set side by side under the figure (view independent, so the table is the same whichever map type is drawn).
compare_rows <- function(lines, vh, vdir, net, units, detail) {
  with_units(units, {
    main <- vdir$tiles[[2]]; grid <- vdir$tiles[[3]]; nt <- net_tiles(net)
    list(list(label = "Street mapped", value = vh$tiles[[1]]$value),
         list(label = "Local streets", value = if (identical(detail, "all")) vh$tiles[[3]]$value else "not included"),
         list(label = "Main direction", value = paste0(main$value, " (", main$note, ")")),
         list(label = "Grid-aligned streets", value = grid$value),
         list(label = if (is.null(nt)) "Intersections" else nt[[1]]$label, value = if (is.null(nt)) "n/a" else nt[[1]]$value),
         list(label = "Dead ends", value = if (is.null(nt)) "n/a" else nt[[2]]$value),
         list(label = "One-way streets (as tagged)", value = if (is.null(nt)) "n/a" else nt[[3]]$value),
         list(label = "Orderliness (1 is a perfect grid)", value = if (is.null(nt)) "n/a" else nt[[4]]$value))
  })
}

# ---- Share links ------------------------------------------------------------
# A link carries the place text, which match was chosen, the radius in the chosen unit, the map type, style, area, and switches.
# It is read back defensively: anything unrecognized or out of range is ignored, and the values only ever go into form controls.
share_params <- function(input, choice) {
  b <- function(x, default) if (is.null(x)) default else as.integer(isTRUE(x))
  p <- list(q = trimws(input$query %||% ""), c = choice, r = input$radius, u = as_unit(input$units), v = input$view, s = input$style, a = input$area,
            l = b(input$locals, 0L), lb = b(input$labels, 1L), as = b(input$all_streets, 0L), ua = b(input$use_addr, 0L), ad = trimws(input$addr %||% ""))
  p <- Filter(function(x) length(x) == 1 && !is.na(x) && nzchar(as.character(x)), p)
  if (!nzchar(p$q %||% "")) return(NULL)
  p
}
share_url <- function(params, base = "") {
  if (is.null(params)) return("")
  paste0(base, "?", paste0(names(params), "=", vapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE), ""), collapse = "&"), "&go=1")
}
# Turn a query string into the settings it describes; invalid pieces are dropped.
parse_share <- function(qs) {
  x <- tryCatch(shiny::parseQueryString(qs %||% ""), error = function(e) list())
  num <- function(k, lo, hi) { v <- suppressWarnings(as.numeric(x[[k]])); if (length(v) == 1 && is.finite(v) && v >= lo && v <= hi) v else NULL }
  txt <- function(k) { v <- x[[k]]; if (length(v) == 1 && nzchar(trimws(v)) && nchar(v) <= max_query_chars) trimws(v) else NULL }
  pick <- function(k, ok) { v <- x[[k]]; if (length(v) == 1 && v %in% ok) v else NULL }
  flag <- function(k) { v <- x[[k]]; if (length(v) == 1 && v %in% c("0", "1")) v == "1" else NULL }
  out <- list(q = txt("q"), c = num("c", 1, 50), r = num("r", 0.1, 500), u = pick("u", c("km", "mi")), v = pick("v", names(views)), s = pick("s", names(styles)),
              a = pick("a", c("dim", "city", "circle")), l = flag("l"), lb = flag("lb"), as = flag("as"), ua = flag("ua"), ad = txt("ad"), go = identical(x$go, "1"))
  if (!is.null(out$c)) out$c <- as.integer(out$c)
  if (is.null(out$q)) return(NULL)
  Filter(Negate(is.null), out)
}

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # 1. Look up the place; offer the candidates.
  auto_find <- reactiveVal(0); auto_draw <- reactiveVal(0)       # used only when a shared link is opened
  restore <- reactiveValues(pending = NULL, radius_km = NULL, q = NULL)
  picked_text <- reactiveValues(place = NULL, address = NULL)   # the text of the last clicked suggestion or searched name: typing it again suggests nothing
  sugg <- reactiveValues(place = NULL, address = NULL)       # suggestions while typing: list(text, items)
  pick_count <- reactiveVal(0); pick_row <- NULL          # a picked place suggestion: looked up exactly, instead of searched for by name
  addr_pick <- reactiveVal(NULL)                          # a picked address suggestion: list(key, hit)
  pending_addr <- reactiveVal(NULL)                       # an address typed or picked in the one search box, waiting for its place to be found: list(label, lat, lon)
  clear_address <- function() { bslib::update_switch("use_addr", value = FALSE, session = session); updateTextInput(session, "addr", value = ""); addr_pick(NULL) }
  places <- eventReactive(list(input$find, auto_find(), pick_count()), {
    req(!is.null(input$find) || auto_find() > 0 || pick_count() > 0)
    row <- pick_row; pick_row <<- NULL; sugg$place <- NULL
    supplied <- !is.null(restore$q)
    q <- trimws(restore$q %||% (if (!is.null(row)) row$label) %||% input$query %||% "")      # a shared link or a pick supplies its text itself: the text box update has not come back from the browser yet
    restore$q <- NULL
    req(nzchar(q))
    if (!supplied) picked_text$place <- q
    if (nchar(q) > max_query_chars) {
      showNotification(paste0("Please keep the place name under ", max_query_chars, " characters."), type = "warning")
      return(NULL)
    }
    # One box takes a place or a street address. An address names the place it lies in, which is what gets looked up and outlined; the map is then centered on the address.
    pa <- isolate(pending_addr())
    if (is.null(pa) && is.null(row) && !supplied && looks_like_address(q)) {
      hit <- tryCatch(geocode_address(q, NULL, limit = 1), error = function(e) NULL)
      if (is.null(hit) || nrow(hit) == 0) hit <- tryCatch(census_search(q, 1), error = function(e) NULL)      # a U.S. address OpenStreetMap does not have yet
      city <- if (!is.null(hit) && nrow(hit) > 0) city_of_label(hit$label[1]) else ""
      if (nzchar(city)) { pa <- list(label = q, lat = hit$lat[1], lon = hit$lon[1]); pending_addr(pa); q <- city }
      else showNotification("Neither OpenStreetMap nor the Census address file could find that address. Check the spelling, add the city and state, or type just the town or city.", type = "warning")
    }
    if (is.null(pa)) clear_address()
    failed <- FALSE
    withProgress(message = "Looking up place...", value = 0.5, {
      res <- if (!is.null(row)) tryCatch(geocode_lookup(row$osm_type, row$osm_id), error = function(e) NULL)
      if (is.null(res)) res <- tryCatch(geocode(q), error = function(e) {
        failed <<- TRUE
        showNotification(paste("Place lookup failed:", conditionMessage(e)), type = "error"); NULL })
    })
    if (is.null(res) && !failed) showNotification("No place found. Try adding the state or country.", type = "warning")
    res
  })

  # The chosen place, shown as soon as there is one. Several matches (from typing a name and clicking Find) are offered as a short list to choose from;
  # one match, or a suggestion that was clicked, needs no list.
  output$place_card <- renderUI({
    p <- places(); req(p)
    i <- selected()
    parts <- str_trim(strsplit(p$label[i], ",")[[1]])
    div(class = "place-card", role = "status",
        div(class = "place-line", span(class = "place-check", `aria-hidden` = "true", icon("circle-check")),
            div(class = "place-text", div(class = "place-name", parts[1]), if (length(parts) > 1) div(class = "place-where", paste(parts[-1], collapse = ", ")))),
        if (isTRUE(input$use_addr) && !isTRUE(input$area == "city") && nzchar(trimws(input$addr %||% "")))
          div(class = "place-addr", span("Map centered on "), strong(str_trunc(trimws(input$addr), 70)), actionLink("clear_addr", "Use the middle of the place instead")),
        if (nrow(p) > 1)
          selectInput("choice", "Not the right one? Other matches", choices = setNames(seq_len(nrow(p)), str_trunc(p$label, 60)), selected = isolate(restore$pending$c), selectize = FALSE, width = "100%"))
  })
  observeEvent(places(), {
    pa <- pending_addr(); p <- places(); if (is.null(pa) || is.null(p)) return()
    pending_addr(NULL)
    updateTextInput(session, "addr", value = pa$label); bslib::update_switch("use_addr", value = TRUE, session = session)
    addr_pick(list(key = address_key(trimws(pa$label), p, 1L), hit = tibble(label = pa$label, lat = pa$lat, lon = pa$lon)))
  }, ignoreNULL = FALSE)
  observeEvent(input$clear_addr, clear_address())
  output$has_place <- renderText(if (!is.null(tryCatch(places(), error = function(e) NULL))) "yes" else "no")
  outputOptions(output, "has_place", suspendWhenHidden = FALSE)

  # The radius slider follows the chosen place: its range reaches somewhat past the radius that encloses the place.
  selected <- reactive({
    p <- places(); req(p)
    i <- suppressWarnings(as.integer(input$choice %||% 1))
    if (length(i) != 1 || is.na(i) || i < 1 || i > nrow(p)) i <- 1L
    i
  })
  # The slider is in the chosen unit; the plan is computed in kilometers and converted for display.
  current_plan <- reactiveVal(radius_plan(NA_real_))
  apply_plan <- function(plan, value_km = NULL, u = NULL) {
    u <- as_unit(u %||% isolate(input$units))
    if (!is.null(value_km)) plan$value <- min(plan$max, max(plan$min, value_km))
    pu <- radius_plan_units(plan, u)
    updateSliderInput(session, "radius", min = pu$min, max = pu$max, value = pu$value, step = pu$step)
    session$sendCustomMessage("radiusUnit", u)
  }
  observe({
    p <- places(); i <- selected()
    bb <- place_bbox(p, i)
    enc <- tryCatch(enclosing_radius_m(if ("boundary" %in% names(p)) p$boundary[[i]] else NULL, p$lon[i], p$lat[i], bb), error = function(e) NA_real_)
    plan <- radius_plan(enc)
    current_plan(plan)
    pend <- isolate(restore$pending)
    if (!is.null(pend)) {                                  # a shared link: pick the saved match first, then its radius, then draw
      want <- pend$c %||% 1L
      if (want >= 1 && want <= nrow(p) && want != i) { updateSelectInput(session, "choice", selected = want); return() }
      u <- as_unit(pend$u %||% "km"); vkm <- if (!is.null(pend$r)) pend$r * (if (u == "mi") km_per_mile else 1) else NULL
      apply_plan(plan, vkm, u); restore$radius_km <- vkm; restore$pending <- NULL
      if (isTRUE(pend$go)) auto_draw(isolate(auto_draw()) + 1)
      return()
    }
    apply_plan(plan)
  })
  # Switching units keeps the same distance on the slider, shown in the other unit.
  unit_prev <- reactiveVal("km")
  observeEvent(input$units, {
    new_u <- as_unit(input$units); old_u <- unit_prev()
    if (identical(new_u, old_u)) return()
    cur <- suppressWarnings(as.numeric(input$radius))
    value_km <- if (length(cur) == 1 && is.finite(cur)) cur * (if (old_u == "mi") km_per_mile else 1) else NULL
    unit_prev(new_u)
    apply_plan(current_plan(), value_km)
  }, ignoreInit = TRUE)

  output$detail_help <- renderUI({
    u <- as_unit(input$units); f <- if (u == "mi") km_per_mile else 1; r <- function(km) round(km / f, 1)
    p(class = "fine", style = "margin: .2rem 0 .8rem;",
      sprintf("Past %s %s the map keeps main streets only, so it draws quickly (past %s %s, only major roads). ", r(full_detail_km), u, r(main_detail_km), u),
      sprintf("This switch includes every street up to %s %s, which can be slow.", r(all_streets_cap_km), u))
  })

  # Opening a shared link: put the saved settings into the controls, then look the place up (the radius and drawing follow once it is found).
  apply_settings <- function(st) {
    updateTextInput(session, "query", value = st$q)
    if (!is.null(st$u)) updateRadioButtons(session, "units", selected = st$u)
    if (!is.null(st$v)) updateRadioButtons(session, "view", selected = st$v)
    if (!is.null(st$s)) updateRadioButtons(session, "style", selected = st$s)
    if (!is.null(st$a)) updateRadioButtons(session, "area", selected = st$a)
    for (k in c("l", "lb", "as", "ua")) if (!is.null(st[[k]])) bslib::update_switch(c(l = "locals", lb = "labels", as = "all_streets", ua = "use_addr")[[k]], value = st[[k]])
    if (!is.null(st$ad)) updateTextInput(session, "addr", value = st$ad)
    unit_prev(as_unit(st$u))                                   # the units were set by the link, not by a click, so nothing should be converted
    restore$pending <- st; restore$q <- st$q; auto_find(isolate(auto_find()) + 1)
  }
  observeEvent(session$clientData$url_search, {
    st <- parse_share(session$clientData$url_search)
    if (is.null(st) || !is.null(isolate(restore$pending))) return()
    apply_settings(st)
  }, once = TRUE)

  # Saved settings: only when the person turns the switch on. The browser keeps one short line of text (the same options a share link holds, without the address);
  # it is read back once at the start of a visit when no share link was opened, and fills the controls without drawing.
  saved_qs <- reactive({
    p <- share_params(input, suppressWarnings(as.integer(input$choice %||% 1))); if (is.null(p)) return(NULL)
    sub("&go=1$", "", share_url(p[setdiff(names(p), c("ua", "ad"))], ""))
  }) |> debounce(1200)
  observe({
    if (isTRUE(input$remember)) { q <- saved_qs(); if (!is.null(q)) session$sendCustomMessage("saveSettings", list(qs = q)) }
  })
  observeEvent(input$remember, if (identical(input$remember, FALSE)) session$sendCustomMessage("saveSettings", list(qs = NULL)), ignoreInit = TRUE)
  observeEvent(input$forget, {
    session$sendCustomMessage("saveSettings", list(qs = NULL)); bslib::update_switch("remember", value = FALSE)
    showNotification("Saved settings were removed from this browser.", type = "message")
  })
  observeEvent(input$saved_settings, {
    if (!is.null(parse_share(session$clientData$url_search)) || !is.null(isolate(restore$pending))) return()      # a shared link wins
    st <- parse_share(input$saved_settings); if (is.null(st)) return()
    st$go <- FALSE; apply_settings(st); bslib::update_switch("remember", value = TRUE)
  }, once = TRUE)

  # Reset: every map option goes back to its starting value (the place stays).
  observeEvent(input$reset, {
    updateRadioButtons(session, "units", selected = "km"); updateRadioButtons(session, "view", selected = "hierarchy")
    updateRadioButtons(session, "area", selected = "dim"); updateRadioButtons(session, "style", selected = "classic")
    for (k in c("locals", "all_streets", "use_addr", "inspect")) bslib::update_switch(k, value = FALSE)
    bslib::update_switch("labels", value = TRUE); updateTextInput(session, "addr", value = "")
    unit_prev("km"); apply_plan(current_plan(), NULL, "km")
  })

  output$share_box <- renderUI({
    base <- paste0(session$clientData$url_protocol %||% "", if (!is.null(session$clientData$url_hostname)) "//" else "", session$clientData$url_hostname %||% "",
                   if (nzchar(session$clientData$url_port %||% "")) paste0(":", session$clientData$url_port) else "", session$clientData$url_pathname %||% "")
    url <- share_url(share_params(input, suppressWarnings(as.integer(input$choice %||% 1))), base)
    req(nzchar(url))
    tagList(div(class = "find-row",
                tags$input(id = "share_url", type = "text", readonly = "readonly", class = "form-control", value = url, `aria-label` = "Link to this map", onfocus = "this.select()"),
                tags$button(type = "button", id = "share_copy", class = "btn btn-outline-primary", onclick = "streetMapsCopyLink()", "Copy")),
            p(class = "fine", "Opening the link draws this map again with the same place, size, map type, and style. The link contains the place or address you typed, so check it before you share it."))
  })

  # Compare: build both maps, show them in a window with the numbers, and offer the picture as a download.
  cmp <- reactiveVal(NULL)
  observeEvent(input$cmp_go, {
    g <- tryCatch(geo(), error = function(e) NULL)
    if (is.null(g)) { showNotification("Draw a map first, then compare it with another place.", type = "warning"); return() }
    res <- withProgress(message = "Drawing both places", value = 0.4, {
      tryCatch(compare_build(g, input$cmp_query, get_view(input$view), sid(), as_unit(input$units), isTRUE(input$all_streets),
                             isTRUE(input$locals), isTRUE(input$labels)),
               error = function(e) list(error = paste("The comparison could not be drawn:", conditionMessage(e))))
    })
    if (!is.null(res$error)) { showNotification(res$error, type = "warning", duration = 10); return() }
    f <- tempfile(fileext = ".png"); draw_figure(res$fig, f, "png", sid(), preview = TRUE, size = res$size)
    uri <- paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.size(f)))); unlink(f)
    cmp(res)
    names_all <- vapply(res$places, `[[`, "", "place"); n <- length(names_all)
    title <- if (n == 2) paste(names_all, collapse = " and ") else paste0(paste(names_all[-n], collapse = ", "), ", and ", names_all[n])
    showModal(modalDialog(
      title = title, size = "xl", easyClose = TRUE,
      tags$img(src = uri, style = "width: 100%; height: auto; border-radius: .5rem;",
               alt = paste0(if (n == 2) "Two maps side by side" else paste(n, "maps in a grid"), ", each within ", res$radius_txt, " ", res$unit, " of its center: ", title, ", ", get_view(input$view), " view.")),
      p(class = "fine", style = "margin-top: .6rem;", if (n == 2) paste0("The second place is ", res$label_b, ". ") else paste0("The other places are ", paste(res$labels, collapse = "; "), ". "), "All are drawn as plain circles of the same size and the same classes, so they can be read against each other."),
      tags$table(class = "counts-table cmp-table",
        tags$thead(tags$tr(tags$th(""), lapply(names_all, tags$th))),
        tags$tbody(lapply(seq_along(res$a$rows), function(i) tags$tr(tags$td(res$a$rows[[i]]$label), lapply(res$places, function(p) tags$td(p$rows[[i]]$value)))))),
      footer = tagList(downloadButton("cmp_dl", "Download picture (PNG)", class = "btn-outline-primary"), downloadButton("cmp_pdf", "Download PDF", class = "btn-outline-primary"), modalButton("Close"))))
  })
  cmp_file <- function(ext) function() { r <- cmp(); paste0("compare_", paste(vapply(r$places, function(p) file_slug(p$place), ""), collapse = "_"), "_", format(Sys.Date(), "%Y%m%d"), ".", ext) }
  output$cmp_dl <- downloadHandler(filename = cmp_file("png"), content = function(file) { r <- cmp(); req(r); draw_figure(r$fig, file, "png", isolate(sid()), size = r$size) })
  output$cmp_pdf <- downloadHandler(filename = cmp_file("pdf"), content = function(file) { r <- cmp(); req(r); draw_figure(r$fig, file, "pdf", isolate(sid()), size = r$size) })
  observeEvent(input$cmp_preset, {
    if (!nzchar(input$cmp_preset %||% "")) return()
    r <- compare_add(input$cmp_query, input$cmp_preset)
    updateTextInput(session, "cmp_query", value = r$text); updateSelectInput(session, "cmp_preset", selected = "")
    if (r$full) showNotification(paste0("A comparison holds up to ", max_compare_extra, " other places. Remove one first."), type = "warning")
  })

  # Address check: Locate (or Enter) looks the address up right away and shows what was found, so typing is never silent.
  address_key <- function(addr, p, i) paste(addr, i, p$lon[i], p$lat[i], sep = "|")
  addr_hits <- eventReactive(input$find_addr, {
    p <- places(); addr <- trimws(input$addr %||% "")
    i <- suppressWarnings(as.integer(input$choice %||% 1))
    if (is.null(p) || length(i) != 1 || is.na(i) || i < 1 || i > nrow(p)) return(list(status = "noplace"))
    if (!nzchar(addr)) return(list(status = "empty"))
    if (nchar(addr) > max_query_chars) return(list(status = "long"))
    near <- short_title(p$label[i])
    res <- tryCatch(find_address(addr, near, place_bbox(p, i), limit = 5), error = function(e) structure(list(), class = "lookup_failed"))
    key <- address_key(addr, p, i)
    if (inherits(res, "lookup_failed")) return(list(status = "error", key = key))
    if (is.null(res$hits)) return(list(status = "none", key = key, near = near))
    list(status = "ok", key = key, hits = res$hits, near = near, outside = isTRUE(res$outside))
  })
  output$addr_status <- renderUI({
    ap <- addr_pick()
    if (!is.null(ap)) {
      cur <- tryCatch({ p <- places(); address_key(trimws(input$addr %||% ""), p, as.integer(input$choice %||% 1)) }, error = function(e) "")
      if (identical(ap$key, cur)) return(div(class = "fine addr-msg", role = "status", paste0("Selected: ", str_trunc(ap$hit$label, 60), ". The map will be centered here.")))
    }
    if (is.null(input$find_addr) || input$find_addr == 0) return(NULL)
    ah <- addr_hits()
    cur <- tryCatch({ p <- places(); i <- as.integer(input$choice %||% 1); address_key(trimws(input$addr %||% ""), p, i) }, error = function(e) "")
    msg <- switch(ah$status,
      noplace = "Find a place first, then locate the address.",
      empty = "Type an address first.",
      long = paste0("Please keep the address under ", max_query_chars, " characters."),
      error = "The address lookup did not answer. Please try again in a moment.",
      none = paste0("No match near ", ah$near, ". Check the spelling or add the street type."),
      NULL)
    if (!is.null(ah$key) && !identical(ah$key, cur)) return(div(class = "fine addr-msg", role = "status", "The address changed. Click Locate to check it."))
    if (!is.null(msg)) return(div(class = "fine addr-msg", role = "status", msg))
    labs <- vapply(ah$hits$label, \(l) { pr <- str_trim(strsplit(l, ",")[[1]]); str_trunc(paste(head(pr, min(3, length(pr))), collapse = ", "), 60) }, "")
    away <- if (isTRUE(ah$outside)) div(class = "fine addr-msg", role = "status", paste0("Nothing matched inside ", ah$near, ", so these are the closest matches elsewhere."))
    if (nrow(ah$hits) == 1) tagList(away, div(class = "fine addr-msg", role = "status", paste0("Found: ", labs[1], ". The map will be centered here.")))
    else tagList(away, selectInput("addr_choice", "Which address?", choices = setNames(seq_along(labs), labs), width = "100%"))
  })

  # Suggestions while typing. The browser sends the text after a short pause; a lookup is made only when the switch is on, the text is long enough,
  # and the last lookup was not just now. A failed or empty lookup simply shows no suggestions.
  last_sugg <- 0
  observeEvent(input$typed, {
    ty <- input$typed; kind <- ty$kind; text <- trimws(as.character(ty$text %||% ""))
    if (!isTRUE(kind %in% c("place", "address")) || !isTRUE(input$suggest %||% TRUE)) return()
    if (length(text) != 1 || nchar(text) < 3 || nchar(text) > 80) { sugg[[kind]] <- NULL; return() }
    if (identical(picked_text[[kind]], text)) return()      # the box holds the suggestion that was just clicked: nothing more to suggest
    near <- NULL; bias <- NULL
    if (kind == "place" && looks_like_address(text)) {        # a house number with no town: lean toward the place already on the map
      p0 <- tryCatch(places(), error = function(e) NULL)
      if (!is.null(p0) && nrow(p0) > 0) { i0 <- tryCatch(selected(), error = function(e) 1L); bias <- c(p0$lat[i0], p0$lon[i0]) }
    }
    if (kind == "address") {
      if (!isTRUE(input$use_addr)) return()
      p <- tryCatch(places(), error = function(e) NULL); if (is.null(p)) return()
      near <- place_bbox(p, selected())
    }
    now <- as.numeric(Sys.time()); if (now - last_sugg < 0.4) return(); last_sugg <<- now
    res <- tryCatch(photon_search(text, kind, near, bias = bias), error = function(e) NULL)
    # Photon found no street address: try the Census Bureau's file, which needs a city and state in the text (typed, or the place chosen above).
    if (looks_like_address(text) && (is.null(res) || !any(res$is_addr))) {
      cq <- if (kind == "address") { cs <- tryCatch(us_city_state(places()$label[selected()]), error = function(e) ""); if (nzchar(cs)) paste(text, cs, sep = ", ") else "" } else if (grepl(",", text)) text else ""
      if (nzchar(cq)) { cres <- tryCatch(census_search(cq), error = function(e) NULL); if (!is.null(cres)) res <- dplyr::bind_rows(res, cres) }
    }
    sugg[[kind]] <- list(text = text, items = res)
  })
  sugg_list <- function(kind) {
    s <- sugg[[kind]]; if (is.null(s) || is.null(s$items) || nrow(s$items) == 0) return(NULL)
    tags$ul(class = "sugg", role = "listbox", `aria-label` = if (kind == "place") "Place suggestions" else "Address suggestions",
            lapply(seq_len(nrow(s$items)), function(k) tags$li(role = "none", tags$button(type = "button", role = "option", class = "sugg-item", `data-kind` = kind, `data-i` = k, s$items$label[k]))))
  }
  output$place_sugg <- renderUI(sugg_list("place"))
  output$addr_sugg <- renderUI(sugg_list("address"))
  observeEvent(input$pick, {
    pk <- input$pick; kind <- pk$kind; k <- suppressWarnings(as.integer(pk$i))
    if (!isTRUE(kind %in% c("place", "address")) || length(k) != 1 || is.na(k)) return()
    s <- sugg[[kind]]; if (is.null(s$items) || k < 1 || k > nrow(s$items)) return()
    row <- s$items[k, ]; sugg[[kind]] <- NULL; picked_text[[kind]] <- trimws(row$label)
    if (kind == "place" && "is_addr" %in% names(row) && isTRUE(row$is_addr) && nzchar(row$where)) {        # a street address: look up the place it is in, then center the map on the address
      updateTextInput(session, "query", value = row$label); pending_addr(list(label = row$label, lat = row$lat, lon = row$lon))
      restore$q <- row$where; pick_count(isolate(pick_count()) + 1)
    } else if (kind == "place") {
      pending_addr(NULL); updateTextInput(session, "query", value = row$label); pick_row <<- row; pick_count(isolate(pick_count()) + 1)
    } else {
      p <- tryCatch(places(), error = function(e) NULL); if (is.null(p)) return()
      updateTextInput(session, "addr", value = row$label)
      addr_pick(list(key = address_key(trimws(row$label), p, selected()), hit = tibble(label = row$label, lat = row$lat, lon = row$lon)))
    }
  })

  load_count <- 0
  current_addr_sig <- function() {
    addr <- trimws(input$addr %||% "")
    if (isTRUE(input$use_addr) && !isTRUE(input$area == "city") && nzchar(addr)) paste(addr, as.character(input$addr_choice %||% 1), sep = "|") else ""
  }
  # 2. Download and classify the streets when asked (the slow, network-bound step).
  geo_load <- eventReactive(list(input$draw, auto_draw()), {
    req(!is.null(input$draw) || auto_draw() > 0)
    p <- places()
    shiny::validate(shiny::need(!is.null(p), "Type a place and click \"Find\" first."))
    # The browser sliders and menus can be bypassed, so every input is checked again here.
    i <- suppressWarnings(as.integer(input$choice %||% 1))
    shiny::validate(shiny::need(length(i) == 1 && !is.na(i) && i >= 1 && i <= nrow(p),
                                "Please pick one of the places listed, then draw again."))
    r_km <- if (!is.null(restore$radius_km)) restore$radius_km else suppressWarnings(as.numeric(input$radius) * (if (identical(as_unit(input$units), "mi")) km_per_mile else 1))
    restore$radius_km <- NULL                              # only the first draw after opening a link uses the saved radius; after that, the slider rules
    shiny::validate(shiny::need(length(r_km) == 1 && is.finite(r_km), "Please choose a radius."))
    radius_m <- as.integer(round(min(max(r_km, min_radius_km), max_radius_km) * 1000))
    lon <- p$lon[i]; lat <- p$lat[i]
    # Optional address as the center of the map (not used with "City limits only", which is drawn from the boundary).
    center_txt <- NULL
    addr <- trimws(input$addr %||% "")
    if (isTRUE(input$use_addr) && !isTRUE(input$area == "city") && nzchar(addr)) {
      shiny::validate(shiny::need(nchar(addr) <= max_query_chars, paste0("Please keep the address under ", max_query_chars, " characters.")))
      bb <- place_bbox(p, i)
      near <- short_title(p$label[i])
      hit <- NULL
      ah <- tryCatch(addr_hits(), error = function(e) NULL)   # NULL until Locate has been used
      ap <- addr_pick()
      if (!is.null(ap) && identical(ap$key, address_key(addr, p, i))) {
        hit <- ap$hit
      } else if (!is.null(ah) && identical(ah$key, address_key(addr, p, i)) && !is.null(ah$hits)) {
        k <- suppressWarnings(as.integer(input$addr_choice %||% 1))
        if (length(k) != 1 || is.na(k) || k < 1 || k > nrow(ah$hits)) k <- 1L
        hit <- ah$hits[k, ]
      } else {
        res <- tryCatch(find_address(addr, near, bb, limit = 1), error = function(e) structure(list(), class = "lookup_failed"))
        hit <- if (inherits(res, "lookup_failed")) res else if (is.null(res$hits)) NULL else res$hits[1, ]
      }
      shiny::validate(shiny::need(!inherits(hit, "lookup_failed"), "The address lookup did not answer. Please try again in a moment."))
      shiny::validate(shiny::need(!is.null(hit), paste0("OpenStreetMap could not find that address near ", near, ". Check the spelling, or turn off \"Center on a street address\".")))
      far_km <- as.numeric(st_distance(st_sfc(st_point(c(lon, lat)), crs = 4326), st_sfc(st_point(c(hit$lon, hit$lat)), crs = 4326)))[1] / 1000
      if (far_km > 50) showNotification(sprintf("That address is about %d %s from %s, so the map is centered there.", round(far_km / (if (identical(as_unit(input$units), "mi")) km_per_mile else 1)), as_unit(input$units), near), type = "warning", duration = 10)
      lon <- hit$lon; lat <- hit$lat
      parts <- str_trim(strsplit(hit$label, ",")[[1]]); center_txt <- str_trunc(paste(head(parts, min(2, length(parts))), collapse = " "), 44)
    }
    shiny::validate(shiny::need(utm_supported(lat), "Maps are not available this close to the poles."))
    crs_m <- utm_epsg(lon, lat)
    base_lon <- lon; base_lat <- lat                      # the center before a city outline moves it; later option changes start from here

    # City limits when asked for and available; otherwise the circle, with a note saying why.
    shape <- NULL
    if (isTRUE(input$area == "city")) {
      sh <- city_shape(if ("boundary" %in% names(p)) p$boundary[[i]] else NULL, crs_m)
      if (is.null(sh)) {
        showNotification("OpenStreetMap has no city boundary for this match, so the map shows a circle instead. Try another match.", type = "warning", duration = 10)
      } else if (as.numeric(st_area(sh)) > max_city_km2 * 1e6) {
        showNotification(sprintf("This boundary covers %s square kilometers, more than the %d this app draws, so the map shows a circle instead.",
                                 format(round(as.numeric(st_area(sh)) / 1e6), big.mark = ","), max_city_km2), type = "warning", duration = 12)
      } else shape <- sh
    }
    # Circle with the city outlined: the circle is drawn as usual and the boundary is only used to dim what lies outside it.
    inside <- NULL
    if (isTRUE(input$area == "dim")) {
      inside <- city_shape(if ("boundary" %in% names(p)) p$boundary[[i]] else NULL, crs_m)
      if (is.null(inside)) showNotification("OpenStreetMap has no city boundary for this match, so the map shows a plain circle. Try another match.", type = "warning", duration = 10)
    }
    if (!is.null(shape)) {
      radius_m <- as.integer(round(shape_radius(shape)))
      pt <- suppressWarnings(st_coordinates(st_transform(st_point_on_surface(shape), 4326)))
      lon <- pt[1, 1]; lat <- pt[1, 2]
    }

    detail <- detail_for(radius_m, isTRUE(input$all_streets))
    if (detail != "all") showNotification(if (detail == "main") "This area is large, so the map includes main streets only (local streets are left out to keep it fast)."
                                          else "This area is very large, so the map includes only highways, arterials, and major collectors.", type = "message", duration = 8)
    withProgress(message = "Getting streets", value = 0, {
      incProgress(0.2, detail = "Downloading from OpenStreetMap...")
      roads <- fetch_roads(lon, lat, radius_m, crs_m, shape, detail = detail, on_try = function(i, n, url, cap) {
        host <- sub("^https?://([^/]+).*$", "\\1", url)
        tryCatch(setProgress(detail = if (i == 1) sprintf("Downloading from %s (up to %d s)...", host, round(cap))
                                      else sprintf("Trying another server: %s (up to %d s)...", host, round(cap))), error = function(e) NULL)
      })
      incProgress(0.5, detail = "Classifying roads...")
      lines <- prep_lines(roads, lon, lat, radius_m, crs_m, shape)
      shiny::validate(shiny::need(!is.null(lines), "OpenStreetMap has no mapped streets in that area. Try a larger area or another place."))
    })
    clip <- if (is.null(shape)) st_buffer(center_of(lon, lat, crs_m), radius_m * 0.988) else shape
    net <- network_stats(lines, segments_of(lines), clip, as.numeric(st_area(clip)) / 1e6, radius_m * 0.03)
    area_load <- if (is.null(shape)) st_buffer(center_of(lon, lat, crs_m), radius_m) else shape
    load_count <<- load_count + 1
    list(lines = lines, place = short_title(p$label[i]), lon = lon, lat = lat, radius_m = radius_m, crs_m = crs_m, shape = shape, inside = inside,
         detail = detail, center_txt = center_txt, net = net,
         roads = roads, key = place_key(p, i), base_lon = base_lon, base_lat = base_lat, cover = st_as_sfc(st_bbox(st_buffer(area_load, 100))),
         sig = option_sig(r_km, input$area, input$all_streets, current_addr_sig()))
  })

  # What is drawn follows the options at once whenever the streets already downloaded cover them (see plan_live); otherwise the map
  # stays as it is and a note says that a new download is needed. The state is a reactiveVal so that typing in an unrelated box does not redraw anything.
  radius_km_live <- reactiveVal(NULL)
  observeEvent(input$radius, {
    v <- suppressWarnings(as.numeric(input$radius))
    if (length(v) == 1 && is.finite(v)) radius_km_live(v * (if (identical(unit_prev(), "mi")) km_per_mile else 1))       # converted with the unit the slider is in right now
  })
  live_state <- reactive({
    p <- places(); i <- selected()
    list(key = place_key(p, i), radius_km = radius_km_live(), area = input$area, all_streets = input$all_streets, addr_sig = current_addr_sig(),
         boundary = if ("boundary" %in% names(p)) p$boundary[[i]] else NULL)
  })
  geo_state <- reactiveVal(NULL)
  compute_geo <- function() {
    ld <- geo_load()
    st <- tryCatch(live_state(), error = function(e) NULL)
    if (is.null(st)) return(list(g = ld, tok = list(n = load_count)))
    if (is.null(st$radius_km) || !is.finite(st$radius_km)) st$radius_km <- ld$sig$rk
    tok <- list(n = load_count, sig = option_sig(st$radius_km, st$area, st$all_streets, st$addr_sig), same_place = identical(st$key, ld$key))
    if (identical(tok, isolate(geo_state())$tok)) return(NULL)
    w <- plan_live(ld, st)
    if (inherits(w, "need_draw")) {                      # keep showing what is on screen now (a live view, if one was drawn), and say what is missing
      prev <- isolate(geo_state()); keep <- if (!is.null(prev) && is.null(prev$error) && identical(prev$tok$n, load_count)) prev$g else ld
      keep$stale <- w$why; list(g = keep, tok = tok)
    } else list(g = w, tok = tok)
  }
  observe({
    out <- tryCatch(compute_geo(), error = function(e) list(error = e, tok = list(err = conditionMessage(e), n = load_count)))
    if (!is.null(out) && !identical(out$tok, isolate(geo_state())$tok)) geo_state(out)
  }, priority = 10)
  geo <- reactive({
    s <- geo_state(); req(s)
    if (!is.null(s$error)) stop(s$error)
    s$g
  })
  # A change that needs streets that were not downloaded (a larger radius, or a radius small enough that local streets are wanted) loads them on its own once the
  # slider has been still for a moment. Anything else that needs a download, such as a new place, still waits for Draw map.
  auto_reasons <- c("streets farther out", "the local streets", "the city limits")
  auto_tried <- new.env(); auto_tried$sig <- ""
  stale_why <- reactive({ s <- geo_state(); if (is.null(s) || !is.null(s$error)) NULL else s$g$stale })
  stale_settled <- debounce(stale_why, 1200)
  auto_sig <- function() paste(input$radius, input$units, input$area, input$locals, input$all_streets, input$use_addr, sep = "|")
  observeEvent(stale_settled(), {
    why <- stale_settled(); if (is.null(why) || !(why %in% auto_reasons)) return()
    sg <- auto_sig(); if (identical(sg, auto_tried$sig)) return()            # the same request already ran, so a failed download is not retried in a loop
    auto_tried$sig <- sg; auto_draw(isolate(auto_draw()) + 1)
  }, ignoreNULL = TRUE)
  output$need_draw <- renderUI({
    s <- geo_state(); why <- if (is.null(s$error)) s$g$stale else NULL
    if (is.null(why)) return(NULL)
    if (why %in% auto_reasons && !identical(auto_sig(), auto_tried$sig))
      return(div(class = "fine need-draw", role = "status", paste0("Loading ", why, " for this change. The map updates when they arrive.")))
    div(class = "fine need-draw", role = "status", paste0("That change needs ", why, ". Click Draw map to load it. Until then, the map shows the last one drawn."))
  })

  # 3. Build the figure. Changing the style or the labels switch redraws from the downloaded streets, with no new request.
  sid <- reactive(get_style_name(input$style))
  # Bike & walk needs one more download (separate bike and walking ways). It happens only when that map type is chosen, and is kept for the place on screen.
  paths_cache <- new.env(); paths_cache$key <- NULL; paths_cache$value <- NULL
  paths_data <- reactive({
    g <- geo(); req(identical(get_view(input$view), "active"))
    key <- paste(g$lon, g$lat, g$radius_m, if (is.null(g$shape)) "circle" else round(as.numeric(st_area(g$shape))))
    if (!identical(key, paths_cache$key)) {                                  # one download per place on screen, kept while other options change
      val <- withProgress(message = "Loading bike and walking paths", value = 0.5, load_paths(g$lon, g$lat, g$radius_m, g$crs_m, g$shape))
      paths_cache$key <- key; paths_cache$value <- val
    }
    paths_cache$value
  })
  active_paths <- function() if (identical(get_view(input$view), "active")) paths_data() else NULL
  built <- reactive({
    g <- geo(); pth <- active_paths()
    make_map(g$lines, g$place, g$lon, g$lat, g$radius_m, g$crs_m, label_roads = isTRUE(input$labels), style = sid(), view = input$view, units = as_unit(input$units),
             show_local = isTRUE(input$locals), shape = g$shape, inside = g$inside, detail = g$detail, center_txt = g$center_txt, paths = pth)
  })
  # Numbers for the tiles and the alt text; recomputed when the map type changes, never re-downloaded.
  vdata <- reactive({
    g <- geo()
    view_data(input$view, g$lines, g$radius_m, center_of(g$lon, g$lat, g$crs_m), g$shape, g$detail, g$center_txt, as_unit(input$units), g$net, paths = active_paths())
  })

  output$has_map <- renderText(if (tryCatch({ geo(); TRUE }, error = function(e) FALSE)) "yes" else "no")   # drives the Download section
  outputOptions(output, "has_map", suspendWhenHidden = FALSE)

  # Inspect: when the switch is on, a click on the map finds the nearest street. The panel position is measured once per drawn figure.
  insp <- reactiveVal(NULL)
  panel <- reactive(map_panel(built(), sid(), dpi = preview_dpi))      # measured at the dpi of the picture on screen: the layout shifts by a few pixels between dpi values
  observeEvent(list(geo(), input$view), insp(NULL), ignoreInit = TRUE)
  observeEvent(input$map_click, {
    req(isTRUE(input$inspect)); g <- geo(); ck <- input$map_click
    pn <- panel()
    res <- if (is.null(pn)) list(status = "outside") else inspect_street(g$lines, pn, suppressWarnings(as.numeric(ck$fx)), suppressWarnings(as.numeric(ck$fy)), g$radius_m, g$crs_m,
                                                                           keep = drawn_rows(g$lines, input$view, input$locals))
    insp(res)
    show_hl(if (identical(res$status, "ok")) { if (isTRUE(input$hl_all)) res$idx else res$piece } else integer(0))
  })
  # Highlight: the browser draws the chosen streets over the picture, so nothing is redrawn on the server.
  hl_idx <- reactiveVal(integer(0))
  show_hl <- function(idx) {
    hl_idx(idx)
    pn <- tryCatch(panel(), error = function(e) NULL); g <- tryCatch(geo(), error = function(e) NULL)
    lines <- if (is.null(pn) || is.null(g) || !length(idx)) list() else hl_polylines(g$lines, idx, pn, g$radius_m)
    session$sendCustomMessage("hl", list(lines = lines))
  }
  observeEvent(list(geo(), input$view, input$locals, input$units, input$labels, sid()), { found(NULL); show_hl(integer(0)) }, ignoreInit = TRUE)
  observeEvent(input$inspect, if (!isTRUE(input$inspect)) show_hl(integer(0)), ignoreInit = TRUE)
  found <- reactiveVal(NULL)
  observeEvent(input$street_go, {
    g <- geo(); res <- street_search(g$lines, input$street_q, keep = drawn_rows(g$lines, input$view, input$locals))
    found(res); show_hl(if (identical(res$status, "ok")) res$idx else integer(0))
  })
  output$street_found <- renderUI({
    res <- found(); if (is.null(res)) return(NULL)
    old_u <- unit_env$u; unit_env$u <- as_unit(input$units); on.exit(unit_env$u <- old_u, add = TRUE)
    fmt <- fmt_len_km
    msg <- switch(res$status,
      short = "Type at least two letters of a street name.", long = "That is too long for a street name.",
      none = "No street with that name in this map.",
      hidden = "That street exists but is a local street. Turn on Show local streets to see it.",
      ok = NULL)
    if (!is.null(msg)) return(div(class = "fine", role = "status", msg))
    it <- res$items; total <- sum(it$km)
    div(class = "fine street-found", role = "status",
        tags$b(sprintf("%d %s match, %s in all.", nrow(it), if (nrow(it) == 1) "street" else "streets", fmt(total))),
        tags$ul(lapply(utils::head(seq_len(nrow(it)), 8), function(k) tags$li(sprintf("%s: %s", it$name[k], fmt(it$km[k]))))),
        if (nrow(it) > 8) paste0("and ", nrow(it) - 8, " more"))
  })
  output$stats <- renderUI({
    vd <- vdata()
    tile <- function(t) div(class = paste("stat", t$cls), div(class = "stat-value", t$value),
                            div(class = "stat-label", t$label), div(class = "stat-note", t$note))
    card <- if (isTRUE(input$inspect)) inspect_card(insp(), as_unit(input$units))
    net <- if (length(vd$net_tiles)) tagList(div(class = "stat-head", "Street network"), div(class = "stat-row stat-mini", lapply(vd$net_tiles, tile)))
    do.call(div, c(list(class = "stat-row"), lapply(vd$tiles, tile), list(net), list(card),
                   list(p(class = "fine zoom-hint", "Click the map to open it at full resolution. Click again to fit the window. Press Esc to close."))))
  })

  output$map <- renderImage({
    g <- geo(); vd <- vdata()
    p <- withProgress(message = "Drawing map", value = 0.5, built())
    f <- tempfile(fileext = ".png")
    draw_figure(p, f, "png", sid(), preview = TRUE)
    list(src = f, contentType = "image/png", alt = fig_alt(g$place, vd, g$radius_m))
  }, deleteFile = TRUE)

  print_key <- reactive(if (isTRUE(input$print_size %in% names(print_sizes))) input$print_size else "8x10")
  output$print_note <- renderUI({
    fmt <- if (isTRUE(input$format %in% export_formats)) input$format else "png"; pl <- print_plan(print_key(), fmt); sz <- paste0(pl$size_in[1], " \u00d7 ", pl$size_in[2], " inches")
    p(class = "fine", style = "margin-top: .6rem;",
      if (is.na(pl$dpi)) paste0("PDF and SVG are laid out for ", sz, " and stay sharp at any size.")
      else paste0("Prints at ", sz, ": ", format(pl$px[1], big.mark = ","), " \u00d7 ", format(pl$px[2], big.mark = ","), " pixels (", pl$dpi, " dpi)",
                  if (pl$dpi < print_dpi) ". The largest sheets use fewer dots per inch to keep the file a manageable size." else ". PDF and SVG stay sharp at any size."),
      " Type and lines grow with the sheet.")
  })
  output$download <- downloadHandler(
    filename = function() {
      g <- tryCatch(geo(), error = function(e) NULL)
      fmt <- if (isTRUE(input$format %in% export_formats)) input$format else "png"
      paste0("overmaps_", get_view(input$view), "_", file_slug(g$place), if (print_key() != "8x10") paste0("_", print_key()), "_", format(Sys.Date(), "%Y%m%d"), ".", format_ext[[fmt]])
    },
    content = function(file) {
      fmt <- if (isTRUE(input$format %in% export_formats)) input$format else "png"
      g <- geo()
      cached <- file.path(export_dir(), paste0(export_key(g$place, g$lon, g$lat, g$radius_m, g$shape, g$inside, input$view, sid(), print_key(),
                                                          isTRUE(input$locals), isTRUE(input$labels), g$detail, g$center_txt, as_unit(input$units), fmt, "v4"), ".", format_ext[[fmt]]))
      if (!file.exists(cached)) {
        tmp <- tempfile(tmpdir = export_dir(), fileext = paste0(".", format_ext[[fmt]]))
        draw_figure(built(), tmp, fmt, sid(), print_key = print_key())
        file.rename(tmp, cached); prune_exports()
      }
      file.copy(cached, file, overwrite = TRUE)
      log_download(fmt, get_view(input$view), split_style(input$style)$name)
      counts_tick(counts_tick() + 1)
    }
  )

  # Download counts: refreshed after this session's downloads and whenever the log file changes (other sessions, other users).
  counts_tick <- reactiveVal(0)
  counts <- reactive({ counts_tick(); invalidateLater(4000, session); summarize_counts(read_counts()) })   # cheap: the file is re-read only when it changed
  output$counts <- renderUI({
    cs <- counts()
    if (cs$total == 0) return(p(class = "fine", "No maps have been downloaded yet."))
    n <- function(x) format(x, big.mark = ",")
    tagList(
      div(class = "count-row",
          div(class = "count-cell", span(class = "count-num", n(cs$total)), span(class = "count-lab", "all time")),
          div(class = "count-cell", span(class = "count-num", n(cs$week)), span(class = "count-lab", "last 7 days")),
          div(class = "count-cell", span(class = "count-num", n(cs$day)), span(class = "count-lab", "last 24 hours"))),
      tags$table(class = "counts-table",
        tags$thead(tags$tr(tags$th("Format"), tags$th("All time"), tags$th("Last 7 days"))),
        tags$tbody(lapply(seq_len(nrow(cs$table)), function(i) tags$tr(tags$td(cs$table$format[i]), tags$td(n(cs$table$all_time[i])), tags$td(n(cs$table$last7[i])))))),
      p(class = "fine", "Counts files this app prepared for download since ", format(cs$first, "%b %d, %Y", tz = "UTC"), " (UTC). ",
        "It cannot tell whether a file was then saved. Nothing about the place or the person is recorded."))
  })


  # Explore history: the Roman roads window.
  observeEvent(input$history_open, showModal(history_modal(isolate(sid()))))
  output$h_title <- renderText(history_title(input$h_tab))
  # Ancient cities: streets are downloaded when Draw map is pressed, then the map type and look change at once from what was downloaded.
  c_state <- reactiveVal(NULL)
  observeEvent(input$c_draw, {
    s <- ancient_site(input$c_site); req(s)
    crs <- utm_epsg(s$lon, s$lat); radius_m <- round(s$radius_km * 1000)
    roads <- tryCatch(withProgress(message = paste0("Getting the streets of ", s$name), value = 0.3,
                        fetch_roads(s$lon, s$lat, radius_m, crs, NULL, detail = "ancient")),
                      error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL })
    if (is.null(roads)) { c_state(NULL); return() }
    lines <- prep_lines(roads, s$lon, s$lat, radius_m, crs)
    c_state(list(site = s, lines = lines, crs = crs, got = city_coverage(lines)))
  })
  c_data <- reactive(c_state())
  c_ready <- reactive({ d <- c_data(); if (is.null(d) || is.null(d$lines)) NULL else d })
  output$c_has_map <- renderText(if (is.null(c_ready())) "no" else "yes"); outputOptions(output, "c_has_map", suspendWhenHidden = FALSE)
  output$c_note <- renderUI({ s <- ancient_site(input$c_site); if (is.null(s)) return(NULL); p(class = "fine", paste0(s$civ, ". ", s$note)) })
  output$c_status <- renderUI({
    d <- c_data()
    if (is.null(d)) return(div(class = "fine", role = "status", "Choose a city and press Draw map. The streets are downloaded from OpenStreetMap, which can take a little while."))
    stale <- !identical(d$site$id, input$c_site)
    tagList(div(class = "fine", role = "status", d$got$text), if (stale) div(class = "fine", role = "status", paste0("This map shows ", d$site$name, ". Press Draw map to see ", ancient_site(input$c_site)$name, ".")))
  })
  c_style <- reactive(get_style_name(input$c_style))
  c_plot <- reactive({ d <- c_ready(); req(d); d$view <- get_city_view(input$c_view); city_map(d, c_style()) })
  output$c_map <- renderImage({
    d <- c_ready(); req(d); v <- get_city_view(input$c_view); d$view <- v
    f <- tempfile(fileext = ".png")
    draw_figure(withProgress(message = "Drawing the plan", value = 0.5, c_plot()), f, "png", c_style(), preview = TRUE)
    list(src = f, contentType = "image/png", alt = city_alt(d), width = "100%", height = "auto")
  }, deleteFile = TRUE)
  output$c_download <- downloadHandler(
    filename = function() { d <- c_ready(); fmt <- if (isTRUE(input$c_format %in% export_formats)) input$c_format else "png"
      paste0("streets_", d$site$id, "_", get_city_view(input$c_view), "_", format(Sys.Date(), "%Y%m%d"), ".", format_ext[[fmt]]) },
    content = function(file) { fmt <- if (isTRUE(input$c_format %in% export_formats)) input$c_format else "png"; draw_figure(c_plot(), file, fmt, c_style()) })
  h_data <- reactive(roman_data(input$h_region, input$h_view, input$h_how, input$h_sea))
  h_style <- reactive(get_style_name(input$h_style))
  output$h_blurb <- renderText(unname(roman_view_blurbs[roman_pick(input$h_view, names(roman_views), "kind")]))
  h_plot <- reactive({ d <- h_data(); req(d); roman_map(d, h_style()) })
  output$h_map <- renderImage({
    d <- h_data(); req(d)
    f <- tempfile(fileext = ".png")
    draw_figure(withProgress(message = "Drawing roads", value = 0.5, h_plot()), f, "png", h_style(), preview = TRUE)
    list(src = f, contentType = "image/png", alt = roman_alt(d), width = "100%", height = "auto")
  }, deleteFile = TRUE)
  output$h_download <- downloadHandler(
    filename = function() {
      d <- h_data(); fmt <- if (isTRUE(input$h_format %in% export_formats)) input$h_format else "png"
      paste0("roman_roads_", d$region, "_", d$view, "_", format(Sys.Date(), "%Y%m%d"), ".", format_ext[[fmt]])
    },
    content = function(file) {
      fmt <- if (isTRUE(input$h_format %in% export_formats)) input$h_format else "png"
      draw_figure(h_plot(), file, fmt, h_style())
    })

  # Expand: fold the sidebar away so the map gets the window, and bring it back on exit.
  observeEvent(input$expanded, bslib::toggle_sidebar("sb", open = !isTRUE(input$expanded)))
}

shinyApp(ui, server)

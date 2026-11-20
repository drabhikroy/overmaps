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
OVERMAPS_VERSION <- "1.1.0"      # keep in step with DESCRIPTION, CITATION.cff, and the newest CHANGELOG.md section; a test checks all four
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
                 dens = c("#c2dbf1", "#9ec3e2", "#74a7d4", "#4985bc", "#20609b", "#053b6c"),   # sparse to dense: one blue hue, light to dark
                 # Street lights: the night sky is the deep navy of this style darkened, and the lamps glow in its orange.
                 night = list(bg = "#0d1826", plate = "#122238", glow = "#e8903a"),
                 # Nature & water: soft water and greens that sit under the streets without competing with them.
                 nature = list(water = "#a9cde6", river = "#6fa5cb", wood = "#98b886", park = "#cfdfb7", street = "#7b838c")),

  dusk    = list(label = "Dusk", bg = "#121c2b", ink = "#f2eee6", muted = "#b5bfcb", faint = "#8794a4",
                 ring = "#2b3c52", plate = "#17263b", cols = c("#ff6b4a", "#f5b14a", "#7fd0e6", "#5b8db3", "#46607c"),
                 speed = c("#4cc2e0", "#6fd1a0", "#f2d14a", "#ff9f43", "#ff5d5d"),
                 dir = c("#648eed", "#6cabe7", "#43d5dc", "#d0bf5e", "#f79c40", "#f1857d", "#e96fa3", "#b67cca"),
                 dens = c("#15364f", "#155276", "#12719c", "#2092be", "#45b6de", "#87dcf7"),   # sparse to dense: dark to bright blue
                 night = list(bg = "#121c2b", plate = "#17263b", glow = "#f5b14a"),
                 nature = list(water = "#1f5c8c", river = "#4aa3d8", wood = "#2a6b57", park = "#1d4a3f", street = "#8ea0b8")),
  mono    = list(label = "Mono", bg = "#ffffff", ink = "#111111", muted = "#454545", faint = "#707070",
                 ring = "#cfcfcf", plate = "#fafafa", cols = c("#000000", "#2e2e2e", "#595959", "#8c8c8c", "#c8c8c8"),
                 speed = c("#8e8e8e", "#707070", "#4d4d4d", "#2a2a2a", "#000000"),
                 dir = c("#000000", "#2f2f2f", "#5c5c5c", "#7a7a7a", "#8c8c8c", "#7a7a7a", "#5c5c5c", "#2f2f2f"),   # gray cannot tell NE from NW
                 dens = c("#e6e6e6", "#c8c8c8", "#a3a3a3", "#7a7a7a", "#4a4a4a", "#111111"),
                 night = list(bg = "#0b0b0b", plate = "#151515", glow = "#ffffff"),
                 nature = list(water = "#505050", river = "#2a2a2a", wood = "#b4b4b4", park = "#dcdcdc", street = "#6a6a6a"))
)
# Color vision. A person can pick a palette that stays readable for their eyes. Each look keeps its paper, ink, and rings and swaps only the colors that carry
# meaning: speed, direction, and the nature map for the three kinds of color blindness, and every color for grayscale (where lightness does all the work).
# Deuteranopia and protanopia share one set because it was checked under both. Hierarchy and density colors already stay apart for all three kinds,
# so they are left alone. Every set was tested under simulated color vision (adjacent steps differ by at least 8 on the OKLab scale, times 100).
cvd_modes <- c("Standard" = "standard", "Deuteranopia (red-green)" = "deutan", "Protanopia (red-green)" = "protan",
               "Tritanopia (blue-yellow)" = "tritan", "Grayscale (no color)" = "gray")
cvd_override <- list(
  classic = list(
    rg = list(speed = c("#aa934b", "#817a5c", "#5b6065", "#364568", "#162767"),
              dir = c("#233166", "#4e4221", "#7a6003", "#a38218", "#969594", "#657fdd", "#3749e6", "#2123b1"),
              nature = list(water = "#92c7de", river = "#67a8df", wood = "#7b885c", park = "#e0f8ce", street = "#6f7379")),
    tritan = list(speed = c("#d67e95", "#a56d78", "#755b5d", "#474843", "#14362b"),
                  dir = c("#41252e", "#710039", "#aa0859", "#d2477d", "#aa8590", "#3c9283", "#006d5f", "#01473d"),
                  nature = list(water = "#9ac5e8", river = "#5aaae3", wood = "#87894b", park = "#efe8b8", street = "#767a80")),
    gray = list(cols = c("#000000", "#2e2e2e", "#595959", "#8c8c8c", "#c8c8c8"), speed = c("#8e8e8e", "#707070", "#4d4d4d", "#2a2a2a", "#000000"),
                dir = c("#000000", "#2f2f2f", "#5c5c5c", "#7a7a7a", "#8c8c8c", "#7a7a7a", "#5c5c5c", "#2f2f2f"),     # gray cannot tell NE from NW, so the ring folds
                dens = c("#e6e6e6", "#c8c8c8", "#a3a3a3", "#7a7a7a", "#4a4a4a", "#111111"),
                nature = list(water = "#505050", river = "#2a2a2a", wood = "#b4b4b4", park = "#dcdcdc", street = "#6a6a6a"),
                night = list(bg = "#0d1826", plate = "#122238", glow = "#ffffff"))),
  dusk = list(
    rg = list(speed = c("#3e67b1", "#6b89ab", "#99a8a1", "#c8c690", "#f9e373"),
              dir = c("#4960b3", "#7a756a", "#ba961c", "#e5bc40", "#ddd1b1", "#aabffd", "#7492fd", "#455efb"),
              nature = list(water = "#205286", river = "#62b9fd", wood = "#456b52", park = "#15413c", street = "#939ca8")),
    tritan = list(speed = c("#4d675d", "#568a7b", "#5caf9b", "#5fd5bb", "#5ffcdd"),
                  dir = c("#b55c7a", "#ec3986", "#ff7eaa", "#febfd1", "#c7ece3", "#46f1d7", "#1ec9b2", "#78918c"),
                  nature = list(water = "#004f77", river = "#3da1d1", wood = "#4c7d58", park = "#113827", street = "#a5aeba")),
    gray = list(cols = c("#ffffff", "#d0d0d0", "#a0a0a0", "#707070", "#484848"), speed = c("#707070", "#909090", "#b0b0b0", "#d0d0d0", "#ffffff"),
                dir = c("#ffffff", "#d8d8d8", "#b0b0b0", "#8c8c8c", "#6a6a6a", "#8c8c8c", "#b0b0b0", "#d8d8d8"),
                dens = c("#262626", "#3c3c3c", "#5a5a5a", "#7e7e7e", "#aaaaaa", "#e0e0e0"),
                nature = list(water = "#2c2c2c", river = "#c4c4c4", wood = "#6e6e6e", park = "#484848", street = "#ececec"),
                night = list(bg = "#121c2b", plate = "#17263b", glow = "#ffffff"))),
  mono = list())
# A style key is a look name, optionally followed by "~" and a color vision mode ("dusk~tritan"). Unknown or missing pieces fall back to Classic and Standard
# (the value comes from the browser, so it is checked here).
split_style <- function(x) {
  x <- as.character(x %||% "classic")[1]; if (is.na(x)) x <- "classic"
  parts <- strsplit(x, "~", fixed = TRUE)[[1]]; nm <- if (length(parts)) parts[1] else "classic"; md <- if (length(parts) > 1) parts[2] else "standard"
  list(name = if (isTRUE(nm %in% names(styles))) nm else "classic", mode = if (isTRUE(md %in% cvd_modes)) md else "standard")
}
get_style_name <- function(x) split_style(x)$name
get_style <- function(x) {
  s <- split_style(x); pal <- styles[[s$name]]
  grp <- switch(s$mode, deutan = "rg", protan = "rg", s$mode)
  ov <- if (s$mode == "standard") NULL else cvd_override[[s$name]][[grp]]
  if (length(ov)) modifyList(pal, ov) else pal
}
style_id <- function(style, mode = NULL) get_style_name(style)
# The key the drawing code receives: the look plus the person's color vision mode (Standard adds nothing, so older keys and cache entries still match).
style_key <- function(style, mode = NULL) {
  m <- split_style(paste0(get_style_name(style), "~", as.character(mode %||% "standard")[1]))$mode
  paste0(get_style_name(style), if (m != "standard") paste0("~", m) else "")
}

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

export_formats <- c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "EPS" = "eps", "PDF" = "pdf", "SVG" = "svg")   # pictures first, then vector files in alphabetical order
raster_formats <- c("png", "jpeg", "tiff")
vector_formats <- c("eps", "pdf", "svg")
# Pictures can be made at more than 300 dpi. "Maximum" uses as many pixels as the file-size cap below allows for the sheet.
resolution_choices <- c("150 dpi" = "150", "300 dpi" = "300", "600 dpi" = "600", "Maximum" = "max")
vector_choice <- c("Vector" = "vector")      # the resolution menu shows only this for PDF, SVG and EPS, which have no dots per inch
res_pick <- function(x) { x <- as.character(x %||% "300")[1]; if (isTRUE(x %in% resolution_choices)) x else "300" }
res_suffix <- function(format, res) { r <- res_pick(res); if (!isTRUE(format %in% raster_formats) || r == "300") "" else if (r == "max") "_max" else paste0("_", r, "dpi") }
format_ext <- c(png = "png", jpeg = "jpg", tiff = "tif", eps = "eps", pdf = "pdf", svg = "svg")

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
  ) |> center_places()
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
         west = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[3]] %||% NA))), east = map_dbl(res, \(r) suppressWarnings(as.numeric(r$boundingbox[[4]] %||% NA)))) |> center_places()
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
# The smallest circle that holds every point (Welzl's method, run over the convex hull). x and y are in meters. Returns c(cx, cy, r).
min_enclosing_circle <- function(x, y) {
  keep <- chull(x, y); px <- x[keep]; py <- y[keep]; n <- length(px)
  if (n == 0) return(c(NA_real_, NA_real_, NA_real_))
  if (n > 1) { set.seed(1); o <- sample.int(n); px <- px[o]; py <- py[o] }
  inside <- function(c, i) sqrt((px[i] - c[1])^2 + (py[i] - c[2])^2) <= c[3] * (1 + 1e-12) + 1e-9
  two <- function(i, j) c((px[i] + px[j]) / 2, (py[i] + py[j]) / 2, sqrt((px[i] - px[j])^2 + (py[i] - py[j])^2) / 2)
  three <- function(i, j, k) {
    ax <- px[i]; ay <- py[i]; bx <- px[j] - ax; by <- py[j] - ay; cx <- px[k] - ax; cy <- py[k] - ay
    d <- 2 * (bx * cy - by * cx)
    if (abs(d) < 1e-12) { cand <- list(two(i, j), two(i, k), two(j, k)); return(cand[[which.max(vapply(cand, `[`, numeric(1), 3))]]) }
    ux <- (cy * (bx^2 + by^2) - by * (cx^2 + cy^2)) / d; uy <- (bx * (cx^2 + cy^2) - cx * (bx^2 + by^2)) / d
    c(ax + ux, ay + uy, sqrt(ux^2 + uy^2))
  }
  c1 <- c(px[1], py[1], 0)
  if (n > 1) for (i in 2:n) if (!inside(c1, i)) {
    c1 <- c(px[i], py[i], 0)
    for (j in seq_len(i - 1)) if (!inside(c1, j)) {
      c1 <- two(i, j)
      if (j > 1) for (k in seq_len(j - 1)) if (!inside(c1, k)) c1 <- three(i, j, k)
    }
  }
  c1
}
# A town or city is mapped around the middle of its own limits, not around the point the geocoder reports (often a town hall or one end of a long city), so the
# circle that just encloses the limits is as small as it can be. Places without a boundary keep their point.
boundary_center <- function(boundary) {
  if (is.null(boundary)) return(NULL)
  tryCatch({
    bb <- st_bbox(boundary); crs_m <- utm_epsg(mean(bb[c("xmin", "xmax")]), mean(bb[c("ymin", "ymax")]))
    xy <- st_coordinates(st_transform(boundary, crs_m))[, c("X", "Y"), drop = FALSE]
    xy <- xy[is.finite(xy[, 1]) & is.finite(xy[, 2]), , drop = FALSE]
    if (nrow(xy) < 3) return(NULL)
    m <- min_enclosing_circle(xy[, 1], xy[, 2])
    ll <- st_coordinates(st_transform(st_sfc(st_point(m[1:2]), crs = crs_m), 4326))
    c(lon = unname(ll[1, 1]), lat = unname(ll[1, 2]))
  }, error = function(e) NULL)
}
center_places <- function(tbl) {
  if (is.null(tbl) || !"boundary" %in% names(tbl)) return(tbl)
  for (i in seq_len(nrow(tbl))) { cc <- boundary_center(tbl$boundary[[i]]); if (!is.null(cc) && all(is.finite(cc))) { tbl$lon[i] <- cc[["lon"]]; tbl$lat[i] <- cc[["lat"]] } }
  tbl
}

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
  step <- if (hi <= 6) 0.25 else if (hi <= 15) 1 else if (hi <= 30) 2.5 else 5
  hi <- min(max_radius_km, ceiling(hi / step) * step)
  lo <- min(max(if (step < 1) 0.5 else step, floor(r / 8 / step) * step), hi - step)
  v <- ceiling(min(r, full_detail_km) / step - 1e-9) * step
  if (v > full_detail_km) v <- floor(full_detail_km / step) * step           # a coarse step must not push the start past the every-street limit
  list(min = lo, max = hi, step = step, value = min(max(v, lo), hi))
}
# How much of the street network to download for a radius: "all" streets, "main" (tertiary and up), or "major" (secondary and up).
# The same plan expressed in the displayed unit (steps and ends are rounded to values that read well in miles).
radius_plan_units <- function(plan, u = "km", cover = FALSE) {
  if (!identical(u, "mi")) return(plan)
  step <- if (plan$step <= 0.25) 0.1 else if (plan$step <= 0.5) 0.25 else if (plan$step <= 1) 0.5 else if (plan$step <= 2.5) 1 else 2.5
  lo <- max(step, ceiling(plan$min / km_per_mile / step - 1e-9) * step)
  hi <- max(lo + step, floor(plan$max / km_per_mile / step + 1e-9) * step)
  list(min = lo, max = hi, step = step, value = min(hi, max(lo, (if (cover) ceiling(plan$value / km_per_mile / step - 1e-9) else round(plan$value / km_per_mile / step)) * step)))
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
mirror_state$status_failed <- list()
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
  if (grepl("^status check failed", m)) {      # judge the cause under the check; only a slow or unreadable one is reported as a missed status check
    inner <- explain_failure(sub("^status check failed: *", "", msg))
    return(if (inner[1] %in% c("other", "slow")) c("slow", "it did not answer a quick status check") else inner)
  }
  if (grepl("could not resolve|resolve host|name or service|getaddrinfo|nodename", m)) c("unreachable", "its address could not be looked up")
  else if (grepl("too many nodes|limit is 50000", m)) c("dense", "this area is too dense for OpenStreetMap's direct download")
  else if (grepl("status check", m)) c("slow", "it did not answer a quick status check")
  else if (grepl("no free query slot", m)) c("busy", "it had no free query slot")
  else if (grepl("status 429|status 406", m)) c("busy", "it is rate-limiting requests")
  else if (grepl("status 50[0-9]|could not finish", m)) c("busy", "it is overloaded")
  else if (grepl("connection refused|failed to connect|could not connect|connect tunnel|couldn't connect|connection reset|ssl|tls|certificate|proxy|recv failure|send failure", m)) c("unreachable", "the connection was refused or interrupted")
  else if (grepl("timeout|timed out", m)) c("slow", "it did not answer in time")
  else if (grepl("status 4", m)) c("rejected", "it rejected the request")
  else c("other", "it failed")
}
# The whole text of an error, including the cause httr2 keeps in its parent (the part that says what curl actually reported).
error_text <- function(e) {
  parts <- conditionMessage(e)
  p <- e$parent
  while (!is.null(p) && inherits(p, "condition")) { m <- conditionMessage(p); if (!grepl(m, parts[1], fixed = TRUE)) parts <- c(parts, m); p <- p$parent }
  trimws(gsub("\\s+", " ", paste(parts, collapse = " ")))
}
host_of <- function(url) sub("^https?://([^/]+).*$", "\\1", url)

# A quick check that a server is alive and has a free query slot, so a dead or swamped one costs seconds rather than a full attempt.
# Servers with no status page, or an unreadable one, are simply tried.
overpass_ready <- function(url, check_s = 6) {
  # A server whose status page just failed is not skipped for good: a status page can be slow or blocked while the query itself works, so the next
  # pass goes straight to the real request instead of failing on the same check again.
  failed_at <- mirror_state$status_failed[[host_of(url)]]
  if (!is.null(failed_at) && as.numeric(difftime(Sys.time(), failed_at, units = "secs")) < 180) return(invisible(TRUE))
  resp <- tryCatch(request(sub("/interpreter$", "/status", url)) |> req_user_agent(overpass_ua) |> req_timeout(check_s) |>
                     req_error(is_error = function(r) FALSE) |> req_perform(), error = function(e) e)
  if (inherits(resp, "error")) {
    mirror_state$status_failed[[host_of(url)]] <- Sys.time()
    stop(sprintf("status check failed: %s", error_text(resp)), call. = FALSE)
  }
  mirror_state$status_failed[[host_of(url)]] <- NULL
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
  tryCatch(osmdata_sf(q, f, quiet = TRUE), error = function(e) {
    # The main site's download cuts relations off at the edge of the box, and osmdata stops with "way can not be found" (or a names length error) when a
    # relation lists a way that was cut away. The streets do not need relations, so read the file again without them rather than lose the whole download.
    plain <- gsub("(?s)<relation[^>]*>.*?</relation>", "", doc, perl = TRUE)
    if (identical(plain, doc)) stop(e)
    writeLines(plain, f, useBytes = TRUE)
    osmdata_sf(q, f, quiet = TRUE)
  })
}

# OpenStreetMap's own map download: every feature in the box, so keep only the street classes we draw.
osm_api_get <- function(bb, cap_s, feature_values = hw, extra = NULL) {
  resp <- request(osm_api_url) |>
    req_url_query(bbox = sprintf("%.6f,%.6f,%.6f,%.6f", bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])) |>
    req_user_agent(overpass_ua) |> req_timeout(cap_s) |> req_error(is_error = function(r) FALSE) |> req_perform()
  status <- resp_status(resp)
  body <- tryCatch(resp_body_string(resp), error = function(e) "")
  if (status != 200) stop(sprintf("server answered with status %d %s", status, substr(gsub("\\s+", " ", body), 1, 80)), call. = FALSE)
  res <- parse_osm_doc(body, opq(bbox = bb))
  if (identical(extra, "lights")) return(keep_lights(res))
  if (identical(extra, "nature")) return(keep_nature(res))
  if (identical(extra, "site")) return(keep_site_extras(res))
  if (!is.null(res$osm_lines) && nrow(res$osm_lines) > 0 && "highway" %in% names(res$osm_lines))
    res$osm_lines <- res$osm_lines[res$osm_lines$highway %in% feature_values, ]
  res
}

# Street lighting: lamps (nodes tagged highway=street_lamp) and the walking and bike ways that carry a lit tag. Streets bring their own lit tag with the
# ordinary download, so only these two are asked for. The main site's download holds every feature, so it is cut down to the same two.
lights_query <- function(bb, timeout_s) {
  box <- sprintf("%.6f,%.6f,%.6f,%.6f", bb[["ymin"]], bb[["xmin"]], bb[["ymax"]], bb[["xmax"]])
  sprintf('[out:xml][timeout:%d];(node["highway"="street_lamp"](%s);way["highway"~"^(%s)$"]["lit"](%s););out body;>;out skel qt;',
          as.integer(timeout_s), box, paste(path_hw, collapse = "|"), box)
}
keep_lights <- function(res) {
  if (!is.null(res$osm_lines) && nrow(res$osm_lines) > 0 && all(c("highway", "lit") %in% names(res$osm_lines)))
    res$osm_lines <- res$osm_lines[res$osm_lines$highway %in% path_hw & !is.na(res$osm_lines$lit), ]
  else res$osm_lines <- NULL
  if (!is.null(res$osm_points) && nrow(res$osm_points) > 0 && "highway" %in% names(res$osm_points))
    res$osm_points <- res$osm_points[res$osm_points$highway %in% "street_lamp", ]
  else res$osm_points <- NULL
  res
}

# One attempt against one server, within `cap_s` seconds in all.
# Nature and water: parks, woods, grass, lakes, riverbank areas, rivers, canals, streams, and the coastline, in one request.
nature_union <- function(box) {
  tags <- c(natural = "water|wood|scrub|grassland|heath|wetland", waterway = "riverbank", leisure = "park|garden|nature_reserve|common",
            landuse = "forest|grass|meadow|village_green|recreation_ground")
  areas <- unlist(lapply(names(tags), function(k) sprintf('%s["%s"~"^(%s)$"](%s);', c("way", "relation"), k, tags[[k]], box)))
  sprintf('%s way["natural"="coastline"](%s);way["waterway"~"^(river|stream|canal)$"](%s);', paste(areas, collapse = ""), box, box)
}
nature_query <- function(bb, timeout_s) {
  box <- sprintf("%.6f,%.6f,%.6f,%.6f", bb[["ymin"]], bb[["xmin"]], bb[["ymax"]], bb[["xmax"]])
  sprintf('[out:xml][timeout:%d];(%s);out body;>;out skel qt;', as.integer(timeout_s), nature_union(box))
}
# Everything the Nature & water and Street lights maps need for one site, in a single request: the nature features above, every street lamp, and every
# street or path that carries a lit tag. Ancient sites are asked for this once, because the public servers are slow and often busy.
site_extras_query <- function(bb, timeout_s) {
  box <- sprintf("%.6f,%.6f,%.6f,%.6f", bb[["ymin"]], bb[["xmin"]], bb[["ymax"]], bb[["xmax"]])
  sprintf('[out:xml][timeout:%d];(%snode["highway"="street_lamp"](%s);way["highway"]["lit"](%s););out body;>;out skel qt;',
          as.integer(timeout_s), nature_union(box), box, box)
}
# What a mapped feature is, from its tags: water, woods, or parks and grass (wetland and scrub count as parks and grass); NA for anything else.
nature_class <- function(x) {
  col <- function(k) if (k %in% names(x)) as.character(x[[k]]) else rep(NA_character_, nrow(x))
  nat <- col("natural"); way <- col("waterway"); lei <- col("leisure"); lu <- col("landuse")
  out <- rep(NA_character_, nrow(x))
  out[lu %in% c("grass", "meadow", "village_green", "recreation_ground") | lei %in% c("park", "garden", "nature_reserve", "common") | nat %in% c("scrub", "grassland", "heath", "wetland")] <- nature_levels[3]
  out[nat %in% "wood" | lu %in% "forest"] <- nature_levels[2]
  out[nat %in% "water" | way %in% "riverbank"] <- nature_levels[1]
  out
}
keep_nature <- function(res) {
  for (k in c("osm_polygons", "osm_multipolygons")) {
    x <- res[[k]]
    if (!is.null(x) && nrow(x) > 0) { x <- x[!is.na(nature_class(x)), ]; res[[k]] <- if (nrow(x) > 0) x else NULL } else res[[k]] <- NULL
  }
  x <- res$osm_lines
  if (!is.null(x) && nrow(x) > 0) {
    way <- if ("waterway" %in% names(x)) x$waterway else NA_character_; nat <- if ("natural" %in% names(x)) x$natural else NA_character_
    x <- x[way %in% c("river", "stream", "canal") | nat %in% "coastline", ]; res$osm_lines <- if (nrow(x) > 0) x else NULL
  } else res$osm_lines <- NULL
  res$osm_points <- NULL
  res
}

# The answer to site_extras_query, split into what each map needs, with only the columns that are used so the saved snapshot stays small.
# lit_ids are the ids of every street or path tagged as lit, so a snapshot of the streets (which does not keep the lit tag) can still be colored by lighting.
keep_site_extras <- function(res) {
  slim <- function(x, cols) if (is.null(x) || nrow(x) == 0) NULL else x[, intersect(cols, names(x))]
  lit_ids <- character()
  x <- res$osm_lines
  if (!is.null(x) && nrow(x) > 0 && all(c("highway", "lit") %in% names(x))) {
    ok <- !is.na(x$highway) & tolower(as.character(x$lit)) %in% lit_yes
    if ("osm_id" %in% names(x)) lit_ids <- unique(as.character(x$osm_id[ok]))
  }
  nat <- keep_nature(res); lgt <- keep_lights(res)
  nat_cols <- c("osm_id", "natural", "waterway", "leisure", "landuse")
  nature <- list(osm_polygons = slim(nat$osm_polygons, nat_cols), osm_multipolygons = slim(nat$osm_multipolygons, nat_cols), osm_lines = slim(nat$osm_lines, nat_cols))
  lights <- list(osm_lines = slim(lgt$osm_lines, c("osm_id", "highway", "lit")), osm_points = slim(lgt$osm_points, c("osm_id", "highway")))
  list(nature = nature, lights = lights, lit_ids = lit_ids,
       n_lamps = if (is.null(lights$osm_points)) 0L else nrow(lights$osm_points), n_lit = length(lit_ids))
}

overpass_get <- function(url, bb, cap_s, feature_values = hw, extra = NULL) {
  t0 <- Sys.time()
  if (identical(url, osm_api_url)) return(osm_api_get(bb, cap_s, feature_values, extra))
  overpass_ready(url, check_s = min(6, cap_s / 3))
  left <- max(5, cap_s - as.numeric(difftime(Sys.time(), t0, units = "secs")))
  q <- opq(bbox = bb, timeout = ceiling(left))
  if (is.null(extra)) q <- q |> add_osm_feature(key = "highway", value = feature_values)
  resp <- request(url) |>
    req_user_agent(overpass_ua) |>
    req_body_form(data = switch(extra %||% "streets", lights = lights_query(bb, ceiling(left)), nature = nature_query(bb, ceiling(left)), site = site_extras_query(bb, ceiling(left)), opq_string(q))) |>
    req_timeout(left) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()
  status <- resp_status(resp)
  if (status != 200) stop(sprintf("server answered with status %d", status), call. = FALSE)
  doc <- resp_body_string(resp)
  # Overpass reports a query that ran out of time or memory inside a normal-looking answer.
  if (grepl("<remark", doc, fixed = TRUE) && grepl("runtime error|timed out|out of memory", doc, ignore.case = TRUE))
    stop("server could not finish the query", call. = FALSE)
  res <- parse_osm_doc(doc, q)
  if (identical(extra, "lights")) keep_lights(res) else if (identical(extra, "nature")) keep_nature(res) else if (identical(extra, "site")) keep_site_extras(res) else res
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
                                       cap = cap, msg = if (ok) "" else error_text(out))
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
  detail <- if (isTRUE(detail %in% c("all", "main", "major", "paths", "ancient", "lights", "nature", "site"))) detail else "all"
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

  # Ancient sites are far from the regional servers, so the global ones are the only choice and get more time and one more pass.
  patient <- identical(detail, "ancient")
  res <- overpass_download(bb, area_km2 = as.numeric(st_area(area)) / 1e6, mirrors = overpass_mirrors_for(bb), on_try = on_try,
                          deadline_s = if (patient) 300 else 180, passes = if (patient) c(1, 2.2, 3.5) else c(1, 2.2),
                          getter = function(u, bb, cap) {
                            out <- overpass_get(u, bb, cap, feature_values = detail_classes(detail), extra = if (detail %in% c("lights", "nature", "site")) detail else NULL)
                            # A busy mirror can answer with a normal-looking page that holds nothing. For an ancient site, where every place has at least tracks and paths,
                            # that counts as a failure so the next server gets a turn. The main OpenStreetMap site is the exception: its empty answer is believed.
                            if (patient && !identical(u, osm_api_url) && (is.null(out$osm_lines) || nrow(out$osm_lines) == 0)) stop("server answered with no streets", call. = FALSE)
                            out
                          })
  if (is.null(res$data)) {
    msg <- download_failure_message(res$tries)
    message("[overmaps] download failed: ", paste(sprintf("%s -> %s", vapply(res$tries, function(t) host_of(t$url), ""),
                                                           vapply(res$tries, function(t) substr(sub("^status check failed: *", "status check: ", gsub("Failed to perform HTTP request\\. *Caused by error in `curl::curl_fetch_memory\\(\\)`: *! *", "", t$msg)), 1, 240), "")), collapse = " | "))
    stop(shiny::safeError(msg))
  }
  # An empty answer for an ancient site is more likely a server hiccup than an empty place, so it is not kept for the next run.
  if (!(patient && (is.null(res$data$osm_lines) || nrow(res$data$osm_lines) == 0))) { saveRDS(res$data, cache); prune_cache() }
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
  tagged <- facility_flags(roads$osm_lines)        # sidewalk, bike lane, and lighting tags, read before the other columns are dropped
  lines <- roads$osm_lines |>
    select(any_of(c("osm_id", "name", "highway", "maxspeed", "bridge", "tunnel", "oneway"))) |>
    st_transform(crs_m)
  lines$walk_tag <- tagged$walk; lines$bike_tag <- tagged$bike; lines$lit_tag <- tagged$lit
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
       bike = any_of_vals("^cycleway([.:](left|right|both))?$", c("lane", "track", "opposite_lane", "opposite_track", "share_busway", "separate", "yes")),
       lit = any_of_vals("^lit$", lit_yes))
}
# Values of the lit tag that mean the street has lighting at least part of the night. "no" and "disused" do not count; missing means unknown.
lit_yes <- c("yes", "24/7", "automatic", "limited", "interval", "sunset-sunrise", "dusk-dawn", "night", "operating_times")
lines_lit <- function(lines) if ("lit_tag" %in% names(lines)) lines$lit_tag else rep(FALSE, nrow(lines))
lights_levels <- c("Lit street", "Lit path", "Not tagged lit", "Street lamps")
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
# Lamps and lit walking and bike ways (the extra download for the Street lights map), clipped like the streets. Lit streets come from the ordinary download.
# The result is never NULL once the download has finished, so "nothing mapped here" can be told apart from "not loaded yet".
prep_lights <- function(roads, lon, lat, radius_m, crs_m, shape = NULL) {
  center_m <- st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_m)
  clip <- if (is.null(shape)) st_buffer(center_m, radius_m * 0.988) else { inner <- st_buffer(shape, -radius_m * 0.012); if (length(inner) == 0 || all(st_is_empty(inner))) shape else inner }
  ways <- NULL; lamps <- NULL
  x <- roads$osm_lines
  if (!is.null(x) && nrow(x) > 0 && all(c("highway", "lit") %in% names(x))) {
    x <- x[x$highway %in% path_hw & tolower(as.character(x$lit)) %in% lit_yes, "highway"]
    if (nrow(x) > 0) {
      x <- suppressWarnings(st_intersection(st_transform(x, crs_m), clip)) |> explode_lines()
      if (nrow(x) > 0) { x$km <- as.numeric(st_length(x$geometry)) / 1000; ways <- x[, "km"] }
    }
  }
  p <- roads$osm_points
  if (!is.null(p) && nrow(p) > 0 && "highway" %in% names(p)) {
    p <- p[p$highway %in% "street_lamp", "highway"]
    if (nrow(p) > 0) {
      p <- st_transform(p, crs_m)
      p <- p[lengths(st_intersects(p, clip)) > 0, ]
      if (nrow(p) > 0) lamps <- st_sf(geometry = st_geometry(p))
    }
  }
  list(ways = ways, lamps = lamps)
}
load_lights <- function(lon, lat, radius_m, crs_m, shape = NULL) {
  prep_lights(fetch_roads(lon, lat, radius_m, crs_m, shape, detail = "lights"), lon, lat, radius_m, crs_m, shape)
}
# Nature and water for the Nature & water map: the parks, woods, and water mapped in OpenStreetMap, clipped to the map and made not to overlap (water over woods over parks),
# plus rivers, canals, and streams as lines. A coastline has the sea on its right, so the sea is rebuilt from it. Never NULL once downloaded.
nature_levels <- c("Water", "Woods", "Parks & grass")
sea_from_coast <- function(coast, clip) {
  tryCatch({
    cg <- suppressWarnings(st_intersection(st_geometry(coast), clip))
    segs <- suppressWarnings(st_cast(st_collection_extract(cg, "LINESTRING"), "LINESTRING"))
    if (length(segs) == 0) return(NULL)
    ring <- suppressWarnings(st_cast(st_boundary(clip), "LINESTRING"))
    whole <- suppressWarnings(st_cast(st_collection_extract(st_geometry(coast), "LINESTRING"), "LINESTRING"))     # the unclipped lines cross the edge exactly, so the pieces close
    pieces <- suppressWarnings(st_cast(st_collection_extract(st_polygonize(st_union(c(whole, st_geometry(ring)))), "POLYGON"), "POLYGON"))
    if (length(pieces) == 0) return(NULL)
    probe <- function(side) st_sfc(lapply(seq_along(segs), function(i) {
      m <- st_coordinates(segs[i])[, 1:2, drop = FALSE]; d <- sqrt(rowSums((m[-1, , drop = FALSE] - m[-nrow(m), , drop = FALSE])^2)); k <- which.max(d)
      v <- m[k + 1, ] - m[k, ]; mid <- (m[k, ] + m[k + 1, ]) / 2; st_point(mid + side * 4 * c(v[2], -v[1]) / sqrt(sum(v^2)))
    }), crs = st_crs(clip))
    wet <- which(lengths(st_intersects(pieces, probe(1))) > 0); dry <- which(lengths(st_intersects(pieces, probe(-1))) > 0)
    wet <- setdiff(wet, dry)
    if (length(wet) == 0) NULL else suppressWarnings(st_intersection(st_union(pieces[wet]), clip))
  }, error = function(e) NULL)
}
prep_nature <- function(roads, lon, lat, radius_m, crs_m, shape = NULL) {
  center_m <- st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_m)
  clip <- st_make_valid(if (is.null(shape)) st_buffer(center_m, radius_m) else shape)
  polys <- NULL
  for (k in c("osm_polygons", "osm_multipolygons")) {
    x <- roads[[k]]
    if (is.null(x) || nrow(x) == 0) next
    cl <- nature_class(x); x <- x[!is.na(cl), ]; cl <- cl[!is.na(cl)]
    if (nrow(x) == 0) next
    polys <- rbind(polys, st_sf(cls = cl, geometry = suppressWarnings(st_make_valid(st_transform(st_geometry(x), crs_m)))))
  }
  as_area <- function(g) {
    if (is.null(g) || length(g) == 0) return(NULL)
    g <- suppressWarnings(st_collection_extract(st_make_valid(g), "POLYGON"))
    if (length(g) == 0) return(NULL)
    u <- suppressWarnings(st_union(g)); if (length(u) == 0 || all(st_is_empty(u))) NULL else u
  }
  grab <- function(name) {
    if (is.null(polys) || !any(polys$cls == name)) return(NULL)
    u <- tryCatch(as_area(suppressWarnings(st_intersection(st_make_valid(st_union(st_geometry(polys)[polys$cls == name])), clip))), error = function(e) NULL)
    u
  }
  water <- grab(nature_levels[1]); wood <- grab(nature_levels[2]); park <- grab(nature_levels[3])
  ln <- roads$osm_lines; ways <- NULL
  if (!is.null(ln) && nrow(ln) > 0) {
    nat <- if ("natural" %in% names(ln)) ln$natural else rep(NA_character_, nrow(ln)); wat <- if ("waterway" %in% names(ln)) ln$waterway else rep(NA_character_, nrow(ln))
    if (any(nat %in% "coastline")) {
      sea <- sea_from_coast(st_transform(ln[nat %in% "coastline", ], crs_m), clip)
      if (!is.null(sea)) water <- if (is.null(water)) as_area(sea) else as_area(suppressWarnings(st_union(water, sea)))
    }
    rv <- ln[wat %in% c("river", "stream", "canal"), ]
    if (nrow(rv) > 0) {
      kind <- ifelse(rv$waterway == "stream", "stream", "river")
      g <- suppressWarnings(st_intersection(st_transform(st_sf(kind = kind, geometry = st_geometry(rv)), crs_m), clip)) |> explode_lines()
      if (nrow(g) > 0) { g$km <- as.numeric(st_length(g$geometry)) / 1000; ways <- g[, c("kind", "km")] }
    }
  }
  cut_out <- function(a, b) if (is.null(a) || is.null(b)) a else tryCatch(as_area(suppressWarnings(st_difference(a, b))), error = function(e) a)
  wood <- cut_out(wood, water); park <- cut_out(cut_out(park, wood), water)
  list(water = water, wood = wood, park = park, ways = ways, area_km2 = as.numeric(st_area(clip)) / 1e6)
}
load_nature <- function(lon, lat, radius_m, crs_m, shape = NULL) {
  prep_nature(fetch_roads(lon, lat, radius_m, crs_m, shape, detail = "nature"), lon, lat, radius_m, crs_m, shape)
}
# Census population for the Population map. The Census Bureau's TIGERweb service returns each 2020 block group with its head count and its land area in one free
# request that needs no key. A block group holds roughly 600 to 3,000 people, so they are fine enough to show how a place is laid out. The service covers the
# United States and its territories only; anywhere else it returns nothing, and the map says so instead of failing.
census_tiger_url <- "https://tigerweb.geo.census.gov/arcgis/rest/services/Census2020/Tracts_Blocks/MapServer/1/query"
census_fetch <- function(bb) {
  env <- paste(format(round(as.numeric(bb[c("xmin", "ymin", "xmax", "ymax")]), 5), nsmall = 5, trim = TRUE), collapse = ",")
  resp <- request(census_tiger_url) |>
    req_url_query(where = "1=1", geometry = env, geometryType = "esriGeometryEnvelope", inSR = 4326, spatialRel = "esriSpatialRelIntersects",
                  outFields = "GEOID,POP100,HU100,AREALAND", returnGeometry = "true", outSR = 4326, geometryPrecision = 5, f = "geojson") |>
    req_user_agent(user_agent) |> req_timeout(30) |> req_retry(max_tries = 2, is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 504)) |> req_perform()
  txt <- resp_body_string(resp)
  if (grepl("\"error\"", substr(txt, 1, 200), fixed = TRUE)) stop("the Census Bureau service returned an error")
  x <- sf::read_sf(txt, quiet = TRUE)
  if (nrow(x) == 0 || !all(c("POP100", "HU100", "AREALAND") %in% names(x))) return(NULL)
  x
}
# Fixed classes in round numbers, in residents per square kilometer or per square mile, so two places can be compared and each legend entry always means the same.
# Homes (the Census's housing units) use lower breaks, since a home holds about two to three people.
census_scheme <- function(u = unit_env$u, measure = "pop") {
  mi <- identical(u, "mi")
  b <- if (measure == "hu") { if (mi) c(100, 400, 1500, 4000, 10000) else c(50, 200, 600, 1500, 4000) } else { if (mi) c(250, 1000, 4000, 10000, 25000) else c(100, 500, 1500, 4000, 10000) }
  f <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  list(breaks = c(0, b, Inf), labels = c(if (measure == "hu") "No housing" else "No residents", paste("Under", f(b[1])), paste(f(b[1:4]), "to", f(b[2:5])), paste(f(b[5]), "or more")),
       area = if (mi) "square mile" else "square kilometer")
}
# One block group's density is spread evenly across it, so a cell is shaded by its block group and the resident total counts the share of each block group that
# lies inside the map. Never NULL; has_census says whether there is anything to draw.
prep_census <- function(bg, lon, lat, radius_m, crs_m, shape = NULL, failed = FALSE) {
  center_m <- st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_m)
  clip <- st_make_valid(if (is.null(shape)) st_buffer(center_m, radius_m) else shape)
  area_km2 <- as.numeric(st_area(clip)) / 1e6
  none <- list(has_census = FALSE, failed = failed, area_km2 = area_km2)
  if (is.null(bg) || nrow(bg) == 0) return(none)
  g <- st_sf(pop = as.numeric(bg$POP100), hu = as.numeric(bg$HU100), land_km2 = as.numeric(bg$AREALAND) / 1e6, geometry = st_make_valid(st_transform(st_geometry(bg), crs_m)))
  g$full <- as.numeric(st_area(g))
  g$dens_km <- ifelse(g$land_km2 > 0, g$pop / g$land_km2, 0); g$hdens_km <- ifelse(g$land_km2 > 0, g$hu / g$land_km2, 0)
  g <- g[is.finite(g$pop) & is.finite(g$hu) & is.finite(g$dens_km) & is.finite(g$hdens_km) & g$full > 0, ]
  cells <- tryCatch(suppressWarnings(st_intersection(g, clip)), error = function(e) NULL)
  if (is.null(cells) || nrow(cells) == 0) return(none)
  cells <- suppressWarnings(st_collection_extract(cells, "POLYGON"))
  if (nrow(cells) == 0) return(none)
  share <- pmin(as.numeric(st_area(cells)) / cells$full, 1)
  list(has_census = TRUE, failed = FALSE, cells = cells[, c("pop", "hu", "dens_km", "hdens_km")], pop = sum(cells$pop * share), hu = sum(cells$hu * share),
       max_dens_km = max(cells$dens_km), max_hdens_km = max(cells$hdens_km), n_bg = nrow(cells), area_km2 = area_km2)
}
load_census <- function(lon, lat, radius_m, crs_m, shape = NULL) {
  center_m <- st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_m)
  clip <- if (is.null(shape)) st_buffer(center_m, radius_m) else shape
  bb <- st_bbox(st_transform(clip, 4326))
  pad <- 0.0005; bb[c("xmin", "ymin")] <- bb[c("xmin", "ymin")] - pad; bb[c("xmax", "ymax")] <- bb[c("xmax", "ymax")] + pad
  bg <- tryCatch(census_fetch(bb), error = function(e) { message("census: ", conditionMessage(e)); structure(list(), class = "lookup_failed") })
  if (inherits(bg, "lookup_failed")) return(prep_census(NULL, lon, lat, radius_m, crs_m, shape, failed = TRUE))
  prep_census(bg, lon, lat, radius_m, crs_m, shape)
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
# Every type is drawn from the same downloaded streets. Two types ask for one more download, only when chosen: Bike & walk (separate paths) and Street lights (lamps and lit paths).
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
# The order of the picker, which fills two columns row by row, so each row is a pair: the roads (class, speed), the grid (direction, block length), how much street
# and who it serves (density, bike and walk), the surroundings (lights, nature and water), and the people who live there (population).
views <- c(hierarchy = "Road hierarchy", speed = "Speed limits", direction = "Direction", blocks = "Block length",
           density = "Street density", active = "Bike & walk", lights = "Street lights", nature = "Nature & water", census = "Population", homes = "Housing")
view_blurbs <- c(hierarchy = "Roads by role, from highways to local streets.",
                 speed = "Posted speed limits, where OpenStreetMap has them.",
                 direction = "Which way the streets run, with a compass rose.",
                 blocks = "Distance between junctions: short blocks are easier to walk.",
                 density = "Street length per square kilometer, in hexagons.",
                 active = "Sidewalks, bike lanes, and paths, as tagged in OpenStreetMap.",
                 lights = "Lit streets and mapped lamps, glowing against the night.",
                 nature = "Parks, woods, rivers, and lakes under the streets.",
                 census = "Residents per square kilometer from the 2020 Census, U.S. places.",
                 homes = "Homes per square kilometer from the 2020 Census, U.S. places.")
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
hex_density <- function(segs, radius_m, center_m, shape = NULL, across = 11) {
  cxy <- as.numeric(st_coordinates(center_m)); s <- radius_m / across   # s = center-to-corner distance; "across" hexagons fit in a radius
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
view_data <- function(view, lines, radius_m, center_m, shape = NULL, detail = "all", center_txt = NULL, units = "km", net = NULL, paths = NULL, fine = FALSE, lights = NULL, nature = NULL, census = NULL) {
  old_u <- unit_env$u; unit_env$u <- as_unit(units); on.exit(unit_env$u <- old_u, add = TRUE)
  vd <- view_data_core(view, lines, radius_m, center_m, shape, center_txt, paths, fine, lights, nature, census)
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
view_data_core <- function(view, lines, radius_m, center_m, shape = NULL, center_txt = NULL, paths = NULL, fine = FALSE, lights = NULL, nature = NULL, census = NULL) {
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

  if (view == "lights") {
    lit_km <- sum(lines$km[lines_lit(lines)]); lit_pct <- if (total_km > 0) 100 * lit_km / total_km else 0
    path_km <- if (is.null(lights$ways)) 0 else sum(lights$ways$km)
    n_lamps <- if (is.null(lights$lamps)) 0L else nrow(lights$lamps)
    pct_txt <- if (lit_pct > 0 && lit_pct < 1) "less than 1%" else paste0(round(lit_pct), "%")
    km_by <- setNames(c(lit_km, path_km, total_km - lit_km), lights_levels[1:3])
    sparse <- lit_pct < 5 && n_lamps < 25
    found <- paste0(pct_txt, " of the street length (", fmt_km(lit_km), ") carries a lit tag, and ", format(n_lamps, big.mark = ","),
                    if (n_lamps == 1) " street lamp is" else " street lamps are", " mapped")
    return(list(view = view, scope = scope, noun = "lit by the lighting tagged in OpenStreetMap", km_by = km_by, levels = lights_levels, n_lamps = n_lamps, lit_pct = lit_pct,
      has_lights = !is.null(lights),
      subtitle = as.character(glue("{pct_txt} of the {round(uv(total_km))} {ulab()} of street {where} is tagged as lit in OpenStreetMap, and {format(n_lamps, big.mark = ',')} ",
                                   "street {if (n_lamps == 1) 'lamp is' else 'lamps are'} mapped.{if (path_km > 0) paste0(' Lit walking and bike paths add ', fmt_km(path_km), '.') else ''}")),
      caption = paste0("lighting is what volunteers have tagged (the lit tag on streets and paths, and individual street lamps), not a survey: a street shown dark may well be lit; here ", found,
                       if (sparse) ", which is very little, so most of this map is dark because lighting is untagged, not because it is unlit" else ""),
      tiles = list(t_total, tile("s-through", pct_txt, "Tagged lit", "share of street length with a lit tag"),
                   tile("s-local", format(n_lamps, big.mark = ","), "Street lamps", "mapped as individual points"))))
  }

  if (view == "nature") {
    a_km2 <- function(g) if (is.null(g)) 0 else as.numeric(sum(st_area(g))) / 1e6
    area_km2 <- if (is.null(shape)) pi * (radius_m / 1000)^2 else as.numeric(st_area(shape)) / 1e6
    km2 <- setNames(c(a_km2(nature$water), a_km2(nature$wood), a_km2(nature$park)), nature_levels); pct <- 100 * km2 / area_km2
    river_km <- if (is.null(nature$ways)) 0 else sum(nature$ways$km)
    pf <- function(p) if (p > 0 && p < 1) "less than 1%" else paste0(round(p), "%")
    green <- pct[[2]] + pct[[3]]; sparse <- green + pct[[1]] < 2
    return(list(view = view, scope = scope, noun = "with the parks, woods, and water mapped in OpenStreetMap", km_by = km2, levels = nature_levels, pct = pct, river_km = river_km,
      has_nature = !is.null(nature), area_km2 = area_km2,
      subtitle = as.character(glue("{pf(green)} of the map is woods, parks, or grass and {pf(pct[[1]])} is water, as mapped in OpenStreetMap. Rivers, canals, and streams run {fmt_km(river_km)}.")),
      caption = paste0("parks, woods, grass, and water are what volunteers have mapped (natural, leisure, landuse, and waterway tags); farmland, private gardens, and anything unmapped are not shown",
                       if (sparse) "; very little is mapped here, so a mostly blank map does not mean there is no green space" else ""),
      tiles = list(t_total, tile("s-through", pf(green), "Woods and parks", "share of the map covered"),
                   tile("s-local", pf(pct[[1]]), "Water", "lakes, rivers, and sea"))))
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

  if (view %in% c("census", "homes")) {
    hu <- identical(view, "homes"); m <- if (hu) "hu" else "pop"
    sc <- census_scheme(measure = m); lv <- sc$labels; fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
    what <- if (hu) "housing units" else "residents"; Noun <- if (hu) "Housing units" else "Residents"
    credit <- if (hu) "housing units are from the 2020 Census (U.S. Census Bureau)" else "population is from the 2020 Census (U.S. Census Bureau)"
    if (!isTRUE(census$has_census)) {
      why <- if (isTRUE(census$failed)) "The Census Bureau did not answer, so nothing is shaded. Try again in a moment."
             else "The Census Bureau publishes counts for the United States and its territories only, so there is nothing to shade for this place."
      return(list(view = view, scope = scope, noun = "with no Census data available", cells = NULL, levels = lv, has_census = FALSE, subtitle = why,
        caption = paste0(credit, "; none is available here"),
        tiles = list(t_total, tile("s-through", "None", Noun, "no Census data for this place"), tile("s-local", "None", "Densest block group", "no Census data for this place"))))
    }
    cells <- census$cells; dk <- if (hu) cells$hdens_km else cells$dens_km; cells$dens <- dk * uf()^2           # per displayed square unit
    k <- findInterval(cells$dens, sc$breaks[-1])                                                              # 0 below the first break up to 5 at or past the last
    cells$cls <- factor(ifelse((if (hu) cells$hu else cells$pop) <= 0, 1L, 2L + pmin(k, 5L)), levels = seq_along(lv), labels = lv)
    tot <- if (hu) census$hu else census$pop; tot_txt <- fmt_n(if (tot >= 1000) signif(tot, 3) else round(tot))
    mx <- (if (hu) census$max_hdens_km else census$max_dens_km) * uf()^2
    return(list(view = view, scope = scope, noun = paste0("shaded by ", what, " per square kilometer, from the 2020 Census"), cells = cells, levels = lv, has_census = TRUE,
      total = tot, max_dens = mx, unit_area = sc$area, what = what,
      subtitle = as.character(glue("About {tot_txt} {if (hu) 'housing units stood' else 'people lived'} {where} at the 2020 Census. The most crowded block group has {fmt_n(round(mx))} {what} per {sc$area}.")),
      caption = paste0(credit, "; each of the ", census$n_bg, " block groups is shaded by its ", what, " per ", sc$area, " of land, and the total counts the share of each block group inside the map, ",
                       "so it is an estimate; the classes are the same for every place, so maps can be compared"),
      tiles = list(t_total, tile("s-through", paste0("about ", tot_txt), Noun, "2020 Census, inside the map"),
                   tile("s-local", paste0(fmt_n(round(mx)), " per ", if (identical(ulab(), "mi")) "mi" else "km", "²"), "Densest block group", paste0("of ", census$n_bg, " block groups")))))
  }

  segs <- segments_of(lines)
  hx <- hex_density(segs, radius_m, center_m, shape, across = if (isTRUE(fine)) relief_across else 11)
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

# Relief helpers. Heights are proportional to street density on one fixed scale, so two places can be compared, and are capped so a single
# extreme hexagon cannot flatten the rest.
relief_across <- 26         # hexagons across a radius when raised; the flat map uses 11, and the finer grid is what makes thin peaks
relief_ref_km <- 25         # km of street per square kilometer that reaches the reference height
relief_cap <- 1.2           # heights stop growing at this multiple of the reference
relief_power <- 1.5         # heights grow faster than density, so moderate areas stay low and dense cores stand out
relief_height <- 0.3        # height at the reference density, as a share of the map radius
relief_dark <- " (shaded)"  # suffix for the darker right face of each spike
relief_out <- " (outside)"  # suffix for the faded colors of hexagons outside the city limit
shade_color <- function(col, f) { m <- grDevices::col2rgb(col)[, 1] * f; grDevices::rgb(m[1], m[2], m[3], maxColorValue = 255) }
mix_color <- function(col, with, a) { m <- (1 - a) * grDevices::col2rgb(col)[, 1] + a * grDevices::col2rgb(with)[, 1]; grDevices::rgb(m[1], m[2], m[3], maxColorValue = 255) }
# The Street lights map is a night scene in every style: the style's own night colors for the sky and ground, its light ink for the text, and one glow color.
night_palette <- function(pal, name) {
  n <- get_style(name)$night
  ink <- if (sum(grDevices::col2rgb(pal$bg)[, 1] * c(0.2126, 0.7152, 0.0722)) / 255 < 0.25) pal$ink else pal$bg
  modifyList(pal, list(bg = n$bg, plate = n$plate, ink = ink, muted = mix_color(n$plate, ink, 0.75), faint = mix_color(n$plate, ink, 0.58),
                       ring = mix_color(n$plate, ink, 0.2), glow = n$glow))
}
relief_spikes <- function(cells, hex_w, radius_m) {
  ref <- relief_ref_km * uf()                                                # the reference density in the unit on screen (cells$dens is already in that unit)
  cen <- suppressWarnings(st_coordinates(st_centroid(st_geometry(cells))))
  cx <- cen[, 1]; cy <- cen[, 2]
  h <- pmin(cells$dens / ref, relief_cap)^relief_power * relief_height * radius_m
  w <- 0.4 * hex_w; dp <- 0.5 * w                                             # half the width of a spike's base, and how far its front corner sits below the center
  ord <- order(cy, decreasing = TRUE)                                         # northern spikes first, so spikes further south cover them
  polys <- do.call(rbind, lapply(seq_along(ord), function(k) {
    i <- ord[k]; cl <- as.character(cells$cls[i])
    data.frame(x = c(cx[i], cx[i] - w, cx[i], cx[i], cx[i], cx[i] + w), y = c(cy[i] + h[i], cy[i], cy[i] - dp, cy[i] + h[i], cy[i] - dp, cy[i]),
               id = rep(c(2 * k - 1, 2 * k), each = 3), face = rep(c(cl, paste0(cl, relief_dark)), each = 3))
  }))
  list(polys = polys, cap = relief_cap * ref)
}
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
make_map <- function(lines, place, lon, lat, radius_m, crs_m, label_roads = TRUE, style = "classic", view = "hierarchy", show_local = TRUE, shape = NULL, inside = NULL, detail = "all", center_txt = NULL, units = "km", paths = NULL, note = NULL, relief = FALSE, lights = NULL, nature = NULL, census = NULL) {
  old_u <- unit_env$u; unit_env$u <- as_unit(units); on.exit(unit_env$u <- old_u, add = TRUE)
  pal <- get_style(style); view <- get_view(view)
  if (view == "lights") pal <- night_palette(pal, style)
  center_m <- center_of(lon, lat, crs_m)
  circle <- if (is.null(shape)) st_buffer(center_m, radius_m) else shape
  # Relief: the street-density hexagons are raised into spikes that stand up from the same flat map.
  rel <- identical(view, "density") && isTRUE(relief)
  vd <- view_data(view, lines, radius_m, center_m, shape, detail, center_txt, unit_env$u, paths = paths, fine = rel, lights = lights, nature = nature, census = census)
  hide_local <- view == "hierarchy" && !isTRUE(show_local)   # only the drawing changes; the numbers and legend totals still count every street
  drawn <- if (hide_local) filter(lines, role != roles$role[5]) else lines
  legend_theme <- theme(legend.position = "bottom", legend.text = element_text(size = 11, color = pal$muted, margin = margin(l = 3, r = 6)),
                        legend.title = element_text(size = 11, color = pal$muted),
                        legend.key.width = unit(1.0, "cm"), legend.key.height = unit(0.5, "cm"), legend.margin = margin(18, 0, 0, 0), legend.box.margin = margin(0, 0, 0, 0))
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
        if (dimmed && !rel) geom_sf(data = outside, fill = pal$plate, alpha = if (view %in% c("direction", "density")) 0.84 else 0.74, color = NA),
        geom_sf(data = circle, fill = NA, color = pal$ring, linewidth = 0.6),
        geom_sf(data = inside_c, fill = NA, color = pal$muted, linewidth = 0.45, linetype = "22"))
    }
  }
  # When the map is centered on an address, a small ring marks it.
  marker <- if (!is.null(center_txt)) geom_sf(data = center_m, shape = 21, size = 3.4, stroke = 1.2, fill = pal$plate, color = pal$ink)
  frame <- list(if (!rel) veil, marker, map_furniture(center_m, radius_m, pal), coord_sf(crs = crs_m, datum = NA, expand = FALSE), theme_void(base_family = body_font), legend_theme)
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
  } else if (view == "lights") {
    glow <- pal$glow; core <- mix_color(glow, "#ffffff", 0.4); dim <- mix_color(pal$plate, pal$ink, 0.24)
    lv <- lights_levels; lit <- lines_lit(lines)
    leg <- setNames(c(glue("Lit street  \u00b7  {round(uv(vd$km_by[[1]]), 1)} {ulab()}"), glue("Lit path  \u00b7  {round(uv(vd$km_by[[2]]), 1)} {ulab()}"),
                      glue("Not tagged lit  \u00b7  {round(uv(vd$km_by[[3]]), 1)} {ulab()}"), glue("Street lamps  \u00b7  {format(vd$n_lamps, big.mark = ',')}")), lv)
    mk <- function(keep, cls) { if (!any(keep)) return(NULL); g <- st_geometry(lines)[keep]; st_sf(cls = factor(rep(cls, length(g)), levels = lv), geometry = g) }
    shown <- lv[c(TRUE, !is.null(lights$ways) && nrow(lights$ways) > 0, TRUE, TRUE)]                    # no legend entry for lit paths when none are mapped
    unlit <- mk(!lit, lv[3]); lit_s <- mk(lit, lv[1])
    ways <- if (!is.null(lights$ways) && nrow(lights$ways) > 0) st_sf(cls = factor(rep(lv[2], nrow(lights$ways)), levels = lv), geometry = st_geometry(lights$ways)) else NULL
    glowing <- do.call(rbind, Filter(Negate(is.null), list(lit_s, ways)))
    lamps <- if (!is.null(lights$lamps) && nrow(lights$lamps) > 0) st_sf(cls = factor(rep(lv[4], nrow(lights$lamps)), levels = lv), geometry = st_geometry(lights$lamps)) else NULL
    map_plot <- base +
      (if (!is.null(unlit)) geom_sf(data = unlit, aes(color = cls, linewidth = cls), lineend = "round") else NULL) +
      (if (!is.null(glowing)) list(geom_sf(data = glowing, color = glow, linewidth = 3.2, alpha = 0.06, lineend = "round"),
                                   geom_sf(data = glowing, color = glow, linewidth = 1.7, alpha = 0.14, lineend = "round"),
                                   geom_sf(data = glowing, aes(color = cls, linewidth = cls), lineend = "round")) else NULL) +
      (if (!is.null(lamps)) list(geom_sf(data = lamps, color = glow, size = 3.4, alpha = 0.10, shape = 16),
                                 geom_sf(data = lamps, color = glow, size = 2.0, alpha = 0.28, shape = 16),
                                 geom_sf(data = lamps, aes(color = cls), size = 0.9, shape = 16)) else NULL) +
      scale_color_manual(values = setNames(c(glow, mix_color(glow, dim, 0.35), dim, core), lv), labels = leg, name = NULL, limits = shown, drop = FALSE) +
      scale_linewidth_manual(values = setNames(c(0.7, 0.5, 0.3, 0), lv), labels = leg, name = NULL, limits = shown, drop = FALSE) +
      guides(color = guide_legend(ncol = 2, byrow = TRUE, override.aes = list(shape = ifelse(shown == lv[4], 16, NA), size = 2.2))) + frame
  } else if (view == "nature") {
    nt <- pal$nature; lv <- nature_levels
    leg <- setNames(glue("{lv}  \u00b7  {ifelse(vd$pct < 1 & vd$pct > 0, '<1', round(vd$pct))}%"), lv)
    areas <- list(nature$park, nature$wood, nature$water)                                         # parks first, then woods, then water on top
    area_layers <- Map(function(g, l) if (is.null(g)) NULL else geom_sf(data = st_sf(cls = factor(l, levels = lv), geometry = st_sfc(g[[1]], crs = st_crs(circle))), aes(fill = cls), color = NA),
                       areas, lv[c(3, 2, 1)])
    shore <- if (!is.null(nature$water)) geom_sf(data = st_sf(geometry = st_sfc(nature$water[[1]], crs = st_crs(circle))), fill = NA, color = nt$river, linewidth = 0.25, alpha = 0.7) else NULL
    ways <- if (!is.null(nature$ways) && nrow(nature$ways) > 0) lapply(c("stream", "river"), function(k) {
      d <- nature$ways[nature$ways$kind == k, ]; if (nrow(d) == 0) NULL else geom_sf(data = d, color = nt$river, linewidth = if (k == "river") 0.9 else 0.35, lineend = "round") }) else NULL
    rl <- roles$role; lw <- roles$lw
    street_layers <- lapply(seq_along(rl), function(i) {
      d <- filter(lines, role == rl[i]); if (nrow(d) == 0) return(NULL)
      list(if (i <= 3) geom_sf(data = d, color = pal$plate, linewidth = lw[i] * 0.4 + 0.4, alpha = 0.75, lineend = "round") else NULL,
           geom_sf(data = d, color = mix_color(nt$street, pal$plate, 0.15), linewidth = lw[i] * 0.4, lineend = "round"))
    })
    present <- lv[vd$pct > 0]
    cxy <- as.numeric(st_coordinates(center_m))
    keys <- st_sf(cls = factor(lv, levels = lv), geometry = st_sfc(rep(list(st_polygon(list(cbind(cxy[1] + c(0, 1, 1, 0, 0), cxy[2] + c(0, 0, 1, 1, 0))))), length(lv)), crs = st_crs(center_m)))
    map_plot <- base + area_layers + shore + ways + rev(street_layers) +
      geom_sf(data = keys, aes(fill = cls), alpha = 0, color = NA, show.legend = TRUE) +
      scale_fill_manual(values = setNames(c(nt$water, nt$wood, nt$park), lv), labels = leg, limits = if (length(present) > 0) present else lv, name = NULL, drop = FALSE) +
      guides(fill = guide_legend(nrow = 1, override.aes = list(colour = pal$ring, linewidth = 0.25, alpha = 1))) + frame
  } else if (view %in% c("census", "homes")) {
    rl <- roles$role; lw <- roles$lw
    street_layers <- lapply(seq_along(rl), function(i) {
      d <- filter(lines, role == rl[i]); if (nrow(d) == 0) return(NULL)
      geom_sf(data = d, color = pal$ink, alpha = 0.42, linewidth = lw[i] * 0.32, lineend = "round")
    })
    if (!isTRUE(vd$has_census)) {
      map_plot <- base + rev(street_layers) + frame
    } else {
      lv <- vd$levels
      fills <- setNames(c(mix_color(pal$ring, pal$plate, 0.45), pal$dens), lv)
      cxy <- as.numeric(st_coordinates(center_m))      # a class with no block groups gets no legend key from ggplot, so each class carries an invisible one-meter square
      keys <- st_sf(cls = factor(lv, levels = lv), geometry = st_sfc(rep(list(st_polygon(list(cbind(cxy[1] + c(0, 1, 1, 0, 0), cxy[2] + c(0, 0, 1, 1, 0))))), length(lv)), crs = st_crs(center_m)))
      map_plot <- base + geom_sf(data = vd$cells, aes(fill = cls), color = pal$plate, linewidth = 0.3) + rev(street_layers) +
        geom_sf(data = keys, aes(fill = cls), alpha = 0, color = NA, show.legend = TRUE) +
        scale_fill_manual(values = fills, limits = lv, name = paste0(vd$what, " per ", vd$unit_area), drop = FALSE) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top", title.hjust = 0.5,
                                   override.aes = list(colour = pal$ring, linewidth = 0.25, alpha = 1))) + frame +
        theme(legend.key.width = unit(0.9, "cm"), legend.key.height = unit(0.5, "cm"), legend.key.spacing.x = unit(10, "pt"),
              legend.margin = margin(18, 0, 10, 0), legend.title = element_text(size = 11, color = pal$muted, margin = margin(0, 0, 6, 0)),
              legend.text = element_text(size = 10.5, color = pal$muted, margin = margin(l = 4)))
    }
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
    if (rel) {
      # Hexagons whose center lies outside the city limit are faded as a whole, spike and ground together, as the flat map fades its outside.
      out <- if (is.null(inside)) rep(FALSE, nrow(cells)) else !apply(suppressWarnings(st_intersects(st_centroid(st_geometry(cells)), st_make_valid(inside), sparse = FALSE)), 1, any)
      cells$cls <- ifelse(out, paste0(as.character(cells$cls), relief_out), as.character(cells$cls))
      spikes <- relief_spikes(cells, vd$hex_w, radius_m)
      dark <- setNames(vapply(fills, function(cl) shade_color(cl, 0.78), ""), paste0(vd$levels, relief_dark))
      pale <- setNames(vapply(fills, function(cl) mix_color(cl, pal$plate, 0.8), ""), paste0(names(fills), relief_out))
      pale_dark <- setNames(vapply(pale, function(cl) shade_color(cl, 0.93), ""), paste0(names(fills), relief_out, relief_dark))
      layer <- list(geom_sf(data = cells, aes(fill = cls), color = pal$plate, linewidth = 0.2, alpha = 0.3),
                    veil,                                                      # the ring and the city limit sit under the spikes, so a spike covers the line where they meet
                    geom_polygon(data = spikes$polys, aes(x, y, group = id, fill = face), color = pal$plate, linewidth = 0.12, show.legend = FALSE))
      fill_scale <- scale_fill_manual(values = c(fills, dark, pale, pale_dark), breaks = vd$levels, name = paste0(ulab(), " of street per ", ulab(), "\u00b2"), drop = FALSE)
      vd$caption <- paste0(vd$caption, "; spikes rise faster than density, so dense areas stand out, up to ", round(spikes$cap), " ", ulab(), " per ", ulab(), "\u00b2, on the same scale for every place")
    } else {
      layer <- geom_sf(data = cells, aes(fill = cls), color = pal$plate, linewidth = 0.4)
      fill_scale <- scale_fill_manual(values = fills, limits = vd$levels, name = paste0(ulab(), " of street per ", ulab(), "\u00b2"), drop = FALSE)
    }
    map_plot <- base + layer +
      geom_sf(data = keys, aes(fill = cls), alpha = 0, color = NA, show.legend = TRUE) + fill_scale +
      guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom", title.hjust = 0.5,
                                 override.aes = list(colour = pal$ring, linewidth = 0.25, alpha = 1))) + frame +
      theme(legend.key.width = unit(1.7, "cm"), legend.key.height = unit(0.42, "cm"), legend.key.spacing.x = unit(0, "pt"),
            legend.margin = margin(18, 0, 16, 0), legend.title = element_text(size = 11, color = pal$muted, margin = margin(0, 0, 6, 0)),
            legend.text = element_text(size = 10, color = pal$muted, margin = margin(t = 4)))
  }

  caption <- paste0("Data \u00a9 OpenStreetMap contributors (ODbL), ", format(Sys.Date(), "%Y"), " \u00b7 ", vd$caption,
                    if (!is.null(shape)) "; city limits are the boundary mapped in OpenStreetMap" else "",
                    if (dimmed) paste0("; the dashed line is the city limit mapped in OpenStreetMap, and ", if (rel) "hexagons" else "streets", " outside it are dimmed")
                    else if (!is.null(veil)) "; the dashed line is the city limit mapped in OpenStreetMap" else "",
                    if (hide_local) paste0("; the ", round(uv(vd$stats$local_km)), " ", ulab(), " of local streets are not drawn") else "",
                    if (!is.null(center_txt)) paste0("; the small circle marks ", center_txt) else "",
                    if (!is.null(note)) paste0("; ", note) else "")
  composite <- if (is.null(extra)) map_plot else (map_plot / extra) + patchwork::plot_layout(heights = c(1, 0.4))
  composite + patchwork::plot_annotation(
    title = escape_markup(place), subtitle = vd$subtitle, caption = caption,
    theme = theme(
      plot.title = element_textbox_simple(width = text_box_w, hjust = 0.5, family = head_font, size = 38, color = pal$ink, margin = margin(0, 0, 22, 0)),
      plot.subtitle = element_textbox_simple(width = text_box_w, hjust = 0.5, family = body_font, size = 13, color = pal$muted, lineheight = 1.3, margin = margin(0, 0, 28, 0)),
      plot.caption = element_textbox_simple(width = text_box_w, hjust = 0.5, family = body_font, size = 8.5, color = pal$faint, lineheight = 1.3, margin = margin(if (view == "density") 26 else 16, 0, 0, 0)),
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
# The dpi a sheet of this size (inches) can have: what was asked for, held to the pixel cap.
export_dpi <- function(size_in, res = "300") {
  cap <- floor(sqrt(max_export_px / (size_in[1] * size_in[2])))
  res <- res_pick(res); want <- if (res == "max") cap else as.numeric(res)
  list(dpi = min(want, cap), want = want, cap = cap, capped = want > cap)
}
print_plan <- function(key = "8x10", format = "png", res = "300") {
  wh <- print_sizes[[if (isTRUE(key %in% names(print_sizes))) key else "8x10"]]
  layout <- c(8, 8 * wh[2] / wh[1])
  if (format %in% vector_formats) return(list(layout = layout, size_in = wh, dpi = NA_real_, px = NULL))
  e <- export_dpi(wh, res)
  list(layout = layout, size_in = wh, dpi = e$dpi, px = round(wh * e$dpi), want = e$want, capped = e$capped)
}
figure_bg <- function(p, default) {
  f <- tryCatch(p$patches$annotation$theme$plot.background$fill, error = function(e) NULL)
  if (is.character(f) && length(f) == 1 && !is.na(f)) f else default
}
draw_figure <- function(p, file, format = "png", style = "classic", preview = FALSE, size = fig_in, print_key = NULL, res = "300") {
  if (!is.character(format) || length(format) != 1 || !format %in% export_formats)
    stop("Unsupported file format.")
  pal <- get_style(style)
  pal$bg <- figure_bg(p, pal$bg)                               # the Street lights map is drawn on a night page, so the margins must match
  dpi <- if (preview) preview_dpi else print_dpi
  w <- size[1]; h <- size[2]
  if (!is.null(print_key) && !preview) {                       # a print size: same 8-inch layout, more pixels per inch
    pl <- print_plan(print_key, format, res); w <- pl$layout[1]; h <- pl$layout[2]
    if (!is.na(pl$dpi)) dpi <- pl$dpi * pl$size_in[1] / pl$layout[1]
  } else if (!preview) dpi <- export_dpi(c(w, h), res)$dpi       # no print size: the figure's own size, at the resolution asked for
  showtext_opts(dpi = if (format %in% vector_formats) vector_dpi else dpi)
  on.exit(showtext_opts(dpi = preview_dpi), add = TRUE)
  switch(format,
    png  = ragg::agg_png(file, width = w, height = h, units = "in", res = dpi, background = pal$bg),
    jpeg = ragg::agg_jpeg(file, width = w, height = h, units = "in", res = dpi, quality = 95, background = pal$bg),
    tiff = ragg::agg_tiff(file, width = w, height = h, units = "in", res = dpi, compression = "lzw", background = pal$bg),
    pdf  = grDevices::cairo_pdf(file, width = w, height = h, bg = pal$bg),
    svg  = grDevices::svg(file, width = w, height = h, bg = pal$bg),
    eps  = grDevices::cairo_ps(file, width = w, height = h, bg = pal$bg, onefile = FALSE))
  dev_id <- dev.cur()
  on.exit(if (dev_id %in% dev.list()) dev.off(dev_id), add = TRUE)   # never leave a device open if drawing fails
  print(p)
  dev.off(dev_id)
  invisible(file)
}

# The width of the title, subtitle and caption boxes: the figure's width less its side margins (34 points each). A box has to know its own width, because the
# map panel is narrower than the page for tall places and a box sized to the panel is measured at one width and drawn at another, which lets a title that wraps
# run into the subtitle below it.
text_box_w <- grid::unit(fig_in[1] - 2 * 34 / 72, "in")

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
watermark_svg <- r"---(<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><defs><clipPath id="wm"><circle cx="256" cy="256" r="250"/></clipPath></defs><g clip-path="url(#wm)"><defs><linearGradient id="wwat" gradientUnits="userSpaceOnUse" x1="0" y1="90" x2="512" y2="430"><stop offset="0" stop-color="#cddbdc"/><stop offset="1" stop-color="#bccfd3"/></linearGradient><clipPath id="wca"><polygon points="80.0,256.0 83.1,237.8 88.0,221.2 94.3,206.1 102.1,192.7 111.1,180.8 121.3,170.5 132.4,161.8 144.3,154.7 156.9,149.1 170.0,145.2 183.5,142.8 197.2,142.0 211.1,142.8 224.9,145.2 238.5,149.1 251.7,154.7 264.5,161.8 276.6,170.5 287.9,180.8 298.4,192.7 307.7,206.1 315.9,221.2 322.7,237.8 328.0,256.0 328.0,256.0 322.7,274.2 315.9,290.8 307.7,305.9 298.4,319.3 287.9,331.2 276.6,341.5 264.5,350.2 251.7,357.3 238.5,362.9 224.9,366.8 211.1,369.2 197.2,370.0 183.5,369.2 170.0,366.8 156.9,362.9 144.3,357.3 132.4,350.2 121.3,341.5 111.1,331.2 102.1,319.3 94.3,305.9 88.0,290.8 83.1,274.2 80.0,256.0"/></clipPath><clipPath id="wcd"><polygon points="314.0,256.0 318.7,255.5 323.1,254.2 327.4,252.0 331.6,249.1 335.8,245.5 340.0,241.3 344.2,236.6 348.6,231.4 353.2,225.9 358.0,220.0 363.1,213.9 368.6,207.6 374.5,201.2 380.8,194.8 387.7,188.5 395.2,182.3 403.3,176.2 412.1,170.5 421.6,165.0 432.0,160.0 445.2,154.0 458.8,147.4 472.9,140.3 487.6,132.7 502.8,124.6 518.6,116.1 535.2,107.2 552.4,98.0 570.5,88.4 589.4,78.5 609.2,68.4 630.0,58.0 640.0,100.0 640.0,412.0 630.0,454.0 609.2,443.6 589.4,433.5 570.5,423.6 552.4,414.0 535.2,404.8 518.6,395.9 502.8,387.4 487.6,379.3 472.9,371.7 458.8,364.6 445.2,358.0 432.0,352.0 421.6,347.0 412.1,341.5 403.3,335.8 395.2,329.7 387.7,323.5 380.8,317.2 374.5,310.8 368.6,304.4 363.1,298.1 358.0,292.0 353.2,286.1 348.6,280.6 344.2,275.4 340.0,270.7 335.8,266.5 331.6,262.9 327.4,260.0 323.1,257.8 318.7,256.5 314.0,256.0"/></clipPath></defs><rect width="512" height="512" fill="#efece1"/><g><path d="M-77 317L-35 311L-29 352L-70 359ZM-68 371L-27 364L-20 406L-62 412ZM-60 424L-18 418L-12 459L-53 466ZM-74 -11L-33 -18L-26 24L-67 30ZM-66 42L-24 36L-18 77L-59 84ZM-49 149L-7 142L-1 184L-42 190ZM-40 202L1 196L8 237L-34 244ZM-23 309L18 302L25 344L-17 350ZM-6 416L35 409L42 451L0 457ZM10 522L52 516L58 557L17 564ZM-4 87L38 81L44 122L3 129ZM22 247L63 241L70 282L28 289ZM47 407L88 401L95 442L53 449ZM55 461L97 454L103 495L62 502ZM33 -28L74 -34L81 7L39 14ZM75 239L116 232L123 274L81 280ZM92 345L133 339L140 380L98 387ZM117 505L159 499L165 540L124 547ZM86 -36L127 -43L134 -1L93 5ZM94 17L136 10L142 52L101 58ZM111 124L153 117L159 159L118 165ZM128 230L170 224L176 265L135 272ZM137 284L178 277L185 319L143 325ZM145 337L187 330L193 372L152 378ZM154 390L195 384L202 425L160 432ZM190 275L231 269L238 310L197 317ZM193 -53L234 -60L241 -18L199 -12ZM218 107L259 100L266 142L225 148ZM226 160L268 154L274 195L233 202ZM252 320L293 314L300 355L258 362ZM269 427L310 420L317 462L275 468ZM277 480L319 474L325 515L284 522ZM286 533L327 527L334 568L292 575ZM263 45L304 38L311 80L269 86ZM280 152L321 145L328 187L286 193ZM297 258L338 252L345 293L303 300ZM322 418L363 412L370 453L329 460ZM339 525L380 518L387 560L345 566ZM299 -70L341 -77L347 -35L306 -29ZM316 37L358 30L364 71L323 78ZM325 90L366 83L373 125L331 131ZM333 143L375 137L381 178L340 185ZM350 250L391 243L398 285L357 291ZM358 303L400 297L406 338L365 345ZM375 410L417 403L423 445L382 451ZM361 -25L403 -32L409 10L368 16ZM370 28L411 22L418 63L376 70ZM378 81L419 75L426 116L385 123ZM429 401L470 395L477 436L435 443ZM437 455L479 448L485 490L444 496ZM414 -34L456 -40L462 1L421 8ZM423 20L464 13L471 55L429 61ZM440 126L481 120L488 161L446 168ZM448 180L490 173L496 215L455 221ZM457 233L498 226L505 268L463 274ZM465 286L507 280L513 321L472 328ZM482 393L523 386L530 428L489 434ZM468 -42L509 -49L516 -7L474 -1ZM485 65L526 58L533 99L491 106ZM502 171L543 165L550 206L508 213ZM527 331L568 325L575 366L533 373ZM535 385L577 378L583 419L542 426ZM538 56L579 50L586 91L545 98Z" fill="#e4dfcd" stroke="#e4dfcd" stroke-width="5" stroke-linejoin="round"/><path d="M-51 477L-10 471L-3 512L-45 519ZM-43 531L-1 524L5 566L-36 572ZM2 469L43 462L50 504L9 511ZM-21 -19L21 -26L27 15L-14 22ZM5 141L46 134L53 175L11 182ZM13 194L55 187L61 229L20 235ZM30 301L71 294L78 335L37 342ZM38 354L80 347L86 389L45 395ZM41 25L83 19L89 60L48 67ZM58 132L99 126L106 167L65 174ZM120 177L161 170L168 212L126 218ZM162 444L203 437L210 479L169 485ZM170 497L212 490L218 532L177 538ZM148 9L189 2L196 43L154 50ZM182 222L223 215L230 257L188 263ZM224 489L265 482L272 523L230 530ZM232 542L274 535L280 577L239 583ZM210 53L251 47L258 88L216 95ZM246 -62L287 -68L294 -27L253 -20ZM271 98L313 92L319 133L278 140ZM288 205L330 198L336 240L295 246ZM308 -17L349 -23L356 18L314 25ZM342 197L383 190L390 231L348 238ZM386 135L428 128L434 170L393 176ZM395 188L436 182L443 223L401 230ZM403 241L445 235L451 276L410 283ZM412 295L453 288L460 330L418 336ZM446 508L487 502L494 543L452 550ZM474 340L515 333L522 375L480 381ZM490 446L532 440L539 481L497 488ZM499 500L540 493L547 535L505 541ZM476 11L518 5L524 46L483 53ZM493 118L535 111L541 153L500 159ZM510 225L551 218L558 259L517 266ZM518 278L560 271L566 313L525 319ZM521 -51L563 -57L569 -16L528 -9Z" fill="#e1dbc6" stroke="#e1dbc6" stroke-width="5" stroke-linejoin="round"/><path d="M-57 96L-16 89L-9 131L-51 137ZM-15 362L27 356L33 397L-8 404ZM-12 34L29 27L36 69L-6 75ZM64 514L105 507L112 549L70 555ZM50 79L91 72L98 114L56 120ZM66 185L108 179L114 220L73 227ZM83 292L125 286L131 327L90 334ZM109 452L150 446L157 487L115 494ZM139 -45L181 -51L187 -10L146 -3ZM156 62L198 55L204 97L163 103ZM165 115L206 109L213 150L171 157ZM198 329L240 322L246 363L205 370ZM215 435L257 429L263 470L222 477ZM201 0L243 -6L249 35L208 42ZM235 213L276 207L283 248L241 255ZM260 373L302 367L308 408L267 415ZM254 -8L296 -15L302 27L261 33ZM305 312L347 305L353 347L312 353ZM367 357L408 350L415 391L373 398ZM392 517L434 510L440 551L399 558Z" fill="#e8e4d4" stroke="#e8e4d4" stroke-width="5" stroke-linejoin="round"/><path d="M-32 256L10 249L16 291L-25 297ZM100 399L142 392L148 434L107 440ZM103 70L144 64L151 105L109 112ZM173 169L215 162L221 203L180 210ZM207 382L248 375L255 417L213 423ZM243 267L285 260L291 302L250 308ZM314 365L355 358L362 400L320 406ZM330 472L372 465L378 507L337 513ZM384 463L425 457L432 498L390 505ZM420 348L462 342L468 383L427 390ZM431 73L473 66L479 108L438 114ZM530 3L571 -4L578 38L536 44Z" fill="#dbe3cc" stroke="#dbe3cc" stroke-width="5" stroke-linejoin="round"/></g><g clip-path="url(#wca)"><polygon points="80.0,256.0 83.1,237.8 88.0,221.2 94.3,206.1 102.1,192.7 111.1,180.8 121.3,170.5 132.4,161.8 144.3,154.7 156.9,149.1 170.0,145.2 183.5,142.8 197.2,142.0 211.1,142.8 224.9,145.2 238.5,149.1 251.7,154.7 264.5,161.8 276.6,170.5 287.9,180.8 298.4,192.7 307.7,206.1 315.9,221.2 322.7,237.8 328.0,256.0 328.0,256.0 322.7,274.2 315.9,290.8 307.7,305.9 298.4,319.3 287.9,331.2 276.6,341.5 264.5,350.2 251.7,357.3 238.5,362.9 224.9,366.8 211.1,369.2 197.2,370.0 183.5,369.2 170.0,366.8 156.9,362.9 144.3,357.3 132.4,350.2 121.3,341.5 111.1,331.2 102.1,319.3 94.3,305.9 88.0,290.8 83.1,274.2 80.0,256.0" fill="#efece1"/><path d="M-17 -42L2 -32L-8 -12L-27 -22ZM-33 -11L-13 -1L-23 18L-43 8ZM13 -26L33 -16L23 3L3 -7ZM-18 34L2 44L-8 64L-28 54ZM13 50L32 60L22 79L3 69ZM58 35L78 45L68 64L48 54ZM-19 186L1 196L-9 216L-29 206ZM135 -41L155 -31L145 -11L125 -21ZM104 20L124 30L114 50L94 40ZM150 5L169 15L159 35L140 25ZM103 96L123 106L113 126L93 116ZM-35 369L-16 379L-26 398L-45 388ZM195 -10L215 0L205 20L186 10ZM87 202L107 212L97 232L77 222ZM26 324L45 333L35 353L16 343ZM-21 414L-1 424L-11 444L-31 434ZM241 -25L261 -15L251 5L231 -5ZM149 157L168 167L158 187L139 177ZM56 339L76 349L66 369L46 359ZM25 400L45 410L35 429L15 419ZM10 430L29 440L19 459L0 449ZM256 21L276 31L266 51L246 41ZM102 324L121 334L111 354L92 344ZM302 6L321 16L311 36L292 26ZM147 309L167 319L157 339L137 329ZM117 370L136 380L126 399L107 389ZM101 400L121 410L111 430L91 420ZM255 173L275 183L265 203L245 193ZM131 416L151 426L141 445L121 435ZM378 7L397 17L387 36L368 26ZM332 98L351 108L341 127L322 117ZM316 128L336 138L326 158L306 148ZM301 158L320 168L310 188L291 178ZM285 189L305 199L295 218L275 208ZM270 219L289 229L279 249L260 239ZM239 280L259 289L249 309L229 299ZM193 370L212 380L202 400L183 390ZM177 401L197 411L187 430L167 420ZM131 492L150 502L140 521L121 511ZM115 522L135 532L125 551L105 541ZM393 53L412 63L402 82L383 72ZM346 143L366 153L356 173L336 163ZM331 174L351 184L341 203L321 193ZM300 234L320 244L310 264L290 254ZM285 265L304 275L294 294L275 284ZM207 416L227 426L217 446L197 436ZM392 129L412 139L402 158L382 148ZM377 159L396 169L386 188L367 179ZM361 189L381 199L371 219L351 209ZM330 250L350 260L340 279L320 269ZM315 280L335 290L325 310L305 300ZM300 310L319 320L309 340L290 330ZM269 371L288 381L278 401L259 391ZM253 401L273 411L263 431L243 421ZM238 432L257 442L247 461L228 451ZM222 462L242 472L232 491L212 481ZM500 -7L519 3L509 22L490 12ZM469 53L488 63L478 83L459 73ZM453 83L473 93L463 113L443 103ZM268 447L288 457L278 477L258 467ZM253 477L272 487L262 507L243 497ZM391 281L411 291L401 310L381 300ZM329 402L349 412L339 431L319 421ZM529 84L549 94L539 114L519 104ZM483 175L503 185L493 205L473 195ZM468 205L487 215L477 235L458 225ZM421 296L441 306L431 326L411 316ZM344 448L364 458L354 477L334 467ZM329 478L348 488L338 507L319 497ZM529 160L548 170L538 190L519 180ZM374 463L394 473L384 493L364 483ZM405 478L424 488L414 508L395 498ZM512 342L532 352L522 372L502 362ZM420 524L439 534L429 554L410 544ZM526 464L546 474L536 494L516 484ZM496 525L515 535L505 554L486 544Z" fill="#e1dbc6" stroke="#e1dbc6" stroke-width="5" stroke-linejoin="round"/><path d="M-2 4L17 14L7 34L-12 24ZM28 19L48 29L38 49L18 39ZM11 202L31 212L21 231L1 221ZM-35 293L-15 303L-25 322L-45 312ZM119 66L138 76L129 95L109 85ZM88 126L108 136L98 156L78 146ZM57 263L76 273L66 293L47 283ZM179 97L199 107L189 126L169 116ZM118 218L137 228L127 247L108 237ZM-37 521L-17 531L-27 550L-47 540ZM86 354L106 364L96 384L76 374ZM40 445L60 455L50 475L30 465ZM132 340L152 350L142 369L122 359ZM332 22L352 32L342 51L322 41ZM301 82L321 92L311 112L291 102ZM162 355L182 365L172 385L152 375ZM147 385L166 395L156 415L137 405ZM146 461L166 471L156 491L136 481ZM192 446L212 456L202 476L182 466ZM177 477L196 487L186 506L167 496ZM423 68L443 78L433 98L413 88ZM314 432L333 442L323 462L304 452ZM452 236L472 245L462 265L442 255ZM437 266L456 276L446 295L427 285ZM436 342L456 352L446 371L426 361ZM405 402L425 412L415 432L395 422ZM344 524L363 534L353 553L334 543ZM482 327L501 337L491 357L472 347ZM435 494L455 504L445 523L425 513ZM481 479L500 489L490 509L471 499Z" fill="#dbe3cc" stroke="#dbe3cc" stroke-width="5" stroke-linejoin="round"/><path d="M-33 65L-14 75L-24 94L-43 84ZM-3 80L17 90L7 110L-13 100ZM-3 156L16 166L6 186L-13 176ZM58 111L77 121L67 140L48 130ZM-4 232L16 242L6 262L-14 252ZM73 157L92 167L82 186L63 176ZM149 81L169 91L159 111L139 101ZM10 354L30 364L20 383L0 373ZM133 187L153 197L143 217L123 207ZM-6 460L14 470L4 490L-16 480ZM241 51L260 61L250 81L231 71ZM210 112L229 122L219 142L200 132ZM286 37L306 47L296 66L276 56ZM225 158L244 168L234 187L215 177ZM178 249L198 259L188 278L168 268ZM39 521L59 531L49 551L29 541ZM317 52L336 62L326 82L307 72ZM362 37L382 47L372 67L352 57ZM362 113L381 123L371 143L352 133ZM269 295L289 305L279 325L259 315ZM238 356L258 366L248 385L228 375ZM223 386L243 396L233 415L213 405ZM438 38L458 48L448 67L428 57ZM530 8L549 18L540 38L520 28ZM437 190L457 200L447 219L427 209ZM298 462L318 472L308 492L288 482ZM267 523L287 533L277 553L257 543ZM390 357L410 367L400 386L380 376ZM513 190L533 200L523 220L503 210ZM390 433L409 443L399 462L380 452ZM497 373L516 383L506 402L487 392Z" fill="#e8e4d4" stroke="#e8e4d4" stroke-width="5" stroke-linejoin="round"/><path d="M59 -41L78 -31L68 -12L49 -22ZM43 -11L63 -1L53 19L33 9ZM-18 110L1 120L-9 140L-28 130ZM-34 141L-14 151L-24 170L-44 160ZM89 -26L109 -16L99 4L79 -6ZM74 5L93 15L83 34L64 24ZM43 65L62 75L52 95L33 85ZM27 95L47 105L37 125L17 115ZM12 126L32 136L22 155L2 145ZM-34 217L-15 227L-25 246L-44 236ZM119 -10L139 0L129 19L109 9ZM89 50L108 60L98 80L79 70ZM73 81L93 91L83 110L63 100ZM42 141L62 151L52 171L32 161ZM27 171L46 181L36 201L17 191ZM-19 262L0 272L-10 292L-29 282ZM165 -25L185 -15L175 4L155 -6ZM134 35L154 45L144 65L124 55ZM57 187L77 197L67 216L47 206ZM42 217L61 227L51 247L32 237ZM26 247L46 257L36 277L16 267ZM11 278L30 288L20 307L1 297ZM-5 308L15 318L5 338L-15 328ZM-20 338L0 348L-10 368L-30 358ZM211 -40L231 -30L221 -10L201 -20ZM180 21L200 31L190 50L170 40ZM165 51L184 61L174 80L155 70ZM134 111L153 121L143 141L124 131ZM118 142L138 152L128 171L108 161ZM103 172L122 182L112 202L93 192ZM72 233L92 243L82 262L62 252ZM41 293L61 303L51 323L31 313ZM-5 384L14 394L4 414L-15 404ZM-36 445L-16 455L-26 474L-46 464ZM226 6L245 16L235 35L216 25ZM210 36L230 46L220 66L200 56ZM195 66L215 76L205 96L185 86ZM164 127L184 137L174 156L154 146ZM102 248L122 258L112 278L92 268ZM87 278L106 288L96 308L77 298ZM71 309L91 319L81 338L61 328ZM41 369L60 379L50 399L31 389ZM-21 490L-2 500L-12 520L-31 510ZM287 -39L307 -29L297 -10L277 -20ZM272 -9L291 1L281 20L262 10ZM225 82L245 92L235 111L215 101ZM194 142L214 152L204 172L184 162ZM179 173L199 183L189 202L169 192ZM163 203L183 213L173 232L153 223ZM148 233L168 243L158 263L138 253ZM133 263L152 273L142 293L123 283ZM117 294L137 304L127 323L107 313ZM71 385L90 395L80 414L61 404ZM55 415L75 425L65 445L45 435ZM25 476L44 486L34 505L15 495ZM9 506L29 516L19 535L-1 525ZM317 -24L337 -14L327 6L307 -4ZM271 67L291 77L281 96L261 86ZM256 97L275 107L265 127L246 117ZM240 127L260 137L250 157L230 147ZM209 188L229 198L219 218L199 208ZM194 218L213 228L203 248L184 238ZM163 279L182 289L173 309L153 299ZM86 430L105 440L95 460L76 450ZM70 461L90 471L80 490L60 480ZM55 491L74 501L64 521L45 511ZM363 -39L383 -29L373 -9L353 -19ZM348 -9L367 1L357 21L338 11ZM286 113L305 123L295 142L276 132ZM270 143L290 153L280 172L260 162ZM239 203L259 213L249 233L230 223ZM224 234L244 244L234 263L214 253ZM209 264L228 274L218 294L199 284ZM193 294L213 304L203 324L183 314ZM178 325L197 335L187 354L168 344ZM116 446L136 456L126 475L106 465ZM101 476L120 486L110 506L91 496ZM85 506L105 516L95 536L75 526ZM393 -23L413 -13L403 6L383 -4ZM347 67L367 77L357 97L337 87ZM254 249L274 259L264 279L244 269ZM223 310L243 320L233 339L213 329ZM208 340L228 350L218 370L198 360ZM162 431L181 441L171 461L152 451ZM439 -38L459 -28L449 -9L429 -19ZM424 -8L443 2L433 22L414 12ZM408 22L428 32L418 52L398 42ZM377 83L397 93L387 112L367 102ZM316 204L335 214L325 234L306 224ZM254 325L273 335L263 355L244 345ZM161 507L181 517L171 537L151 527ZM469 -23L489 -13L479 7L459 -3ZM454 7L473 17L463 37L444 27ZM408 98L427 108L417 128L398 118ZM346 219L365 229L355 249L336 239ZM284 341L304 351L294 370L274 360ZM207 492L226 502L217 522L197 512ZM191 522L211 532L201 552L181 542ZM515 -38L535 -28L525 -8L505 -18ZM484 23L504 33L494 52L474 42ZM438 114L457 124L447 143L428 133ZM422 144L442 154L432 174L412 164ZM407 174L427 184L417 204L397 194ZM392 205L411 215L401 234L382 224ZM376 235L396 245L386 265L366 255ZM361 265L380 275L370 295L351 285ZM345 296L365 306L355 325L335 315ZM330 326L349 336L339 355L320 345ZM314 356L334 366L324 386L304 376ZM299 386L319 396L309 416L289 406ZM283 417L303 427L293 446L274 436ZM237 508L257 518L247 537L227 527ZM514 38L534 48L524 68L504 58ZM499 69L519 79L509 98L489 88ZM484 99L503 109L493 128L474 118ZM468 129L488 139L478 159L458 149ZM453 159L472 169L462 189L443 179ZM422 220L441 230L431 250L412 240ZM406 250L426 260L416 280L396 270ZM376 311L395 321L385 341L366 331ZM360 341L380 351L370 371L350 361ZM345 372L364 382L354 401L335 391ZM283 493L303 503L293 522L273 512ZM514 114L533 124L523 144L504 134ZM498 145L518 155L508 174L488 164ZM406 326L425 336L415 356L396 346ZM375 387L395 397L385 417L365 407ZM360 417L379 427L369 447L350 437ZM313 508L333 518L323 538L303 528ZM498 221L517 231L507 250L488 240ZM482 251L502 261L492 281L472 271ZM467 281L487 291L477 311L457 301ZM452 312L471 322L461 341L442 331ZM421 372L440 382L430 402L411 392ZM359 493L379 503L369 523L349 513ZM528 236L548 246L538 266L518 256ZM513 266L532 276L522 296L503 286ZM497 297L517 307L507 326L487 316ZM466 357L486 367L476 387L456 377ZM451 388L471 398L461 417L441 407ZM436 418L455 428L445 447L426 437ZM420 448L440 458L430 478L410 468ZM389 509L409 519L399 538L379 528ZM528 312L547 322L537 342L518 332ZM481 403L501 413L491 433L471 423ZM466 433L485 443L475 463L456 453ZM450 464L470 474L460 493L440 483ZM527 388L547 398L537 418L517 408ZM512 418L531 428L521 448L502 438ZM496 449L516 459L506 478L486 468ZM465 509L485 519L475 539L455 529ZM511 494L531 504L521 524L501 514Z" fill="#e4dfcd" stroke="#e4dfcd" stroke-width="5" stroke-linejoin="round"/></g><g clip-path="url(#wcd)"><polygon points="314.0,256.0 318.7,255.5 323.1,254.2 327.4,252.0 331.6,249.1 335.8,245.5 340.0,241.3 344.2,236.6 348.6,231.4 353.2,225.9 358.0,220.0 363.1,213.9 368.6,207.6 374.5,201.2 380.8,194.8 387.7,188.5 395.2,182.3 403.3,176.2 412.1,170.5 421.6,165.0 432.0,160.0 445.2,154.0 458.8,147.4 472.9,140.3 487.6,132.7 502.8,124.6 518.6,116.1 535.2,107.2 552.4,98.0 570.5,88.4 589.4,78.5 609.2,68.4 630.0,58.0 640.0,100.0 640.0,412.0 630.0,454.0 609.2,443.6 589.4,433.5 570.5,423.6 552.4,414.0 535.2,404.8 518.6,395.9 502.8,387.4 487.6,379.3 472.9,371.7 458.8,364.6 445.2,358.0 432.0,352.0 421.6,347.0 412.1,341.5 403.3,335.8 395.2,329.7 387.7,323.5 380.8,317.2 374.5,310.8 368.6,304.4 363.1,298.1 358.0,292.0 353.2,286.1 348.6,280.6 344.2,275.4 340.0,270.7 335.8,266.5 331.6,262.9 327.4,260.0 323.1,257.8 318.7,256.5 314.0,256.0" fill="#efece1"/><path d="M-51 267L-27 257L-16 280L-40 291ZM-35 302L-12 291L-1 315L-25 326ZM-4 371L19 361L30 384L6 395ZM11 406L35 395L45 419L22 430ZM26 441L50 430L61 454L37 464ZM42 475L66 465L76 489L52 499ZM57 510L81 500L92 523L68 534ZM-47 182L-23 172L-13 195L-36 206ZM-32 217L-8 206L3 230L-21 241ZM-16 252L8 241L18 265L-6 275ZM-1 286L23 276L34 300L10 310ZM15 321L39 311L49 334L25 345ZM46 391L69 380L80 404L56 414ZM61 425L85 415L96 438L72 449ZM-43 97L-19 87L-9 111L-33 121ZM-12 167L11 156L22 180L-2 191ZM19 236L42 226L53 249L29 260ZM34 271L58 260L68 284L45 295ZM50 306L73 295L84 319L60 329ZM65 340L89 330L99 354L76 364ZM96 410L120 399L130 423L106 434ZM142 514L166 503L177 527L153 538ZM-55 -22L-31 -33L-21 -9L-44 2ZM-39 12L-16 2L-5 26L-29 36ZM-24 47L0 37L10 60L-13 71ZM-9 82L15 71L26 95L2 106ZM22 151L46 141L57 165L33 175ZM38 186L62 175L72 199L48 210ZM69 255L93 245L103 269L79 279ZM84 290L108 280L119 303L95 314ZM131 394L154 384L165 408L141 418ZM162 464L185 453L196 477L172 488ZM-20 -38L4 -48L14 -25L-10 -14ZM11 32L35 21L45 45L21 55ZM26 66L50 56L61 80L37 90ZM57 136L81 125L91 149L68 160ZM73 171L96 160L107 184L83 194ZM88 205L112 195L122 218L99 229ZM165 379L189 368L200 392L176 403ZM227 518L251 507L261 531L238 541ZM61 51L85 40L95 64L72 75ZM92 120L116 110L126 134L102 144ZM169 294L193 283L203 307L180 318ZM185 329L208 318L219 342L195 352ZM215 398L239 388L250 411L226 422ZM231 433L255 422L265 446L242 457ZM246 468L270 457L281 481L257 491ZM262 502L286 492L296 515L272 526ZM277 537L301 526L312 550L288 561ZM80 1L104 -10L115 14L91 25ZM96 36L119 25L130 49L106 59ZM111 70L135 60L145 83L122 94ZM142 140L166 129L176 153L153 163ZM157 174L181 164L192 188L168 198ZM173 209L197 199L207 222L183 233ZM235 348L259 337L269 361L245 372ZM250 383L274 372L285 396L261 406ZM266 417L289 407L300 431L276 441ZM312 522L336 511L346 535L323 545ZM115 -15L139 -25L149 -1L125 9ZM130 20L154 10L165 33L141 44ZM146 55L170 44L180 68L156 79ZM161 90L185 79L196 103L172 113ZM192 159L216 148L227 172L203 183ZM208 194L231 183L242 207L218 217ZM223 228L247 218L257 242L234 252ZM239 263L262 253L273 276L249 287ZM300 402L324 391L335 415L311 426ZM347 506L370 496L381 519L357 530ZM196 74L220 63L230 87L207 98ZM211 109L235 98L246 122L222 133ZM227 143L251 133L261 157L237 167ZM258 213L282 202L292 226L268 237ZM273 248L297 237L308 261L284 271ZM289 282L312 272L323 296L299 306ZM304 317L328 306L339 330L315 341ZM320 352L343 341L354 365L330 376ZM335 386L359 376L369 400L346 410ZM351 421L374 411L385 434L361 445ZM366 456L390 445L400 469L377 480ZM381 491L405 480L416 504L392 514ZM397 525L421 515L431 539L407 549ZM200 -11L224 -21L234 2L210 13ZM246 93L270 83L280 106L257 117ZM262 128L285 117L296 141L272 152ZM277 163L301 152L311 176L288 186ZM293 197L316 187L327 211L303 221ZM308 232L332 222L342 245L319 256ZM354 336L378 326L389 349L365 360ZM370 371L394 360L404 384L380 395ZM385 406L409 395L420 419L396 430ZM401 440L424 430L435 454L411 464ZM416 475L440 465L450 488L427 499ZM432 510L455 499L466 523L442 534ZM250 8L274 -2L284 22L261 32ZM265 43L289 33L300 56L276 67ZM296 113L320 102L331 126L307 136ZM312 147L336 137L346 160L322 171ZM327 182L351 171L362 195L338 206ZM343 217L366 206L377 230L353 240ZM358 251L382 241L392 265L369 275ZM420 390L444 380L454 403L431 414ZM435 425L459 414L470 438L446 449ZM451 460L475 449L485 473L461 483ZM482 529L506 519L516 542L492 553ZM285 -7L308 -18L319 6L295 17ZM300 28L324 17L334 41L311 51ZM408 271L432 260L443 284L419 294ZM424 305L448 295L458 319L434 329ZM439 340L463 330L474 353L450 364ZM455 375L478 364L489 388L465 399ZM470 410L494 399L504 423L481 433ZM486 444L509 434L520 457L496 468ZM501 479L525 468L535 492L512 503ZM335 12L359 2L369 25L345 36ZM397 151L420 141L431 164L407 175ZM412 186L436 175L446 199L423 210ZM428 221L451 210L462 234L438 244ZM443 255L467 245L477 268L454 279ZM458 290L482 279L493 303L469 314ZM520 429L544 418L555 442L531 453ZM385 31L409 21L419 45L396 55ZM462 205L486 194L497 218L473 229ZM478 240L502 229L512 253L488 264ZM509 309L532 299L543 322L519 333ZM420 16L443 5L454 29L430 40ZM435 51L459 40L470 64L446 75ZM482 155L505 144L516 168L492 179ZM497 190L521 179L531 203L508 213ZM512 224L536 214L547 238L523 248ZM454 1L478 -10L489 14L465 24ZM516 139L540 129L551 153L527 163ZM532 174L555 164L566 187L542 198ZM489 -15L513 -25L523 -2L500 9ZM505 20L528 9L539 33L515 44Z" fill="#e4dfcd" stroke="#e4dfcd" stroke-width="5" stroke-linejoin="round"/><path d="M-20 337L4 326L14 350L-9 360ZM77 460L100 449L111 473L87 484ZM108 529L131 519L142 543L118 553ZM111 445L135 434L146 458L122 468ZM127 479L151 469L161 492L137 503ZM7 117L31 106L41 130L18 140ZM53 221L77 210L88 234L64 245ZM100 325L123 314L134 338L110 349ZM115 360L139 349L149 373L126 383ZM146 429L170 418L180 442L157 453ZM192 533L216 523L227 546L203 557ZM42 101L65 91L76 114L52 125ZM103 240L127 229L138 253L114 264ZM119 275L143 264L153 288L130 298ZM134 309L158 299L169 323L145 333ZM181 414L205 403L215 427L191 437ZM196 448L220 438L231 461L207 472ZM212 483L235 472L246 496L222 507ZM76 86L100 75L111 99L87 109ZM107 155L131 145L142 168L118 179ZM123 190L147 179L157 203L133 214ZM200 363L224 353L234 377L211 387ZM65 -34L88 -44L99 -21L75 -10ZM177 124L200 114L211 137L187 148ZM254 298L278 287L288 311L265 322ZM285 367L309 357L319 380L295 391ZM316 437L340 426L350 450L326 460ZM331 471L355 461L366 485L342 495ZM150 -30L173 -41L184 -17L160 -6ZM181 39L204 29L215 53L191 63ZM242 178L266 168L277 191L253 202ZM231 59L254 48L265 72L241 82ZM405 356L428 345L439 369L415 379ZM466 494L490 484L501 508L477 518ZM269 -42L293 -52L304 -29L280 -18ZM393 236L417 225L427 249L403 260ZM517 514L540 503L551 527L527 537ZM474 325L498 314L508 338L485 348ZM354 -38L378 -49L388 -25L365 -14ZM370 -3L393 -14L404 10L380 21ZM400 66L424 56L435 79L411 90ZM431 136L455 125L466 149L442 159ZM447 170L471 160L481 184L457 194ZM493 274L517 264L528 288L504 298ZM466 120L490 110L500 133L477 144ZM501 105L525 94L535 118L511 128ZM524 -30L548 -41L558 -17L534 -7Z" fill="#e1dbc6" stroke="#e1dbc6" stroke-width="5" stroke-linejoin="round"/><path d="M30 356L54 345L65 369L41 380ZM3 202L27 191L37 215L14 225ZM80 375L104 365L115 388L91 399ZM177 498L201 488L211 512L188 522ZM150 344L174 334L184 357L160 368ZM30 -18L54 -29L64 -5L41 5ZM154 259L177 249L188 272L164 283ZM127 105L150 94L161 118L137 129ZM188 244L212 233L223 257L199 268ZM204 279L228 268L238 292L214 302ZM219 313L243 303L254 326L230 337ZM281 452L305 442L315 465L292 476ZM362 541L386 530L397 554L373 565ZM165 5L189 -6L199 18L176 28ZM215 24L239 13L250 37L226 48ZM323 267L347 256L358 280L334 291ZM235 -26L258 -37L269 -13L245 -3ZM389 321L413 310L423 334L400 345ZM316 62L339 52L350 76L326 86ZM346 132L370 121L381 145L357 156ZM362 167L386 156L396 180L373 190ZM377 201L401 191L412 214L388 225ZM489 359L513 349L524 373L500 383ZM505 394L529 384L539 407L515 418ZM416 101L440 90L450 114L427 125ZM524 344L548 333L558 357L535 368ZM528 259L552 248L562 272L538 283ZM439 -34L463 -45L473 -21L450 -10ZM470 35L494 25L504 48L480 59Z" fill="#e8e4d4" stroke="#e8e4d4" stroke-width="5" stroke-linejoin="round"/><path d="M92 495L116 484L126 508L103 518ZM-28 132L-4 122L7 145L-17 156ZM-5 -3L19 -14L30 10L6 21ZM45 16L69 6L80 29L56 40ZM138 225L162 214L173 238L149 248ZM297 487L320 476L331 500L307 511ZM269 333L293 322L304 346L280 356ZM339 302L363 291L373 315L349 325ZM281 78L305 67L315 91L291 102ZM374 286L397 276L408 299L384 310ZM331 97L355 87L365 110L342 121ZM319 -22L343 -33L354 -9L330 1ZM350 47L374 36L385 60L361 71ZM366 82L390 71L400 95L376 105ZM381 116L405 106L416 130L392 140ZM404 -19L428 -29L439 -5L415 5ZM451 85L474 75L485 99L461 109ZM485 70L509 59L520 83L496 94ZM520 55L544 44L554 68L531 78Z" fill="#dbe3cc" stroke="#dbe3cc" stroke-width="5" stroke-linejoin="round"/></g><path d="M-60 256 C0 256 20 256 66 256" stroke="#cdd9d6" stroke-width="78" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 104 292 104 328 256" stroke="#cdd9d6" stroke-width="60" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 408 292 408 328 256" stroke="#cdd9d6" stroke-width="60" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 192 432 160" stroke="#cdd9d6" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 320 432 352" stroke="#cdd9d6" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 160 C484 137 545 100 630 58" stroke="#cdd9d6" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 352 C484 375 545 412 630 454" stroke="#cdd9d6" stroke-width="56" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M-60 256 C0 256 20 256 66 256" stroke="url(#wwat)" stroke-width="66" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 104 292 104 328 256" stroke="url(#wwat)" stroke-width="48" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M80 256 C98 408 292 408 328 256" stroke="url(#wwat)" stroke-width="48" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 192 432 160" stroke="url(#wwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M314 256 C346 256 360 320 432 352" stroke="url(#wwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 160 C484 137 545 100 630 58" stroke="url(#wwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M432 352 C484 375 545 412 630 454" stroke="url(#wwat)" stroke-width="44" fill="none" stroke-linecap="round" stroke-linejoin="round"/><path d="M-60 256 C0 256 20 256 66 256" transform="translate(0 -14.5)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M-60 256 C0 256 20 256 66 256" transform="translate(0 14.5)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><path d="M80 256 C98 104 292 104 328 256" transform="translate(0 -10.6)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M80 256 C98 104 292 104 328 256" transform="translate(0 10.6)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><path d="M80 256 C98 408 292 408 328 256" transform="translate(0 -10.6)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M80 256 C98 408 292 408 328 256" transform="translate(0 10.6)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><path d="M314 256 C346 256 360 192 432 160" transform="translate(0 -9.7)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M314 256 C346 256 360 192 432 160" transform="translate(0 9.7)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><path d="M314 256 C346 256 360 320 432 352" transform="translate(0 -9.7)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="16 30" opacity="0.6"/><path d="M314 256 C346 256 360 320 432 352" transform="translate(0 9.7)" stroke="#f6f9f7" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="10 34" opacity="0.6"/><g transform="translate(118.8 172.8) rotate(-43.5)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#f6efd9"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/></g><g transform="translate(273.7 168.3) rotate(37.4)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#f6efd9"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/></g><g transform="translate(118.8 339.2) rotate(43.5)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#f6efd9"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/></g><g transform="translate(273.7 343.7) rotate(-37.4)"><rect x="-8.5" y="-45.0" width="17" height="90" fill="#f6efd9"/><rect x="-9.5" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/><rect x="6.1" y="-29.0" width="3.4" height="58" fill="#c9bd9a"/></g><g transform="translate(380.8 194.8) rotate(-44.0)"><rect x="-8.5" y="-42.0" width="17" height="84" fill="#f6efd9"/><rect x="-9.5" y="-27.0" width="3.4" height="54" fill="#c9bd9a"/><rect x="6.1" y="-27.0" width="3.4" height="54" fill="#c9bd9a"/></g><g transform="translate(380.8 317.2) rotate(44.0)"><rect x="-8.5" y="-42.0" width="17" height="84" fill="#f6efd9"/><rect x="-9.5" y="-27.0" width="3.4" height="54" fill="#c9bd9a"/><rect x="6.1" y="-27.0" width="3.4" height="54" fill="#c9bd9a"/></g><g transform="translate(327.0 256.0) rotate(90.0)"><rect x="-8.5" y="-47.0" width="17" height="94" fill="#f6efd9"/><rect x="-9.5" y="-33.0" width="3.4" height="66" fill="#c9bd9a"/><rect x="6.1" y="-33.0" width="3.4" height="66" fill="#c9bd9a"/></g><circle cx="136.9" cy="155.6" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="136.9" y="161.1" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">a</text><circle cx="293.6" cy="183.4" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="293.6" y="188.9" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">b</text><circle cx="136.9" cy="356.4" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="136.9" y="361.9" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">c</text><circle cx="293.6" cy="328.6" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="293.6" y="334.1" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">d</text><circle cx="398.8" cy="177.5" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="398.8" y="183.0" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">e</text><circle cx="398.8" cy="334.5" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="398.8" y="340.0" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">f</text><circle cx="327.0" cy="230.0" r="13" fill="#f6efd9" stroke="#c9bd9a" stroke-width="2.5"/><text x="327.0" y="235.5" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="#a89c7c">g</text><text x="206" y="268" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="46" fill="#bdb393" opacity=".9">A</text><text x="190" y="66" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="46" fill="#bdb393" opacity=".9">B</text><text x="190" y="462" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="46" fill="#bdb393" opacity=".9">C</text><text x="468" y="262" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="46" fill="#bdb393" opacity=".9">D</text></g></svg>)---"
app_css <- "
:root { --line: #e2ddcf; --stage: #e9e5d9; --paper: #fbfaf6; --surface: #fff; --field: #fffdf8; --field-line: #a79f88; --on-primary: #fff; --hover-wash: rgba(255,255,255,.6); --head: 'Oswald', system-ui, sans-serif; }
.bslib-page-title { background: var(--paper); border-bottom: 1px solid var(--line); box-shadow: none; }
.brand { display: flex; align-items: center; gap: .75rem; width: 100%; }
.brand svg { width: 34px; height: 34px; flex: none; }
.brand svg.brand-dark { display: none; }
html[data-bs-theme='dark'] .brand svg.brand-light { display: none; }
html[data-bs-theme='dark'] .brand svg.brand-dark { display: block; }
.cmp-res { margin-right: auto; } .cmp-res .form-group, .cmp-res .shiny-input-container { margin: 0; } .cmp-res label { font-size: .8rem; margin-bottom: .1rem; }
.history-map img { border-radius: .5rem; border: 1px solid var(--line); width: auto !important; max-width: 100%; display: block; margin: 0 auto; }
.history-map .fine { margin-bottom: .6rem; }
.expand-btn { white-space: nowrap; font-size: .85rem; padding: .3rem .7rem; }
.expand-btn .when-expanded, .is-expanded .expand-btn .when-normal { display: none; }
.is-expanded .expand-btn .when-expanded { display: inline; }
/* Expanded: the header shrinks to the Exit button, floating at the top right, and the map takes the space the header and padding used. */
.is-expanded .navbar { position: fixed; top: 0; right: 0; left: auto; width: auto; background: transparent; border: 0; padding: .45rem .6rem; z-index: 1040; }
html.is-expanded body, html.is-expanded .tab-content, html.is-expanded .tab-pane { background: var(--stage); }
.is-expanded .navbar { background: transparent !important; border: 0 !important; min-height: 0 !important; box-shadow: none !important; }
.is-expanded .navbar .container-fluid { padding: 0; }
.is-expanded .navbar .navbar-brand, .is-expanded .navbar .nav-item:not(.tools-item), .is-expanded .cvd-pick { display: none; }
.is-expanded .brand { width: auto; }
.is-expanded .expand-btn, .is-expanded .expand-btn:hover, .is-expanded .expand-btn:focus, .is-expanded .expand-btn:active { background: var(--paper); color: var(--bs-body-color); border-color: var(--bs-secondary-color); }
.is-expanded .bslib-sidebar-layout > .main { padding: .5rem !important; }
.is-expanded .stage { padding-top: 0; }
.is-expanded .history-page .bslib-sidebar-layout { height: 100vh !important; max-height: none; }
.is-expanded .history-map:not(:fullscreen) { min-height: calc(100vh - 1rem); }
.is-expanded .stat-row { padding-top: 2.4rem; }
/* Header: brand, page tabs, then the color vision menu and the Expand button, all centered on one line. Expand only makes sense on the current maps page. */
:root { --nav-h: 81px; }
.navbar { background: var(--paper) !important; border-bottom: 1px solid var(--line); padding-top: 0; padding-bottom: 0; min-height: var(--nav-h); }
.navbar > .container-fluid { align-items: center; }
.navbar .navbar-brand { display: flex; align-items: center; margin: 0; padding: 0; }
.brand-name, .brand-tag { line-height: 1; } .brand-tag { display: flex; align-items: center; height: 2rem; }
.navbar .navbar-nav { align-items: center; margin-left: 2.5rem; gap: .2rem; }
.navbar .navbar-nav .nav-item { display: flex; align-items: center; height: 44px; } .navbar .navbar-nav .nav-link { height: 44px; padding-top: 0; padding-bottom: 8px; align-items: center; font-size: 1rem; }   /* the label sits as high as the brand name; the underline takes the space below */
.navbar .nav-link { min-height: 44px; display: flex; align-items: center; padding: 0 1rem; font-weight: 500; box-shadow: inset 0 -3px 0 transparent; border-bottom: 0; }
.navbar .nav-link.active { box-shadow: inset 0 -3px 0 var(--bs-primary); }
/* The tabs on the left breathe, and the right end (Expand, Settings, Help, Light or Dark) is one row of identical links. */
.navbar .navbar-nav { gap: .35rem; } .navbar .navbar-nav .nav-link { padding-left: 1.2rem; padding-right: 1.2rem; }
.navbar .nav-link svg, .navbar .nav-link .fa, .navbar .nav-link i { margin-right: .45rem; }
.navbar .nav-btn { background: none; border: 0; color: var(--bs-nav-link-color, inherit); cursor: pointer; border-radius: 0; }
.navbar .nav-btn:hover { color: var(--bs-nav-link-hover-color, inherit); }
.navbar .nav-btn:focus-visible { outline: 2px solid var(--bs-primary); outline-offset: -2px; }
.navbar .navbar-nav .nav-item.tools-item { margin-left: auto; }
.cvd-pick { display: flex; align-items: center; gap: .4rem; } .cvd-pick .form-group, .cvd-pick .shiny-input-container { margin: 0; display: flex; align-items: center; gap: .4rem; width: auto !important; }
.cvd-pick label { margin: 0; font-size: .85rem; white-space: nowrap; } .cvd-pick select { min-height: 44px; font-size: .9rem; padding-top: .2rem; padding-bottom: .2rem; width: auto; min-width: 12rem; }
html:not([data-page='current']):not([data-page='history']) .expand-btn { display: none; }
.expand-btn { min-height: 44px; }
/* Home: a very light street map sits behind the page and fades out toward the bottom. */
.home { position: relative; isolation: isolate; min-height: calc(100vh - var(--nav-h)); overflow: hidden; }
.home::before { content: ''; position: absolute; inset: 0; z-index: -1; background: url(home-map.svg) center top / cover no-repeat; opacity: .6;
  -webkit-mask-image: linear-gradient(to bottom, #000 0%, #000 60%, transparent 100%); mask-image: linear-gradient(to bottom, #000 0%, #000 60%, transparent 100%); }
.home-inner { max-width: 1040px; margin: 0 auto; padding: 3rem 1.25rem 3rem; }
.home-hero h1 { font-family: var(--head); font-size: clamp(2.2rem, 5.5vw, 3.4rem); margin-bottom: .6rem; } .home-hero p { font-size: 1.15rem; max-width: 44rem; }
.home-cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 1.25rem; margin: 2rem 0 1.5rem; }
.home-card { background: var(--home-card, rgba(255,255,255,.88)); border: 1px solid var(--line); border-radius: 16px; padding: 1.5rem 1.5rem 1.3rem; display: flex; flex-direction: column; gap: .65rem; box-shadow: 0 6px 24px rgba(60,50,30,.07); }
.home-card h2 { font-family: var(--head); font-size: 1.6rem; margin: 0; } .home-card p { margin: 0; }
.home-icon-img { width: 64px; height: 64px; filter: saturate(.8); }
.home-source { font-size: .85rem; color: var(--bs-secondary-color); margin-top: .2rem; } .home-source a { color: inherit; text-decoration: underline; }
.home-go { margin-top: auto; align-self: flex-start; min-height: 44px; } .home-go .go-arrow { margin-left: .6rem; }
.home-help { color: var(--bs-secondary-color); } .home-help a { color: var(--bs-primary); font-weight: 500; }
/* Both map pages share one frame: a full-height sidebar on the left, the paper-on-desk stage on the right. */
.tab-pane[data-value='history'], .tab-pane[data-value='home'] { padding: 0 !important; margin: 0 !important; } .history-page .bslib-sidebar-layout { margin: 0 !important; border: 0 !important; } .container-fluid:has(> .tab-content > .tab-pane.active[data-value='history']), .container-fluid:has(> .tab-content > .tab-pane.active[data-value='home']) { padding-left: 0 !important; padding-right: 0 !important; } .history-page { height: auto; }
.history-cols { height: calc(100vh - var(--nav-h)); min-height: 480px; --_sidebar-bg: var(--paper); }
.history-cols { border: 0 !important; border-radius: 0 !important; margin: 0 !important; } .history-cols.main, .history-cols > .main { padding: 24px !important; overflow: auto; }   /* the page color shows around the stage, exactly as on Current maps */
.history-map { align-self: stretch; display: flex; flex-direction: column; align-items: center; }
.history-map:not(:fullscreen):not(.map-pseudo-full) { background: var(--stage); padding: 1rem; min-height: calc(100vh - var(--nav-h) - 48px); }   /* the same stage color and 24 px margin as Current maps */
.history-map img { max-height: calc(100vh - var(--nav-h) - 8rem) !important; }
.set-switch { display: flex; gap: .3rem; padding: .25rem; background: var(--stage); border-radius: .75rem; margin-bottom: 1.1rem; }
.set-btn { flex: 1 1 0; min-height: 44px; border: 0; border-radius: .55rem; background: transparent; color: var(--bs-body-color); font-size: .85rem; font-weight: 500; padding: .2rem .35rem; }
.set-btn:hover { background: var(--hover-wash); } .set-btn.is-active { background: var(--bs-primary); color: var(--on-primary); }
.set-btn:focus-visible { outline: 2px solid var(--bs-primary); outline-offset: 2px; }
/* Help: topics down the left, questions that open one at a time. */
.settings-page { max-width: 760px; margin: 0 auto; padding: 1.75rem 1.25rem 3rem; }
.settings-page h1 { font-family: var(--head); font-size: 2rem; margin-bottom: .3rem; } .settings-intro { color: var(--bs-secondary-color); margin-bottom: 1.2rem; }
.set-card { background: var(--surface); border: 1px solid var(--line); border-radius: .8rem; padding: 1rem 1.25rem 1.1rem; margin: 0 0 1rem; }
.set-card h2 { font-family: var(--head); font-size: 1.3rem; margin: 0 0 .35rem; }
.settings-page .shiny-input-container, .settings-page .bslib-input-switch { width: 100%; } .settings-page .form-switch { margin-bottom: .2rem; } .settings-page .fine { margin-bottom: .9rem; }
.settings-page .cvd-pick { display: block; } .settings-page .cvd-pick .form-group, .settings-page .cvd-pick .shiny-input-container { display: block; }
.settings-page .cvd-pick label { display: block; margin-bottom: .25rem; font-size: .9rem; font-weight: 500; } .settings-page .cvd-pick select { width: 100%; max-width: 22rem; }
.help-page { max-width: 1100px; margin: 0 auto; padding: 1.75rem 1.25rem 3rem; }
.help-top h1 { font-family: var(--head); font-size: 2rem; margin-bottom: .3rem; } .help-lede { color: var(--bs-secondary-color); max-width: 46rem; margin-bottom: 1.25rem; }
.help-page .nav-pills .nav-link { min-height: 44px; display: flex; align-items: center; }
.help-panel { padding-left: .5rem; }
/* The answers sit in a panel of fixed height that scrolls, so the About block below never moves when a question opens or closes. */
.help-page .tab-content { height: clamp(22rem, 50vh, 32rem); overflow-y: auto; padding-right: .5rem; border: 1px solid var(--line); border-radius: .6rem; padding: .25rem .9rem; background: var(--help-panel, rgba(255,255,255,.4)); }
.help-page .tab-content:focus-visible { outline: 2px solid var(--bs-primary); outline-offset: 2px; }
.help-page.is-tall .tab-content { height: calc(100vh - var(--nav-h) - 9rem); }
.help-grow { margin-bottom: .9rem; } .help-grow .when-expanded { display: none; } .help-page.is-tall .help-grow .when-normal { display: none; } .help-page.is-tall .help-grow .when-expanded { display: inline; }
.help-section { border-bottom: 1px solid var(--line); }
.help-section summary { min-height: 44px; display: flex; align-items: center; gap: .6rem; cursor: pointer; list-style: none; padding: .35rem 0; font-size: 1.02rem; }
.help-section summary::-webkit-details-marker { display: none; }
.help-section summary::before { content: ''; width: .5rem; height: .5rem; border-right: 2px solid currentColor; border-bottom: 2px solid currentColor; transform: rotate(-45deg); transition: transform .15s; flex: none; }
.help-section[open] summary::before { transform: rotate(45deg); } .help-section[open] summary { font-weight: 600; }
.help-section summary:focus-visible { outline: 2px solid var(--bs-primary); outline-offset: 2px; border-radius: .3rem; }
.help-body { padding: 0 .25rem 1rem 1.4rem; max-width: 46rem; } .help-body ul, .help-body ol { padding-left: 1.2rem; } .help-body li { margin-bottom: .35rem; }
.credit-list { list-style: none; padding: 0; } .credit-list li { padding: .7rem 0; border-bottom: 1px solid var(--line); }
.help-about { margin-top: 2.5rem; padding-top: 1.25rem; border-top: 1px solid var(--line); } .help-about h2 { font-family: var(--head); font-size: 1.3rem; }
.about-list > div { display: grid; grid-template-columns: 9rem 1fr; gap: .5rem; padding: .4rem 0; } .about-list dt { font-weight: 600; } .about-list dd { margin: 0; }
@media (max-width: 700px) { .about-list > div { grid-template-columns: 1fr; gap: 0; } }
/* Color vision recolors the page itself: the accent color of buttons, tabs, switches, and links, and the warm accent. Maps use their own palettes. */
body.cb-deutan, body.cb-protan { --bs-primary: #233166; --bs-primary-rgb: 35,49,102; --sm-accent: #a38218; --bs-link-color: #233166; --bs-link-color-rgb: 35,49,102; --cb-btn: #233166; --cb-btn-hover: #1a2550; }
body.cb-tritan { --bs-primary: #710039; --bs-primary-rgb: 113,0,57; --sm-accent: #006d5f; --bs-link-color: #710039; --bs-link-color-rgb: 113,0,57; --cb-btn: #710039; --cb-btn-hover: #560029; }
body.cb-gray { --bs-primary: #222222; --bs-primary-rgb: 34,34,34; --sm-accent: #555555; --bs-link-color: #222222; --bs-link-color-rgb: 34,34,34; --cb-btn: #222222; --cb-btn-hover: #000000; }
body[class*='cb-'] .btn-primary { --bs-btn-bg: var(--cb-btn); --bs-btn-border-color: var(--cb-btn); --bs-btn-hover-bg: var(--cb-btn-hover); --bs-btn-hover-border-color: var(--cb-btn-hover); --bs-btn-active-bg: var(--cb-btn-hover); --bs-btn-active-border-color: var(--cb-btn-hover); --bs-btn-disabled-bg: var(--cb-btn); --bs-btn-disabled-border-color: var(--cb-btn); }
body[class*='cb-'] .btn-outline-primary { --bs-btn-color: var(--cb-btn); --bs-btn-border-color: var(--cb-btn); --bs-btn-hover-bg: var(--cb-btn); --bs-btn-hover-border-color: var(--cb-btn); --bs-btn-active-bg: var(--cb-btn); --bs-btn-active-border-color: var(--cb-btn); }
body[class*='cb-'] .nav-pills { --bs-nav-pills-link-active-bg: var(--cb-btn); }
body[class*='cb-'] .form-check-input:checked, body[class*='cb-'] .form-switch .form-check-input:checked { background-color: var(--cb-btn); border-color: var(--cb-btn); }
body[class*='cb-'] .irs--shiny .irs-bar, body[class*='cb-'] .irs--shiny .irs-handle { background: var(--cb-btn); border-color: var(--cb-btn); }
body[class*='cb-'] .style-picker .shiny-options-group label:has(input:checked), body[class*='cb-'] .view-picker .shiny-options-group label:has(input:checked) { border-color: var(--cb-btn); box-shadow: 0 0 0 2px rgba(var(--bs-primary-rgb), .2); }
body[class*='cb-'] .ctl-head .step, body[class*='cb-'] .set-btn.is-active { background: var(--cb-btn); }
body[class*='cb-'] .s-total { --accent: var(--cb-btn); } body.cb-deutan .s-through, body.cb-protan .s-through { --accent: #a38218; } body.cb-tritan .s-through { --accent: #006d5f; } body.cb-gray .s-through { --accent: #777777; }
body.cb-gray .home-icon-img, body.cb-gray .home::before, body.cb-gray .brand svg, body.cb-gray .watermark { filter: grayscale(1); }
body.cb-tritan .need-draw, body.cb-deutan .need-draw, body.cb-protan .need-draw, body.cb-gray .need-draw { background: rgba(var(--bs-primary-rgb), .07); }
body.cb-deutan, body.cb-protan { --cb-ok: #233166; } body.cb-tritan { --cb-ok: #006d5f; } body.cb-gray { --cb-ok: #222222; }
body.cb-tritan .hl-line { stroke: #006d5f; }

/* Dark appearance. Bootstrap's own dark mode is switched on with the same attribute, so form controls, cards and tables follow; these tokens retune the page itself.
   The primary color is lightened so it keeps 4.5:1 against the dark page, and the text on it turns dark to match. */
html[data-bs-theme='dark'] { --line: #3a3f47; --stage: #1f2227; --paper: #23262b; --surface: #2b2f35; --field: #2b2f35; --field-line: #7b8390; --on-primary: #0e1520; --hover-wash: rgba(255,255,255,.08);
  --bs-body-bg: #111316; --bs-body-bg-rgb: 17,19,22; --bs-body-color: #e8e6df; --bs-body-color-rgb: 232,230,223; --bs-secondary-color: #b2b0a8; --bs-secondary-bg: #2b2f35; --bs-border-color: #3a3f47;
  --bs-primary: #8db3e6; --bs-primary-rgb: 141,179,230; --bs-link-color: #9cc0ee; --bs-link-color-rgb: 156,192,238; --sm-accent: #e8a35a; --cb-ok: #6fcf9f; color-scheme: dark; }
html[data-bs-theme='dark'] .btn-primary { --bs-btn-color: var(--on-primary); --bs-btn-bg: var(--bs-primary); --bs-btn-border-color: var(--bs-primary); --bs-btn-hover-color: var(--on-primary); --bs-btn-hover-bg: #a9c6ee; --bs-btn-hover-border-color: #a9c6ee; --bs-btn-active-color: var(--on-primary); --bs-btn-active-bg: #a9c6ee; --bs-btn-active-border-color: #a9c6ee; --bs-btn-disabled-color: var(--on-primary); --bs-btn-disabled-bg: var(--bs-primary); --bs-btn-disabled-border-color: var(--bs-primary); }
html[data-bs-theme='dark'] .btn-outline-primary { --bs-btn-color: var(--bs-primary); --bs-btn-border-color: var(--bs-primary); --bs-btn-hover-color: var(--on-primary); --bs-btn-hover-bg: var(--bs-primary); --bs-btn-hover-border-color: var(--bs-primary); --bs-btn-active-color: var(--on-primary); --bs-btn-active-bg: var(--bs-primary); }
html[data-bs-theme='dark'] .btn-outline-secondary { --bs-btn-color: #c9c7bf; --bs-btn-border-color: #7b8390; --bs-btn-hover-bg: #3a3f47; --bs-btn-hover-color: #fff; --bs-btn-hover-border-color: #9aa2ad; }
html[data-bs-theme='dark'] .form-check-input:checked, html[data-bs-theme='dark'] .form-switch .form-check-input:checked, html[data-bs-theme='dark'] .shiny-input-container input[type='radio']:checked { background-color: var(--bs-primary) !important; border-color: var(--bs-primary) !important; }
html[data-bs-theme='dark'] .form-check-input[type='radio']:checked { --bs-form-check-bg-image: url('data:image/svg+xml,%3csvg xmlns=%27http://www.w3.org/2000/svg%27 viewBox=%27-4 -4 8 8%27%3e%3ccircle r=%272%27 fill=%27%230e1520%27/%3e%3c/svg%3e'); }
html[data-bs-theme='dark'] .irs--shiny .irs-bar, html[data-bs-theme='dark'] .irs--shiny .irs-handle { background: var(--bs-primary); border-color: var(--bs-primary); }
html[data-bs-theme='dark'] .irs--shiny .irs-line { background: #3a3f47; border-color: #4a5058; }
html[data-bs-theme='dark'] .irs--shiny .irs-single, html[data-bs-theme='dark'] .irs--shiny .irs-from, html[data-bs-theme='dark'] .irs--shiny .irs-to { background: var(--bs-primary); color: var(--on-primary); }
html[data-bs-theme='dark'] .irs--shiny .irs-min, html[data-bs-theme='dark'] .irs--shiny .irs-max { background: #3a3f47; color: #d4d2ca; }
html[data-bs-theme='dark'] .navbar .nav-link.active, html[data-bs-theme='dark'] .navbar .nav-link:hover { color: #fff; }
html[data-bs-theme='dark'] input[type='radio'], html[data-bs-theme='dark'] input[type='checkbox'] { accent-color: var(--bs-primary); }
html[data-bs-theme='dark'] { --place-card: var(--surface); --home-card: rgba(35,38,43,.94); --help-panel: var(--paper); }
html[data-bs-theme='dark'] .nav-pills { --bs-nav-pills-link-active-bg: var(--bs-primary); --bs-nav-pills-link-active-color: var(--on-primary); }
html[data-bs-theme='dark'] .help-page .nav-pills .nav-link:not(.active) { color: var(--bs-link-color); }
html[data-bs-theme='dark'] .help-section summary { color: var(--bs-body-color); }
html[data-bs-theme='dark'] .home::before { opacity: .1; filter: brightness(.8) saturate(.6); }
html[data-bs-theme='dark'] .home-card { box-shadow: 0 6px 24px rgba(0,0,0,.35); }
html[data-bs-theme='dark'] .home-icon-img { filter: saturate(.8) brightness(.95); }
html[data-bs-theme='dark'] .hl-line { stroke: #ffb067; }
html[data-bs-theme='dark'] .history-map img { border-color: var(--line); }
html[data-bs-theme='dark'] body.cb-deutan, html[data-bs-theme='dark'] body.cb-protan { --bs-primary: #a9b8f2; --bs-primary-rgb: 169,184,242; --bs-link-color: #b5c2f5; --bs-link-color-rgb: 181,194,245; --sm-accent: #d9bd50; --cb-btn: #a9b8f2; --cb-btn-hover: #c3cdf7; --cb-ok: #a9b8f2; }
html[data-bs-theme='dark'] body.cb-tritan { --bs-primary: #f0a0c0; --bs-primary-rgb: 240,160,192; --bs-link-color: #f4b4cd; --bs-link-color-rgb: 244,180,205; --sm-accent: #5fd0bf; --cb-btn: #f0a0c0; --cb-btn-hover: #f6bad1; --cb-ok: #5fd0bf; }
html[data-bs-theme='dark'] body.cb-gray { --bs-primary: #d6d6d6; --bs-primary-rgb: 214,214,214; --bs-link-color: #e0e0e0; --bs-link-color-rgb: 224,224,224; --sm-accent: #b0b0b0; --cb-btn: #d6d6d6; --cb-btn-hover: #ffffff; --cb-ok: #d6d6d6; }
html[data-bs-theme='dark'] body.cb-tritan .hl-line { stroke: #5fd0bf; }
html[data-bs-theme='dark'] body[class*='cb-'] .btn-primary, html[data-bs-theme='dark'] body[class*='cb-'] .btn-primary:hover { --bs-btn-color: var(--on-primary); --bs-btn-hover-color: var(--on-primary); --bs-btn-active-color: var(--on-primary); }
.theme-btn { min-height: 44px; white-space: nowrap; font-size: .85rem; padding: .3rem .7rem; }
.theme-btn .when-dark { display: none; } html[data-bs-theme='dark'] .theme-btn .when-dark { display: inline; } html[data-bs-theme='dark'] .theme-btn .when-light { display: none; }
body.cb-tritan #map:empty, body.cb-tritan .home::before { filter: saturate(.15); }
/* Gray means gray everywhere, including the pictures, the look previews and focus rings that carry their own colors. */
html:has(body.cb-gray) { filter: grayscale(1); }
@media (prefers-reduced-motion: reduce) { .home-card, .navbar .nav-link, .help-section summary::before { transition: none !important; } }

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
                  background: var(--bs-primary); color: var(--on-primary); font-size: .72rem; letter-spacing: 0; }
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
                      border-radius: .65rem; background: var(--surface); cursor: pointer; text-align: center; font-size: .74rem; font-weight: 500; white-space: nowrap; }
.style-picker .shiny-options-group label > span { display: flex; flex-direction: column; align-items: stretch; width: 100%; min-width: 0; margin: 0; padding: 0; }
.style-picker .shiny-options-group label > input { position: absolute; }
.style-picker input:checked + label, .style-picker .shiny-options-group label:has(input:checked) { border-color: var(--bs-primary); box-shadow: 0 0 0 2px rgba(var(--bs-primary-rgb), .18); }
.style-picker input:focus-visible + label, .style-picker .shiny-options-group label:has(input:focus-visible) { outline: 2px solid var(--bs-primary); outline-offset: 2px; }
.sw { display: flex; flex-direction: column; justify-content: center; gap: 3px; width: 100%; height: 34px; box-sizing: border-box; margin-bottom: .4rem; padding: 0 .6rem;
      border: 1px solid; border-radius: .4rem; }
.sw i { display: block; border-radius: 2px; }
.fine { font-size: .75rem; color: var(--bs-secondary-color); margin: 0; }
.addr-msg { margin: 0 0 .5rem; }
.sugg { list-style: none; margin: .2rem 0 .7rem; padding: .25rem; border: 1px solid var(--bs-border-color); border-radius: .5rem; background: var(--bs-body-bg); }
.sugg li { margin: 0; }
.sugg-item { display: block; width: 100%; min-height: 44px; padding: .5rem .65rem; border: 0; border-radius: .35rem; background: transparent; color: var(--bs-body-color); text-align: left; font-size: .92rem; line-height: 1.25; cursor: pointer; }
.sugg-item:hover, .sugg-item:focus-visible { background: rgba(var(--bs-primary-rgb), .10); outline: 2px solid transparent; box-shadow: inset 0 0 0 2px var(--bs-primary); }
.need-draw { margin: .7rem 0 0; padding: .5rem .7rem; border-left: 3px solid var(--sm-accent, #d9822b); background: rgba(217, 130, 43, .09); border-radius: 0 .4rem .4rem 0; color: var(--bs-body-color); }
.accordion { --bs-accordion-bg: transparent; --bs-accordion-border-color: var(--line); font-size: .85rem; }
.accordion-button { padding: .6rem .8rem; font-size: .85rem; font-weight: 500; }
.stage { display: grid; grid-template-columns: minmax(0, 1fr) 17rem; grid-template-rows: minmax(0, 1fr); gap: .85rem; background: var(--stage); }
.stage > *, .map-card .card-body, #map { min-width: 0; }
main.bslib-page-main, .bslib-sidebar-layout > .main { min-width: 0 !important; }
.stage { width: 100%; max-width: 100%; padding-top: 24px; }   /* the same 24 px of stage above the paper and stats as below, so the page looks like a sheet on a desk */
.stats-slot { display: block !important; grid-column: 2; grid-row: 1; min-height: 0; overflow-y: auto; background: transparent; }
.map-card { grid-column: 1; grid-row: 1; }
.stage:not(:has(.stats-slot > *)) { grid-template-columns: minmax(0, 1fr); }   /* no numbers yet: the empty graphic centers in the whole stage, not beside a blank column */
.stage:not(:has(.stats-slot > *)) > .stats-slot { display: none !important; }
.area-picker .shiny-options-group { display: flex; flex-direction: column; gap: .1rem; }
.area-picker .form-check, .area-picker .radio { margin: 0; min-height: 0; }
.area-picker .form-check-label, .area-picker .radio label { font-size: .88rem; }
.view-picker .shiny-options-group { display: grid; grid-template-columns: 1fr 1fr; gap: .5rem; }
.view-picker .form-check, .view-picker .radio { padding: 0; margin: 0; min-height: 0; }
.view-picker input[type=radio] { position: absolute; opacity: 0; pointer-events: none; }
.view-picker .shiny-options-group label { display: flex; flex-direction: column; gap: .15rem; width: 100%; height: 100%; margin: 0; padding: .55rem .6rem !important; border: 1.5px solid var(--line);
  border-radius: .65rem; background: var(--surface); cursor: pointer; }
.view-picker .vt, .view-picker .vb { display: block; }
.view-picker .vt { font-weight: 600; font-size: .86rem; line-height: 1.15; }
.view-picker .vb { font-size: .72rem; line-height: 1.25; color: var(--bs-secondary-color); }
.view-picker .shiny-options-group label:has(input:checked) { border-color: var(--bs-primary); box-shadow: 0 0 0 2px rgba(var(--bs-primary-rgb), .18); }
.view-picker .shiny-options-group label:has(input:focus-visible) { outline: 2px solid var(--bs-primary); outline-offset: 2px; }
.stat-row { display: grid; grid-template-columns: 1fr; gap: .75rem; align-content: start; }
.stat { background: var(--surface); border: 1px solid var(--line); border-radius: .8rem; padding: .7rem 1rem .75rem; border-left: 5px solid var(--accent, var(--bs-primary)); }
.stat-value { font-family: var(--head); font-size: 2rem; line-height: 1.05; letter-spacing: .01em; }
.stat-label { font-size: .7rem; text-transform: uppercase; letter-spacing: .09em; font-weight: 600; color: var(--bs-secondary-color); margin-top: .15rem; }
.stat-note { font-size: .78rem; color: var(--bs-secondary-color); }
.zoom-hint { margin-top: .85rem; }
.cmp-table td:not(:first-child), .cmp-table th:not(:first-child) { text-align: right; } .cmp-table td:first-child { color: var(--bs-secondary-color); }
body:has(#inspect:checked) #map img { cursor: crosshair; }
.hl-case { fill: none; stroke: var(--surface); stroke-width: 7px; stroke-linecap: round; stroke-linejoin: round; }
.hl-line { fill: none; stroke: #d9480f; stroke-width: 3.5px; stroke-linecap: round; stroke-linejoin: round; }
#map { position: relative; }
.street-found ul { margin: .25rem 0 0 1rem; padding: 0; }
.inspect { background: var(--surface); border: 1px solid var(--line); border-radius: .8rem; padding: .7rem 1rem .8rem; border-left: 5px solid var(--bs-secondary-color); }
.inspect-title { font-family: var(--head); font-size: 1.35rem; line-height: 1.1; margin-bottom: .35rem; }
.inspect-row { display: flex; justify-content: space-between; gap: .75rem; font-size: .82rem; padding: .12rem 0; border-top: 1px solid var(--line); }
.inspect-k { color: var(--bs-secondary-color); } .inspect-v { text-align: right; }
.draw-row { display: flex; gap: .5rem; } .draw-row .btn { flex: 1; white-space: nowrap; padding-left: .5rem; padding-right: .5rem; }
.stat-head { font-size: .7rem; text-transform: uppercase; letter-spacing: .09em; font-weight: 600; color: var(--bs-secondary-color); margin: .3rem 0 -.2rem; }
.stat-mini .stat { padding: .45rem .8rem .5rem; border-left-width: 3px; }
.stat-mini .stat-value { font-size: 1.35rem; }
.stat-mini .stat-label { text-transform: none; letter-spacing: .01em; font-size: .78rem; }
.stat-mini .stat-note { font-size: .72rem; }
.s-total { --accent: var(--bs-primary); } .s-local { --accent: #9aa8b6; } .s-through { --accent: #a6372a; }
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
/* The empty page shows the watermark and, below it, one line, centered together in the stage on both map pages. */
#map:empty, .history-map .shiny-image-output:empty { display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 1.6rem; color: var(--bs-secondary-color); text-align: center; }
#map:empty::before, .history-map .shiny-image-output:empty::before { content: ''; flex: none; width: min(60vmin, 480px); aspect-ratio: 1; opacity: .3; background: url(\"data:image/svg+xml,__WM__\") center / contain no-repeat; -webkit-mask-image: radial-gradient(closest-side, #000 25%, rgba(0,0,0,.92) 38%, rgba(0,0,0,.78) 48%, rgba(0,0,0,.6) 58%, rgba(0,0,0,.4) 68%, rgba(0,0,0,.22) 78%, rgba(0,0,0,.09) 88%, rgba(0,0,0,.02) 96%, transparent 100%); mask-image: radial-gradient(closest-side, #000 25%, rgba(0,0,0,.92) 38%, rgba(0,0,0,.78) 48%, rgba(0,0,0,.6) 58%, rgba(0,0,0,.4) 68%, rgba(0,0,0,.22) 78%, rgba(0,0,0,.09) 88%, rgba(0,0,0,.02) 96%, transparent 100%); }
#map:empty::after, .history-map .shiny-image-output:empty::after { max-width: 32rem; font-size: 1.05rem; }
html[data-bs-theme='dark'] #map:empty::before, html[data-bs-theme='dark'] .history-map .shiny-image-output:empty::before { background-image: url(\"data:image/svg+xml,__WMD__\"); }
#map:empty::after { content: 'Choose a location on the left, then draw its map.'; }
.history-map .shiny-image-output:empty::after { content: 'Choose a location on the left, then draw its map.'; }
.history-map .shiny-image-output:empty { flex: 1 1 auto; width: 100%; }
.history-page .bslib-sidebar-layout { height: calc(100vh - var(--nav-h)) !important; max-height: calc(100vh - var(--nav-h)); }
/* Side panel */
.place-card { margin: .1rem 0 .75rem; padding: .6rem .7rem; border: 1px solid var(--line); border-radius: .6rem; background: var(--place-card, rgba(255,255,255,.55)); }
.place-line { display: flex; gap: .55rem; align-items: flex-start; }
.place-check { color: var(--cb-ok, #2f7d5b); font-size: 1.05rem; line-height: 1.35; }
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
.sidebar .form-select { font-size: .92rem; line-height: 1.35; color: var(--bs-body-color); background-color: var(--field); border: 1.5px solid var(--field-line); border-radius: .5rem;
  padding: .45rem 2.2rem .45rem .7rem; min-height: 2.6rem; text-overflow: ellipsis; }
.sidebar .form-select:focus { border-color: var(--bs-primary); box-shadow: 0 0 0 3px rgba(var(--bs-primary-rgb), .2); }
.sidebar .form-select option { color: var(--bs-body-color); background: var(--field); font-size: .92rem; }
.dl-grid { display: grid; grid-template-columns: 1fr 1fr; column-gap: .7rem; row-gap: .1rem; } .dl-grid .span-2 { grid-column: 1 / -1; } .dl-grid .
.dl-grid .form-group, .dl-grid .shiny-input-container { margin-bottom: .6rem; }
.more-panels { margin-top: 1.1rem; }
.more-panels .accordion-body { padding: .8rem .85rem 1rem; }
.more-panels .accordion-body .remember { margin-top: .9rem; padding-top: .8rem; border-top: 1px solid var(--line); }
.sidebar .ctl { margin-bottom: 1.15rem; }
.sidebar .ctl-head { margin-bottom: .75rem; }
.sidebar .shiny-input-container > label.control-label { margin-bottom: .35rem; }
.sidebar .bslib-input-switch, .sidebar .form-switch { margin-bottom: .45rem; }

"

# The same drawing recolored for the dark page: every color in it is swapped through one table, so the shapes stay identical.
watermark_dark_map <- c("#efece1" = "#2a2d35", "#e4dfcd" = "#30343d", "#e1dbc6" = "#2d3139", "#e8e4d4" = "#333741",
                        "#dbe3cc" = "#2b3a33", "#cdd9d6" = "#2c4350", "#cddbdc" = "#3b5666", "#bccfd3" = "#33505f",
                        "#f6f9f7" = "#8aa3b0", "#f6efd9" = "#8c8672", "#c9bd9a" = "#5d594a", "#bdb393" = "#6b6553",
                        "#a89c7c" = "#a39a82", "#cfc8b6" = "#4a4a45")
watermark_svg_dark <- local({
  m <- gregexpr("#[0-9a-fA-F]{6}", watermark_svg)
  regmatches(watermark_svg, m) <- list(vapply(regmatches(watermark_svg, m)[[1]], function(h) {
    v <- watermark_dark_map[tolower(h)]; if (is.na(v)) h else unname(v) }, ""))
  watermark_svg
})
app_css <- gsub("__WM__", utils::URLencode(watermark_svg, reserved = TRUE), app_css, fixed = TRUE)
app_css <- gsub("__WMD__", utils::URLencode(watermark_svg_dark, reserved = TRUE), app_css, fixed = TRUE)
app_js <- "
// Expand: give the map the whole window. The sidebar folds away (the app does that when it hears the state change) and, where the
// browser allows it, the page goes full screen too. Nothing here opens a new window, so popup blockers never get involved.
var streetMapsUsedFullscreen = false;
function streetMapsSetExpanded(on) {
  document.documentElement.classList.toggle('is-expanded', on);
  var b = document.getElementById('expand-btn'); if (b) { b.setAttribute('aria-pressed', on ? 'true' : 'false'); }
  if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('expanded', on, { priority: 'event' }); }
}
// The Expand button belongs to the two map pages (Current maps and Historical maps), so the page in view is kept on the root element for the style sheet to read. Only the header's tabs count: the hidden tabs that switch between the three sets of historical maps must not look like a page change, or Expand would vanish on Egyptian desert and Ancient cities. Leaving a map page also leaves Expand.
$(document).on('shown.bs.tab', '.navbar a[data-bs-toggle=tab][data-value]', function (e) {
  var v = e.target.getAttribute('data-value'); if (!v) return;
  document.documentElement.setAttribute('data-page', v);
  if (v !== 'current' && v !== 'history' && document.documentElement.classList.contains('is-expanded')) { streetMapsToggleExpand(); }
  window.scrollTo(0, 0); setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 50);
});
document.documentElement.setAttribute('data-page', 'home');
// The three sets of historical maps share one picker; a click tells the server which set to show.
function overmapsSet(v) { if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('h_set', v, { priority: 'event' }); } }
// Color vision recolors the page itself, so the choice becomes one class on the body (cb-deutan, cb-protan, cb-tritan, or cb-gray) that the style sheet reads.
Shiny.addCustomMessageHandler('cvdClass', function (m) {
  ['cb-deutan', 'cb-protan', 'cb-tritan', 'cb-gray'].forEach(function (c) { document.body.classList.remove(c); });
  if (m && m !== 'standard') { document.body.classList.add('cb-' + m); }
});
// Light or dark page. The first visit follows the computer's setting; after that the last choice is kept in this browser (a blocked store only means the choice is not kept).
function overmapsSetTheme(mode) {
  var d = document.documentElement; d.setAttribute('data-bs-theme', mode);
  var b = document.getElementById('theme-btn'); if (b) { b.setAttribute('aria-pressed', mode === 'dark' ? 'true' : 'false'); }
  try { localStorage.setItem('overmaps-theme', mode); } catch (e) {}
}
function overmapsToggleTheme() { overmapsSetTheme(document.documentElement.getAttribute('data-bs-theme') === 'dark' ? 'light' : 'dark'); }
(function () {
  var saved = null; try { saved = localStorage.getItem('overmaps-theme'); } catch (e) {}
  var mode = (saved === 'dark' || saved === 'light') ? saved : (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
  document.documentElement.setAttribute('data-bs-theme', mode);
  document.addEventListener('DOMContentLoaded', function () { var b = document.getElementById('theme-btn'); if (b) { b.setAttribute('aria-pressed', mode === 'dark' ? 'true' : 'false'); } });
})();
// Pages below the header start where it ends, whatever its height is on this screen.
function overmapsHelpGrow(b) { var pg = b.closest('.help-page'); var on = pg.classList.toggle('is-tall'); b.setAttribute('aria-pressed', on ? 'true' : 'false'); }
$(document).on('shiny:connected', function () { $('.help-page .tab-content').attr({ tabindex: 0, role: 'region', 'aria-label': 'Help answers' }); });
function overmapsNavHeight() { var n = document.querySelector('.navbar'); if (n) { document.documentElement.style.setProperty('--nav-h', n.offsetHeight + 'px'); } }
$(window).on('load resize', overmapsNavHeight); $(document).on('shiny:connected shown.bs.tab', function () { setTimeout(overmapsNavHeight, 30); });
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
Shiny.addCustomMessageHandler('sliderPost', function (m) { var s = $('#' + m.id).data('ionRangeSlider'); if (s) s.update({postfix: m.post}); });
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
brand_dark_map <- c("#fdfaf1" = "#343842", "#eadfc4" = "#3b404b", "#e3d6b4" = "#383c47", "#efe6cf" = "#3e434f", "#c8dfae" = "#365046",
                    "#86c1dc" = "#5b8fa8", "#4f9bc4" = "#477f98", "#a9c6d3" = "#35566a", "#fffdf6" = "#c2baa0", "#6c5f43" = "#7a745f",
                    "#fff9e6" = "#4a4f5c", "#cdbb8a" = "#3a3e49", "#4a3a22" = "#14161a")
brand_svg_dark <- local({
  m <- gregexpr("#[0-9a-fA-F]{6}", brand_svg)
  regmatches(brand_svg, m) <- list(vapply(regmatches(brand_svg, m)[[1]], function(h) {
    v <- brand_dark_map[tolower(h)]; if (is.na(v)) h else unname(v) }, ""))
  brand_svg
})
brand_svg_dark <- gsub("(id=\"|url\\(#)(km[a-z]+)", "\\1\\2d", brand_svg_dark)
brand_mark <- tagList(HTML(sub("<svg ", "<svg class=\"brand-light\" ", brand_svg)), HTML(sub("<svg ", "<svg class=\"brand-dark\" ", brand_svg_dark)))
favicon_href <- paste0("data:image/svg+xml,", utils::URLencode(brand_svg, reserved = TRUE))

# The look previews show the colors a map will really use, so they follow the Color vision setting in Settings.
style_choice_names_for <- function(mode = "standard") lapply(names(styles), function(k) {
  p <- get_style(style_key(k, mode))
  tagList(div(class = "sw", style = sprintf("background:%s;border-color:%s", p$bg, p$ring),
              tags$i(style = sprintf("height:4px;background:%s", p$cols[1])),
              tags$i(style = sprintf("height:3px;background:%s", p$cols[2])),
              tags$i(style = sprintf("height:2px;background:%s", p$cols[3])),
              tags$i(style = sprintf("height:1px;background:%s", p$cols[5]))),
          p$label)
})
style_choice_names <- style_choice_names_for("standard")

view_choice_names <- lapply(names(views), function(k)
  tagList(span(class = "vt", views[[k]]), span(class = "vb", view_blurbs[[k]])))

section <- function(n, title, ...) div(class = "ctl", div(class = "ctl-head", span(class = "step", n), title), ...)
# File type and resolution side by side. Pictures offer dots per inch; PDF, SVG and EPS have none, so the menu then offers only "Vector" (the server
# swaps the choices when the file type changes). `extra` spans both columns.
# The file type menu: pictures, a thin non-selectable divider, then the vector files.
format_select <- function(id) {
  sel <- selectInput(id, "File type", choices = export_formats, selected = "png", selectize = FALSE)
  html <- as.character(sel$children[[2]]$children[[1]]$children[[1]])        # div > select > the options, as one piece of html
  sel$children[[2]]$children[[1]]$children[[1]] <- HTML(sub("(<option value=\"eps\")",
    "<option value=\"\" disabled class=\"menu-divider\">\u2500\u2500\u2500\u2500\u2500\u2500</option>\n\\1", html))
  sel
}
# The file's own color vision setting. It starts at Standard and never follows the setting in Settings, which recolors the screen only, so a person who
# reads the screen in grayscale can still save a standard-color map, or the other way around.
download_selects <- function(fmt_id, res_id, extra = NULL, cvd_id = NULL)
  div(class = "dl-grid",
      format_select(fmt_id),
      selectInput(res_id, "Resolution", choices = resolution_choices, selected = "300", selectize = FALSE),
      if (!is.null(cvd_id)) div(class = "span-2", selectInput(cvd_id, "Color vision of the file", choices = cvd_modes, selected = "standard", selectize = FALSE)),
      extra)

# What a download will be, in words: the sheet, the pixels and dots per inch, or the note that vector files have none.
print_note_text <- function(fmt, key, res) {
  pl <- print_plan(key, fmt, res_pick(res)); sz <- paste0(pl$size_in[1], " \u00d7 ", pl$size_in[2], " inches")
  if (is.na(pl$dpi)) paste0(toupper(fmt), " is laid out for ", sz, " and stays sharp at any size. Type and lines grow with the sheet.")
  else paste0("Prints at ", sz, ": ", format(pl$px[1], big.mark = ","), " \u00d7 ", format(pl$px[2], big.mark = ","), " pixels (", pl$dpi, " dpi)",
              if (isTRUE(pl$capped)) paste0(". ", pl$want, " dpi would make the file too large at this size, so it uses fewer dots per inch to keep the file a manageable size.") else ".",
              " Type and lines grow with the sheet.")
}
print_key_of <- function(x) if (isTRUE(x %in% names(print_sizes))) x else "8x10"
size_suffix <- function(x) if (print_key_of(x) == "8x10") "" else paste0("_", print_key_of(x))
print_select <- function(id) div(class = "span-2", selectInput(id, "Print size", choices = print_size_labels, selected = "8x10", selectize = FALSE))

# The Home page: one card per kind of map, each saying where its data comes from. The sources are also repeated on every map page.
source_current <- tagList("Street data: ", tags$a(href = "https://www.openstreetmap.org/copyright", target = "_blank", rel = "noopener", "OpenStreetMap contributors"),
                          " (ODbL). Places are found with Photon, an OpenStreetMap search service, and U.S. addresses with the U.S. Census Bureau geocoder.")
source_roads <- tagList("Roman roads: ", tags$a(href = "https://itiner-e.org", target = "_blank", rel = "noopener", "Itiner-e"),
                        " (CC BY 4.0). Travel times: Tristan Hotham, ", tags$em("By Road and by Sea"), " (CC BY 4.0).")
source_desert <- tagList("Eastern Desert routes: Mani\u00e8re, Cr\u00e9py and Redon (2020), Desert Networks project, CNRS, ",
                         tags$a(href = "https://doi.org/10.5281/zenodo.4291876", target = "_blank", rel = "noopener", "doi:10.5281/zenodo.4291876"), " (CC BY 4.0).")
source_cities <- tagList("Ancient city streets: excavated remains drawn into ", tags$a(href = "https://www.openstreetmap.org/copyright", target = "_blank", rel = "noopener", "OpenStreetMap"),
                         " by volunteers (ODbL), read through the Overpass API.")
# One quiet icon per kind of map. They are Noto emoji icons (Apache 2.0) from Iconify, kept as files in www/icons so the app needs no network for them.
card_icon <- function(file) tags$img(src = paste0("icons/", file), alt = "", class = "home-icon-img", width = 64, height = 64)
home_page <- function() {
  pick <- function(id, icon_file, title, blurb, source, label) div(class = "home-card",
    card_icon(icon_file), tags$h2(title), p(blurb), div(class = "home-source", tags$strong("Source. "), source),
    actionButton(id, tagList(label, span(class = "go-arrow", `aria-hidden` = "true", "\u2192")), class = "btn-primary home-go"))
  div(class = "home", div(class = "home-inner",
      div(class = "home-hero", tags$h1("Geek out on streets"),
          p("Every street map hides a pattern. Pick a town and find out which way its streets run, how fast they let you go, and how tightly they pack. Then go back a few thousand years and wander the roads of the ancient world.")),
      div(class = "home-cards",
          pick("go_current", "map-current.svg", "Current maps",
               "Any town or city on Earth, mapped from its streets today. Choose from eight map types and three looks, then download a print at the size you need.",
               source_current, "Open current maps"),
          pick("go_history", "map-historical.svg", "Historical maps",
               paste0("The roads of the Roman Empire, the caravan routes of Egypt's Eastern Desert, and the street plans of ", nrow(ancient_sites), " ancient cities, from Babylon to Pompeii."),
               tagList(source_roads, " ", source_desert, " ", source_cities), "Open historical maps")),
      p(class = "home-help", "New here? ", actionLink("go_help", "Help"), " explains every map type, and Settings has a Color vision choice that recolors the whole app.")))
}
# The Help page follows the layout used in the other apps: a short lead, topics down the left, and each question closed until it is opened.
help_section <- function(question, ..., open = FALSE) tags$details(class = "help-section", open = if (open) NA else NULL, tags$summary(question), div(class = "help-body", ...))
help_topic <- function(title, ...) nav_panel(title, div(class = "help-panel", ...))
help_page <- function() {
  repo <- "https://github.com/drabhikroy/overmaps"
  ext <- function(href, text) tags$a(href = href, target = "_blank", rel = "noopener", text)
  div(class = "help-page",
      div(class = "help-top", tags$h1("Help"),
          p(class = "help-lede", "Pick a topic on the left, then the question that matches what you are trying to do. Nothing here is needed to use the app, because every map explains itself in its caption.")),
      tags$button(type = "button", class = "btn btn-outline-secondary btn-sm help-grow", `aria-pressed` = "false", onclick = "overmapsHelpGrow(this)",
                  tags$span(class = "when-normal", "Show a taller panel"), tags$span(class = "when-expanded", "Show a shorter panel")),
      navset_pill_list(id = "help_topic", widths = c(3, 9), well = FALSE,
        help_topic("Getting started",
          help_section("What can Overmaps do?", open = TRUE,
            p("It turns the street network of a place into a map you can read and print. You can see which roads carry through traffic, how fast they allow you to go, which way they point, how long the blocks are, where the parks, water, and street lights sit, and, for places in the United States, where people live."),
            p("The header has three pages. Home is where the app opens. Current maps draws any place today. Historical maps draws the roads and cities of the ancient world. Both map pages work the same way, so you can move between them at any time.")),
          help_section("How do I draw my first map?",
            tags$ol(
              tags$li("Open Current maps in the header."),
              tags$li("Under Choose a place, type a town, a city, or a street address, then press Find."),
              tags$li("Under Shape the map, pick a Map type. Road hierarchy is a good first one."),
              tags$li("Under Choose a look, pick Classic, Dusk, or Mono."),
              tags$li("Press Draw map. Changing the map type or the look afterward redraws at once from the streets already downloaded."))),
          help_section("Can I type a street address?",
            p("Yes. The same box takes a town, a city, or an address. Suggestions appear as you type. For a U.S. address with a city and state, the Census Bureau address file is asked too, which finds house numbers that general place search often misses.")),
          help_section("How do I switch between current and historical maps?",
            p("Use the tabs in the header. Each page keeps its own settings, so you can go to Historical maps and come back to find your current map exactly as you left it."))),
        help_topic("Reading the maps",
          help_section("What does each map type show?", open = TRUE,
            tags$ul(
              tags$li(tags$strong("Road hierarchy."), " Roads by role, from highways to local streets."),
              tags$li(tags$strong("Speed limits."), " Posted limits, where OpenStreetMap has them, with a note on how much is missing."),
              tags$li(tags$strong("Direction."), " Which way the streets run, with a compass rose and a bar chart."),
              tags$li(tags$strong("Block length."), " Distance between junctions. Short blocks are easier to walk."),
              tags$li(tags$strong("Street density."), " Street length per square kilometer in hexagons. A switch raises the hexagons into spikes."),
              tags$li(tags$strong("Bike & walk."), " Sidewalks, bike lanes, and paths, as tagged in OpenStreetMap."),
              tags$li(tags$strong("Street lights."), " Lit streets and mapped lamps glowing against the night."),
              tags$li(tags$strong("Nature & water."), " Parks, woods, rivers, lakes, and the sea under the streets."),
              tags$li(tags$strong("Population."), " Residents per square kilometer from the 2020 Census, shaded by block group under the streets. It covers the United States and its territories only."),
              tags$li(tags$strong("Housing."), " Homes per square kilometer from the 2020 Census, shaded the same way."))),
          help_section("Where do the Population and Housing maps get their numbers?",
            p("From the 2020 Census, through the U.S. Census Bureau's TIGERweb service, which is asked for the block groups around your map when you choose Population or Housing. A block group holds roughly 600 to 3,000 people, and each one is shaded evenly, so the map shows how a place is laid out and not exactly where each person lives. Housing counts every housing unit, occupied or vacant. The total counts the share of every block group that falls inside the map, so it is an estimate. For a place outside the United States, the map says that no Census data is available.")),
          help_section("What are the three looks?",
            p("Classic is warm paper with ink colors. Dusk is a dark navy sheet with bright roads. Mono is plain black and white and prints well on any printer. The look changes colors only, never the data.")),
          help_section("Why does a place look thin or empty?",
            p("Every map is drawn from what volunteers have mapped in OpenStreetMap, not from measured traffic or an official survey. Some places are mapped in great detail and others barely at all. Speed limits and street lights are tagged unevenly, so those maps say in their caption how much was found.")),
          help_section("What does the Color vision setting do?",
            p("It recolors the whole app and every map for deuteranopia, protanopia, tritanopia, or grayscale. Speed, direction, and nature colors change, and in grayscale lightness alone carries the meaning. Line weights, legends, and labels carry meaning too, so color is never the only clue. Each palette was tested under simulated color vision.")),
          help_section("What is in Settings?",
            p("Settings, at the right end of the header, holds the choices that apply to the whole app: the Color vision setting, whether places are suggested as you type, whether your choices are remembered on this device, and the download counts.")),
          help_section("What does the Expand button do?",
            p("On Current maps and Historical maps it gives the map the whole window. Press Esc or the same button, now reading Exit expand, to go back."))),
        help_topic("Historical maps",
          help_section("What is in Historical maps?", open = TRUE,
            tags$ul(
              tags$li(tags$strong("Roman roads."), " The Itiner-e road network of the empire, shown by region, as main and secondary roads, strength of the evidence, steepness, or days of travel from Rome."),
              tags$li(tags$strong("Egyptian desert."), " Modeled camel caravan routes across the Eastern Desert."),
              tags$li(tags$strong("Ancient cities."), paste0(" The street plans of ", nrow(ancient_sites), " ancient sites in ", length(unique(ancient_sites$civ)), " regions.")))),
          help_section("Are the ancient maps real?",
            p("The Roman roads come from a scholarly atlas, drawn where the evidence, or the best guess, places them. The desert routes are computed, not found. A research team asked a computer for the cheapest camel route between known sites and watering places, and every map says so. The ancient city plans show what volunteers have mapped from excavated remains. They are not reconstructions of the whole city.")),
          help_section("Why does an ancient city's circle end where it does?",
            p("The starting radius is fitted to the mapped streets, with a small margin, so the circle ends just past the edge of the remains. Use the Radius slider to see more or less. A site with very little mapped keeps the radius listed for it.")),
          help_section("Why is Street lights missing for some ancient sites?",
            p("It is offered only where OpenStreetMap shows at least ten street lamps or lit streets around the site. The lights are modern lighting near the site, not ancient. Nature & water is offered for every site.")),
          help_section("Why do only some ancient sites show a population estimate?",
            p(paste0("A site shows one only where scholars have published an estimate that can be cited, so ", nrow(ancient_estimates), " of the ", nrow(ancient_sites), " sites do. The figure is an estimate, not a count, and studies of the same site often disagree widely, so each is shown with the period it describes and its source. For Angkor the figure covers the whole Greater Angkor Region and not the walled city alone. The full references are in the data folder.")))),
        help_topic("Downloads and sharing",
          help_section("Can I use a dark page?",
            p("Yes. The Dark button at the right end of the header switches the whole app to a dark page, and the same button then reads Light. The first time you visit, the app follows your computer's setting, and after that it remembers your last choice in this browser. The page theme changes the app only. The Classic, Dusk, and Mono looks still decide the colors of a map, and a saved file never depends on the page theme, only on the look and the file's Color vision menu.")),
          help_section("Can I save a map in a different color vision setting?",
            p("Yes. The Color vision setting in Settings changes how the app and the maps look on your screen. Each Download section has its own menu, Color vision of the file, which starts at Standard. Change it to save a file with the Deuteranopia, Protanopia, Tritanopia, or Grayscale colors, whatever your screen shows.")),
          help_section("Which file type should I pick?", open = TRUE,
            p("PNG, JPEG, and TIFF are pictures, and you choose a resolution of 150, 300, or 600 dots per inch, or Maximum. PDF, SVG, and EPS are vector files that stay sharp at any size, so their Resolution menu offers only Vector. Use PNG for screens and PDF or SVG for print shops and design tools.")),
          help_section("What is Print size?",
            p("The sheet size of the download, from 8 by 10 inches up to 24 by 30. Type and lines scale with the sheet. Very large sizes at high resolution are held to a file size that opens reliably, and the app says so when it does that.")),
          help_section("How do I share a map?",
            p("On Current maps, draw the map, then copy the link under Share. Opening it redraws the same place, map type, look, and color vision choice for another person.")),
          help_section("Can I compare places?",
            p("Yes. Open Compare places near the bottom of the Current maps sidebar, type up to three other places separated by semicolons, or add a well-known planned street grid. The places are drawn side by side at the same radius with the numbers underneath."))),
        help_topic("Privacy and problems",
          help_section("Where does my data go?", open = TRUE,
            p("Only what a lookup needs. The text you type in the place box goes to Photon, an OpenStreetMap search service, as you type, unless you turn suggestions off in Settings. A U.S. address with a city and state also goes to the Census Bureau. The streets come from OpenStreetMap through the Overpass service. The Population and Housing maps ask the Census Bureau for the block groups inside the map, which sends only the map's coordinates and not the text you typed. Nothing is stored on a server, and there are no accounts."),
            p("Remember my settings keeps your choices in this browser only. Forget saved settings removes them.")),
          help_section("The map says no streets were found, or it is slow.",
            p("The free Overpass servers are sometimes busy, so the app tries several in turn. Wait a minute and press Draw map again. A smaller radius is faster, and so is leaving Include every street off.")),
          help_section("What does the keyboard do?",
            p("Tab moves through every control in order, and focus is always outlined. Enter or Space presses a button. Esc leaves Expand.")),
          help_section("I found a mistake in a map.",
            p("Most gaps come from OpenStreetMap itself, and you can fix them there for everyone. Problems with Overmaps can be reported at ", ext(repo, "github.com/drabhikroy/overmaps"), "."))),
        help_topic("Sources and credits",
          tags$ul(class = "credit-list",
            tags$li(tags$strong("Current maps and ancient city streets. "), "Map data © ", ext("https://www.openstreetmap.org/copyright", "OpenStreetMap contributors"), " (ODbL). Places are found with Photon, and U.S. addresses with the U.S. Census Bureau geocoder."),
            tags$li(tags$strong("Population and Housing. "), "Residents and housing units by block group from the 2020 Census, U.S. Census Bureau, through its TIGERweb service. Public data of the U.S. government."),
            tags$li(tags$strong("Roman roads. "), ext("https://itiner-e.org", "Itiner-e"), " (CC BY 4.0). Travel times from Tristan Hotham, ", tags$em("By Road and by Sea"), " (CC BY 4.0). Simplified for display."),
            tags$li(tags$strong("Egyptian desert routes. "), "Manière, Crépy and Redon (2020), Desert Networks project, CNRS, ", ext("https://doi.org/10.5281/zenodo.4291876", "doi:10.5281/zenodo.4291876"), " (CC BY 4.0). Simplified for display."),
            tags$li(tags$strong("Icons. "), "The two icons on Home are from the Noto Emoji set by Google (Apache 2.0), found through ", ext("https://iconify.design", "Iconify"), "."),
            tags$li(tags$strong("Type. "), "Oswald and Fira Sans, both under the SIL Open Font License.")))),
      tags$section(class = "help-about", tags$h2("About Overmaps"),
        tags$dl(class = "about-list",
          div(tags$dt("Version"), tags$dd(OVERMAPS_VERSION)),
          div(tags$dt("Newest release"), tags$dd(ext(paste0(repo, "/releases/latest"), "github.com/drabhikroy/overmaps/releases/latest"))),
          div(tags$dt("License"), tags$dd(ext("https://polyformproject.org/licenses/noncommercial/1.0.0", "PolyForm Noncommercial License 1.0.0"), tags$br(), "Free for personal use, study, teaching, research, and nonprofit and government work. Commercial use needs a separate license.")),
          div(tags$dt("Copyright"), tags$dd("Copyright 2026 Abhik Roy.")))))
}
cvd_select <- function() div(class = "cvd-pick", selectInput("cvd", "Color vision", choices = cvd_modes, selected = "standard", selectize = FALSE, width = "100%"))

# Settings holds the choices that belong to the whole app rather than to one map: how the page looks, how it is recolored for color vision, what is sent to
# the place-search services, what is remembered on this device, and the download counts.
settings_page <- function() {
  div(class = "settings-page",
    h1("Settings"),
    p(class = "settings-intro", "Choices that apply to the whole app. They take effect at once, and nothing here is sent anywhere except the place lookups described under Places and privacy."),
    div(class = "set-card",
        h2("Appearance"),
        p(class = "fine", "The Light or Dark button at the right end of the header switches the page. The first visit follows your computer's setting, and the choice is kept in this browser.")),
    div(class = "set-card",
        h2("Color vision"),
        p(class = "fine", "Recolors the whole app and every map on your screen for deuteranopia, protanopia, tritanopia, or grayscale. Each Download section has its own Color vision of the file menu, which starts at Standard."),
        cvd_select()),
    div(class = "set-card",
        h2("Places and privacy"),
        input_switch("suggest", "Suggest places and addresses as I type", value = TRUE),
        p(class = "fine", "Suggestions come from Photon, a free OpenStreetMap search service. When you type a U.S. street address with a city and state, the U.S. Census Bureau address file is asked too. Only the text typed so far is sent, and only while this is on."),
        div(class = "remember",
            input_switch("remember", "Remember my settings on this device", value = FALSE),
            p(class = "fine", "Keeps the place, radius, map type, style, and switches in this browser so they are filled in next time. Nothing is sent anywhere, and the address is never saved. ",
              actionLink("forget", "Forget saved settings"), "."))),
    div(class = "set-card", h2("Download counts"), uiOutput("counts")))
}

ui <- function(request) {
  page_navbar(
    title = div(class = "brand", brand_mark,
                span(class = "brand-name", "Overmaps"),
                span(class = "brand-tag", "Geek out on streets")),
    id = "page", selected = "home", fillable = "current",
    window_title = "Overmaps",
    theme = get_theme(),
    header = tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"), tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32.png"),
                       tags$link(rel = "shortcut icon", href = "favicon.ico"), tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
                       tags$style(HTML(app_css)), tags$script(HTML(app_js))),
    nav_panel("Home", value = "home", home_page()),
    nav_panel("Current maps", value = "current", layout_sidebar(fillable = TRUE,
    sidebar = sidebar(
      id = "sb", width = 360, resizable = TRUE,
      open = list(desktop = "open", mobile = "always-above"),
      section(1, "Choose a place",
              div(class = "find-row",
                  textInput("query", tags$span(class = "visually-hidden", "Place"), placeholder = "Town, city, or street address", width = "100%"),
                  actionButton("find", "Find", class = "btn-outline-primary")),
              uiOutput("place_sugg"),
              conditionalPanel("input.suggest", p(class = "fine", "Suggestions use Photon, an OpenStreetMap service, and for U.S. addresses the Census Bureau. See Settings in the header.")),
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
                               input_switch("labels", "Label through routes", value = TRUE)),
              conditionalPanel("input.view == 'density'",
                               input_switch("relief", "Raise the density into spikes", value = FALSE))),
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
            download_selects("format", "res", div(class = "span-2", selectInput("print_size", "Print size", choices = print_size_labels, selected = "8x10", selectize = FALSE)), "dl_cvd"),
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
        accordion_panel("About this map",
          p(class = "fine", "Every map type is drawn from how the community has tagged streets in OpenStreetMap, not from measured traffic. ",
            "Speed limits in particular are patchy in many places, and the map says how much is missing. ",
            "The city limit is the boundary mapped in OpenStreetMap, which can differ from the legal one. The circle view dims everything outside it; ",
            "\"City limits only\" draws just the inside, up to ", max_city_km2, " square kilometers. Without a boundary, or for a larger place, the map is a plain circle. Large areas in dense cities can be slow."),
          p(class = "fine", source_current),
          p(class = "fine", paste0("Overmaps ", OVERMAPS_VERSION, ". Copyright Abhik Roy. Released under the "),
            tags$a(href = "https://polyformproject.org/licenses/noncommercial/1.0.0", target = "_blank", rel = "noopener", "PolyForm Noncommercial License 1.0.0"),
            ". The newest version is at ", tags$a(href = "https://github.com/drabhikroy/overmaps/releases/latest", target = "_blank", rel = "noopener", "github.com/drabhikroy/overmaps/releases/latest"), "."))
      )
    ),
    div(class = "stage", style = "height: 100%; min-height: 0;",
        uiOutput("stats", class = "stats-slot", fill = FALSE),
        card(class = "map-card", full_screen = TRUE,
             card_body(fillable = TRUE, imageOutput("map", width = "100%", height = "100%", fill = TRUE)))))),
    nav_panel("Historical maps", value = "history", history_page()),
    nav_spacer(),
    nav_item(class = "tools-item",
             tags$button(type = "button", id = "expand-btn", class = "nav-link nav-btn expand-btn", onclick = "streetMapsToggleExpand()",
                         `aria-pressed` = "false", title = "Give the map the whole window (Esc to go back)",
                         span(class = "when-normal", icon("expand"), " Expand"), span(class = "when-expanded", icon("compress"), " Exit expand"))),
    nav_panel(title = tagList(icon("gear"), " Settings"), value = "settings", settings_page()),
    nav_panel(title = tagList(icon("circle-question"), " Help"), value = "help", help_page()),
    nav_item(tags$button(type = "button", id = "theme-btn", class = "nav-link nav-btn theme-btn", onclick = "overmapsToggleTheme()", `aria-pressed` = "false",
                         title = "Switch between the light and dark page",
                         span(class = "when-light", icon("circle-half-stroke"), " Dark"), span(class = "when-dark", icon("circle-half-stroke"), " Light")))
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
          legend.key.width = unit(1.1, "cm"), legend.margin = margin(18, 0, 0, 0))
  p + patchwork::plot_annotation(
    title = "Roads of the Roman Empire", subtitle = paste0(if (d$region == "empire") "" else paste0(roman_regions[[d$region]]$label, ". "), roman_words(d)), caption = roman_caption(d),
    theme = theme(
      plot.title = element_textbox_simple(width = text_box_w, hjust = 0.5, family = head_font, size = 38, color = pal$ink, margin = margin(0, 0, 22, 0)),
      plot.subtitle = element_textbox_simple(width = text_box_w, hjust = 0.5, family = body_font, size = 13, color = pal$muted, lineheight = 1.3, margin = margin(0, 0, 28, 0)),
      plot.caption = element_textbox_simple(width = text_box_w, hjust = 0.5, family = body_font, size = 8.5, color = pal$faint, lineheight = 1.3, margin = margin(18, 0, 0, 0)),
      plot.margin = margin(30, 34, 22, 34), plot.background = element_rect(fill = pal$bg, color = NA)))
}
roman_alt <- function(d) paste0("Map of Roman roads ", roman_in(d), ", colored by ", tolower(roman_views[[d$view]]), ". ", roman_words(d))

# ---- Explore history: modeled caravan routes of Egypt's Eastern Desert ----------------------------------------------------------
# Unlike the Roman atlas, these are not roads that anyone found. A research team (Manière, Crépy and Redon, CNRS, 2020; CC BY 4.0, Zenodo
# 10.5281/zenodo.4291876) computed the cheapest camel route between 457 known archaeological sites and watering places for the Ptolemaic and Roman periods,
# from terrain, slope and how easy it is to find the way. Every map and download here says so. The file (data/eastern_desert_routes.rds) is built by data-raw/build_eastern_desert.R.
desert_roads <- function() {
  if (!is.null(history_cache$desert)) return(history_cache$desert)
  f <- history_file("eastern_desert_routes.rds"); if (is.null(f)) return(NULL)
  x <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.null(x) && !(is.list(x) && all(c("routes", "sites") %in% names(x)) && all(c("cost_m", "km") %in% names(x$routes)))) x <- NULL
  history_cache$desert <- x; x
}
desert_breaks <- c(1.25, 1.5, 2, 4)
desert_labels <- c("Under 1.25", "1.25 to 1.5", "1.5 to 2", "2 to 4", "4 or more")
desert_crs <- "+proj=laea +lat_0=25.4 +lon_0=33.3 +datum=WGS84 +units=m"
desert_data <- function() {
  x <- desert_roads(); if (is.null(x) || nrow(x$routes) == 0) return(NULL)
  r <- st_transform(x$routes, desert_crs); r$cls <- factor(desert_labels[findInterval(r$cost_m, desert_breaks) + 1], desert_labels)
  r <- r[order(-r$cost_m), ]                                   # the roughest ground first, so the easy corridors lie on top
  tot <- sum(r$km); share <- function(i) 100 * sum(r$km[i]) / tot
  list(routes = r, sites = st_transform(x$sites, desert_crs), crs = desert_crs, total_km = tot, n_sites = nrow(x$sites), n_water = sum(x$sites$type == "watering place"),
       easy_pct = share(r$cost_m < 1.5), rough_pct = share(r$cost_m >= 2))
}
# What the authors measured when they checked the method against travelers' routes they had held back (48 stretches, about 1,100 km), from their own
# validation spreadsheet in the same Zenodo record. The figures are theirs; the check is against 18th and 19th century travelers, not against ancient roads.
desert_check <- paste0("The authors tested the method on 48 stretches of 18th and 19th century travelers' routes that they had held back, about 1,100 km in all. ",
  "The computed paths ran 7.5% longer overall, and the area between a computed path and the traveler's route averaged 0.33 km² per km of route (median 0.12), ",
  "wider on flat ground (median 0.44) than in the mountains (median 0.06). That tests the method against modern travelers. It does not test it against the ancient roads.")
desert_words <- function(d) sprintf("Computed, not found: %s km of least-cost camel routes linking %s archaeological sites and %s watering places in the Eastern Desert, Ptolemaic and Roman periods. About %s of the length is easy going and %s is rough ground. Cost is relative and has no unit: higher means harder going for a camel.",
  format(round(d$total_km), big.mark = ","), d$n_sites - d$n_water, d$n_water, roman_pct(d$easy_pct), roman_pct(d$rough_pct))
desert_caption <- function(d) paste0("Routes: Manière, Crépy and Redon, Desert Networks project, CNRS HiSoMA (2020), Least cost network data for ancient camel transportation in the Eastern Desert of Egypt, Zenodo, doi:10.5281/zenodo.4291876, CC BY 4.0, simplified for display. ",
  "These routes are the output of a model, not roads found on the ground, and no evidence beyond the sites they join places any of them in antiquity. ", desert_check)
desert_map <- function(d, style = "classic") {
  pal <- get_style(style); r <- d$routes; lv <- levels(r$cls); present <- lv[lv %in% unique(as.character(r$cls))]
  p <- ggplot() + geom_sf(data = r, aes(color = cls), linewidth = 0.26, lineend = "round") +
    geom_sf(data = d$sites, shape = 21, size = 0.9, stroke = 0.3, fill = pal$bg, color = pal$ink) +
    scale_color_manual(values = setNames(pal$dens[2:6], lv), limits = present, name = "Relative travel cost per meter (higher is harder going)") +
    guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5, override.aes = list(linewidth = 1.6)))
  bb <- st_bbox(c(st_bbox(r)[c("xmin", "ymin")], st_bbox(r)[c("xmax", "ymax")]), crs = st_crs(r))
  w <- diff(bb[c("xmin", "xmax")]); h <- diff(bb[c("ymin", "ymax")]); target <- 0.95
  if (h / w > target) { w2 <- h / target; xl <- mean(bb[c("xmin", "xmax")]) + c(-1, 1) * w2 / 2; yl <- bb[c("ymin", "ymax")] }
  else { h2 <- w * target; yl <- mean(bb[c("ymin", "ymax")]) + c(-1, 1) * h2 / 2; xl <- bb[c("xmin", "xmax")] }
  p <- p + coord_sf(crs = d$crs, datum = NA, xlim = xl, ylim = yl, expand = TRUE) + theme_void(base_family = body_font) +
    theme(legend.position = "bottom", legend.text = element_text(size = 11, color = pal$muted, margin = margin(l = 3, r = 8)), legend.title = element_text(size = 11, color = pal$muted, margin = margin(b = 5)),
          legend.key.width = unit(1.1, "cm"), legend.margin = margin(18, 0, 0, 0))
  p + patchwork::plot_annotation(
    title = "Modeled caravan routes of Egypt's Eastern Desert", subtitle = desert_words(d), caption = desert_caption(d),
    theme = theme(
      plot.title = element_textbox_simple(width = text_box_w, hjust = 0.5, family = head_font, size = 38, color = pal$ink, margin = margin(0, 0, 22, 0)),
      plot.subtitle = element_textbox_simple(width = text_box_w, hjust = 0.5, family = body_font, size = 13, color = pal$muted, lineheight = 1.3, margin = margin(0, 0, 28, 0)),
      plot.caption = element_textbox_simple(width = text_box_w, hjust = 0.5, family = body_font, size = 8.5, color = pal$faint, lineheight = 1.3, margin = margin(20, 0, 0, 0)),
      plot.margin = margin(30, 34, 22, 34), plot.background = element_rect(fill = pal$bg, color = NA)))
}
desert_alt <- function(d) paste0("Map of computed camel routes across Egypt's Eastern Desert between ", d$n_sites, " sites and watering places, colored by relative travel cost. ", desert_words(d))
# ---- Explore history: streets of ancient cities ----------------------------------------------------------------------------------
# Real roads for other civilizations are not published as open data the way the Roman atlas is, so a city plan here is what volunteers have mapped in
# OpenStreetMap today: the excavated streets and paths of the site. That is evidence, not a reconstruction, and how much of a site is mapped varies, so
# every plan reports how many streets it found. The centers and radii frame each site; the notes are one plain sentence each.
ancient_sites <- tibble::tribble(
  ~id,           ~name,                    ~civ,                  ~lat,     ~lon,      ~radius_km, ~note,
  "pompeii",     "Pompeii, Italy",         "Roman",               40.7506,  14.4869,   0.8, "Buried by the eruption of Vesuvius in 79 CE and excavated since the 1700s.",
  "ostia",       "Ostia Antica, Italy",    "Roman",               41.7557,  12.2909,   1.0, "The port city of Rome, at the mouth of the Tiber.",
  "hadrian",     "Hadrian's Villa, Italy", "Roman",               41.9422,  12.7739,   0.7, "A vast imperial retreat near Tivoli, built by the emperor Hadrian in the 2nd century CE.",
  "timgad",      "Timgad, Algeria",        "Roman",               35.4844,   6.4686,   0.5, "A Roman colony founded around 100 CE and laid out on a grid.",
  "volubilis",   "Volubilis, Morocco",     "Roman",               34.0722,  -5.5547,   0.6, "A Roman provincial town on the edge of the empire, set among olive groves.",
  "dougga",      "Dougga, Tunisia",        "Roman",               36.4231,   9.2203,   0.5, "A Numidian and then Roman town on a hillside, among the best preserved in North Africa.",
  "leptis",      "Leptis Magna, Libya",    "Roman",               32.6385,  14.2900,   1.0, "A Phoenician and later Roman port city on the Mediterranean coast.",
  "sabratha",    "Sabratha, Libya",        "Roman",               32.8039,  12.4833,   0.6, "A Phoenician trading post that became a Roman port city, known for its seafront theater.",
  "jerash",      "Jerash, Jordan",         "Roman",               32.2806,  35.8914,   0.9, "A Roman city of the Decapolis, with a long colonnaded street and oval plaza.",
  "baalbek",     "Baalbek, Lebanon",       "Roman",               34.0069,  36.2039,   0.6, "A sanctuary city of Jupiter, Bacchus and Venus, with some of the largest Roman temples ever built.",
  "ephesus",     "Ephesus, Turkey",        "Greek and Roman",     37.9394,  27.3417,   1.0, "A Greek and then Roman city near the Aegean coast.",
  "cyrene",      "Cyrene, Libya",          "Greek and Roman",     32.8231,  21.8586,   1.0, "A Greek colony founded in the 7th century BCE, later a Roman city on the plateau above the sea.",
  "paestum",     "Paestum, Italy",         "Greek and Roman",     40.4222,  15.0050,   0.7, "A Greek colony of Poseidonia, with three great temples, taken over by Rome.",
  "priene",      "Priene, Turkey",         "Greek",               37.6600,  27.2970,   0.4, "A Greek city laid out on a regular grid on a hillside.",
  "delos",       "Delos, Greece",          "Greek",               37.3960,  25.2680,   0.6, "A sacred island and trading port in the Cyclades.",
  "delphi",      "Delphi, Greece",         "Greek",               38.4824,  22.5010,   0.5, "The sanctuary of Apollo and home of the oracle, on the slope of Mount Parnassus.",
  "olympia",     "Olympia, Greece",        "Greek",               37.6384,  21.6300,   0.5, "The sanctuary of Zeus and site of the ancient Olympic Games.",
  "mycenae",     "Mycenae, Greece",        "Bronze Age Aegean",           37.7308,  22.7564,   0.4, "A hilltop citadel of the Late Bronze Age, entered through the Lion Gate.",
  "knossos",     "Knossos, Greece",        "Bronze Age Aegean",              35.2980,  25.1631,   0.4, "The palace center of Bronze Age Crete.",
  "phaistos",    "Phaistos, Greece",       "Bronze Age Aegean",              35.0514,  24.8142,   0.3, "A Minoan palace in southern Crete overlooking the Messara plain.",
  "akrotiri",    "Akrotiri, Greece",       "Bronze Age Aegean",              36.3517,  25.4033,   0.3, "A Bronze Age town on Santorini, preserved under volcanic ash around 1600 BCE.",
  "troy",        "Troy, Turkey",           "Bronze Age Anatolia",           39.9575,  26.2389,   0.6, "A mound of nine cities built one over another, from about 3000 BCE, tied to the Trojan War.",
  "hattusa",     "Hattusa, Turkey",        "Bronze Age Anatolia",           40.0197,  34.6139,   1.2, "The walled capital of the Hittite Empire in the 2nd millennium BCE.",
  "babylon",     "Babylon, Iraq",          "Mesopotamia and Iran",        32.5364,  44.4210,   1.5, "The city of Hammurabi and Nebuchadnezzar II on the Euphrates, with the Ishtar Gate and Processional Way.",
  "uruk",        "Uruk, Iraq",             "Mesopotamia and Iran",        31.3222,  45.6361,   1.5, "One of the first cities in the world, a Sumerian center from about 4000 BCE.",
  "ur",          "Ur, Iraq",               "Mesopotamia and Iran",        30.9626,  46.1031,   0.5, "A Sumerian city centered on its great ziggurat, built around 2100 BCE.",
  "persepolis",  "Persepolis, Iran",       "Mesopotamia and Iran",             29.9356,  52.8915,   0.6, "The ceremonial capital of the Achaemenid Empire, begun around 518 BCE.",
  "pasargadae",  "Pasargadae, Iran",       "Mesopotamia and Iran",             30.1939,  53.1672,   1.0, "The first capital of the Achaemenid Empire, founded by Cyrus the Great.",
  "merv",        "Merv, Turkmenistan",     "Caravan cities",           37.6636,  62.1910,   2.0, "A great oasis city on the Silk Road, made of several walled cities built over two thousand years.",
  "petra",       "Petra, Jordan",          "Caravan cities",           30.3285,  35.4444,   1.2, "The Nabataean capital, cut into rock around a colonnaded street.",
  "palmyra",     "Palmyra, Syria",         "Caravan cities",           34.5504,  38.2684,   1.0, "A caravan city in the Syrian desert with a long colonnaded main street.",
  "giza",        "Giza, Egypt",            "Egypt and Nubia",            29.9765,  31.1325,   1.5, "The pyramid plateau of the Old Kingdom, with the pyramids of Khufu, Khafre and Menkaure.",
  "amarna",      "Amarna, Egypt",          "Egypt and Nubia",            27.6430,  30.8960,   2.0, "The short-lived capital of the pharaoh Akhenaten, built on a bare desert plain around 1346 BCE.",
  "meroe",       "Meroë, Sudan",           "Egypt and Nubia",             16.9378,  33.7497,   1.5, "The later capital of the Kingdom of Kush, with fields of small steep pyramids.",
  "great_zim",   "Great Zimbabwe, Zimbabwe", "Kingdom of Zimbabwe", -20.2675, 30.9335,   1.0, "A stone-walled capital of the 11th to 15th centuries, the largest ancient stone site south of the Sahara.",
  "harappa",     "Harappa, Pakistan",      "Indus Valley",        30.6289,  72.8639,   0.8, "The first Indus city to be excavated, on a dry bed of the Ravi River, occupied from about 2600 BCE.",
  "mohenjo",     "Mohenjo-daro, Pakistan", "Indus Valley",        27.3292,  68.1389,   0.8, "A city of the Indus Valley civilization from the third millennium BCE.",
  "dholavira",   "Dholavira, India",       "Indus Valley",        23.8886,  70.2136,   0.6, "A walled Indus city in the Kutch desert, known for its stone-lined reservoirs.",
  "lothal",      "Lothal, India",          "Indus Valley",        22.5214,  72.2494,   0.5, "An Indus port town with a brick basin that may have been a dock, near the Gulf of Khambhat.",
  "kalibangan",  "Kalibangan, India",      "Indus Valley",        29.4742,  74.1303,   0.6, "An Indus town on the Ghaggar plain with a gridded lower town and a walled citadel.",
  "rakhigarhi",  "Rakhigarhi, India",      "Indus Valley",        29.2931,  76.1142,   1.0, "One of the largest Indus sites, a cluster of mounds in a village in Haryana.",
  "nalanda",     "Nalanda, India",         "South Asia",      25.1361,  85.4430,   0.6, "A great Buddhist monastic university, active from the 5th to the 12th century CE.",
  "hampi",       "Hampi, India",           "South Asia",        15.3350,  76.4600,   1.5, "The capital of the Vijayanagara Empire, a temple city among boulders on the Tungabhadra River.",
  "polonnaruwa", "Polonnaruwa, Sri Lanka", "South Asia",            7.9403,  81.0188,   1.2, "The medieval capital of Sri Lanka, with palaces, monasteries and a large reservoir.",
  "sigiriya",    "Sigiriya, Sri Lanka",    "South Asia",            7.9570,  80.7603,   0.8, "A royal citadel on a rock 200 meters high, with water gardens, built in the 5th century CE.",
  "bagan",       "Bagan, Myanmar",         "Southeast Asia",       21.1710,  94.8590,   2.0, "The capital of the Pagan Kingdom, with thousands of brick temples built from the 11th to the 13th century.",
  "sukhothai",   "Sukhothai, Thailand",    "Southeast Asia",                17.0178,  99.7036,   1.2, "The first capital of a Thai kingdom, founded in the 13th century.",
  "angkor_thom", "Angkor Thom, Cambodia",  "Southeast Asia",               13.4412, 103.8590,   1.8, "The walled Khmer capital founded in the late 12th century, centered on the Bayon.",
  "teotihuacan", "Teotihuacan, Mexico",    "Mesoamerica",         19.6925, -98.8438,   2.5, "A great city of central Mexico, laid out along the Avenue of the Dead.",
  "monte_alban", "Monte Albán, Mexico",    "Mesoamerica",             17.0431, -96.7676,   0.6, "A Zapotec capital on a leveled mountaintop, occupied for more than a thousand years.",
  "tikal",       "Tikal, Guatemala",       "Maya",                17.2220, -89.6237,   1.5, "A Maya city in the rainforest, linked by raised causeways.",
  "chichen",     "Chichén Itzá, Mexico",   "Maya",                20.6843, -88.5678,   1.0, "A major Maya and Toltec city in Yucatán, with the stepped pyramid of El Castillo.",
  "uxmal",       "Uxmal, Mexico",          "Maya",                20.3597, -89.7710,   0.7, "A Maya city of the Puuc region, known for its long, richly carved buildings.",
  "tulum",       "Tulum, Mexico",          "Maya",                20.2114, -87.4654,   0.4, "A walled Maya port city on a cliff above the Caribbean.",
  "palenque",    "Palenque, Mexico",       "Maya",                17.4838, -92.0463,   0.6, "A Maya city of the 7th century CE in the rainforest foothills of Chiapas.",
  "copan",       "Copán, Honduras",        "Maya",                14.8400, -89.1422,   0.6, "A Maya city of the classic period, known for its carved stelae and hieroglyphic stairway.",
  "cahokia",     "Cahokia, United States", "Mississippian",       38.6551, -90.0619,   1.5, "The largest city north of Mexico before 1492, built around Monks Mound near the Mississippi.",
  "caral",       "Caral, Peru",            "Andean",        -10.8933, -77.5200,   0.8, "One of the oldest cities in the Americas, with large platform mounds, built about 2600 BCE.",
  "chan_chan",   "Chan Chan, Peru",        "Andean",               -8.1058, -79.0744,   1.2, "The adobe capital of the Chimú kingdom, made of great walled compounds on the Peruvian coast.",
  "tiwanaku",    "Tiwanaku, Bolivia",      "Andean",           -16.5545, -68.6736,   0.8, "A ceremonial and political center on the high Andean plateau, flourishing from about 500 to 1000 CE.",
  "ollanta",     "Ollantaytambo, Peru",    "Inca",               -13.2586, -72.2640,   0.5, "An Inca town still lived in today, laid out on its original grid of narrow stone lanes.",
  "machu",       "Machu Picchu, Peru",     "Inca",               -13.1631, -72.5450,   0.6, "An Inca royal estate in the Andes, built on terraces in the 15th century.",
  "gobekli",     "Göbekli Tepe, Turkey",   "Taş Tepeler",         37.2236,  38.9217,   0.5, "A hilltop of monumental stone enclosures from about 9500 BCE, built before towns or streets. Only the visitor paths of today are mapped.",
  "karahan",     "Karahan Tepe, Turkey",   "Taş Tepeler",         37.0925,  39.3036,   0.5, "A hilltop settlement of rock-cut buildings from about 9500 BCE, built before streets existed. Only the tracks of today are mapped.",
  "carthage", "Carthage, Tunisia", "Roman", 36.8530, 10.3226, 1.2, "The Phoenician capital that fought Rome, rebuilt by the Romans on the hills above the bay of Tunis.",
  "herculaneum", "Herculaneum, Italy", "Roman", 40.8060, 14.3480, 0.4, "A small seaside town buried with Pompeii in 79 CE, so well sealed that wooden beams survived.",
  "italica", "Italica, Spain", "Roman", 37.4381, -6.0453, 0.6, "A Roman town in Andalusia, birthplace of the emperors Trajan and Hadrian, with one of the largest amphitheaters.",
  "pergamon", "Pergamon, Turkey", "Greek and Roman", 39.1329, 27.1845, 1.0, "A hilltop capital of Hellenistic kings, famous for its steep theater and its library.",
  "hierapolis", "Hierapolis, Turkey", "Greek and Roman", 37.9244, 29.1261, 1.0, "A spa city built beside white mineral terraces, with a long colonnaded main street.",
  "aphrodisias", "Aphrodisias, Turkey", "Greek and Roman", 37.7086, 28.7233, 0.7, "A city of sculptors, known for its marble and a nearly intact stadium.",
  "perge", "Perge, Turkey", "Greek and Roman", 36.9610, 30.8540, 0.7, "A walled city near the southern coast with a colonnaded street and a water channel down its middle.",
  "susa", "Susa, Iran", "Mesopotamia and Iran", 32.1900, 48.2580, 1.2, "One of the oldest cities on Earth and a winter capital of the Persian kings.",
  "ctesiphon", "Ctesiphon, Iraq", "Mesopotamia and Iran", 33.0936, 44.5808, 1.0, "The Parthian and Sasanian capital on the Tigris, known for its vast brick arch.",
  "ashur", "Ashur, Iraq", "Mesopotamia and Iran", 35.4561, 43.2594, 0.8, "The first capital of Assyria and home of its god, on a bluff above the Tigris.",
  "mari", "Mari, Syria", "Mesopotamia and Iran", 34.5498, 40.8890, 0.6, "A Bronze Age city on the Euphrates whose palace held thousands of clay tablets.",
  "ugarit", "Ugarit, Syria", "Levant", 35.6019, 35.7864, 0.5, "A Bronze Age port where one of the earliest alphabets was written.",
  "byblos", "Byblos, Lebanon", "Levant", 34.1200, 35.6480, 0.5, "A Phoenician port that traded cedar with Egypt and lent its name to the word for book.",
  "jericho", "Jericho, Palestine", "Levant", 31.8711, 35.4443, 0.5, "A walled settlement more than ten thousand years old, often called the oldest town in the world.",
  "karnak", "Karnak and Luxor, Egypt", "Egypt and Nubia", 25.7188, 32.6573, 1.2, "The temple precinct of Thebes, built and expanded over about two thousand years.",
  "taxila", "Taxila, Pakistan", "South Asia", 33.7460, 72.8190, 1.2, "A crossroads city of learning where Persian, Greek, and Indian worlds met.",
  "anuradhapura", "Anuradhapura, Sri Lanka", "South Asia", 8.3510, 80.3970, 1.5, "The first great capital of Sri Lanka, with giant brick stupas and ancient reservoirs.",
  "anyang", "Anyang, China", "East Asia", 36.1270, 114.3150, 1.2, "Yin Xu, the last capital of the Shang dynasty, where the oldest Chinese writing was found on oracle bones.",
  "nara", "Heijo Palace, Nara, Japan", "East Asia", 34.6914, 135.7967, 1.0, "The capital of Japan in the 8th century, laid out on a grid modeled on Chang'an.",
  "calakmul", "Calakmul, Mexico", "Maya", 18.1058, -89.8111, 1.0, "A rival of Tikal, set deep in the forest and linked to other cities by raised causeways.",
  "pachacamac", "Pachacamac, Peru", "Andean", -12.2594, -76.9010, 0.7, "A sacred oracle city on the Pacific coast, kept in use for more than a thousand years.",
  "chaco", "Chaco Canyon, United States", "Ancestral Puebloan", 36.0605, -107.9608, 0.8, "Great houses of stone connected by straight roads, built between about 850 and 1150 CE.",
  "mesaverde", "Mesa Verde, United States", "Ancestral Puebloan", 37.1670, -108.4620, 0.5, "Villages built into the cliffs of canyon walls in the 12th and 13th centuries.",
  "aksum", "Aksum, Ethiopia", "East Africa", 14.1301, 38.7190, 0.8, "The capital of a trading empire that raised towering carved stone stelae.",
  "lalibela", "Lalibela, Ethiopia", "East Africa", 12.0317, 39.0473, 0.5, "Churches cut downward out of solid rock in the 12th century.",
  "kilwa", "Kilwa Kisiwani, Tanzania", "East Africa", -8.9581, 39.5160, 0.5, "A Swahili island port that grew rich on gold and trade across the Indian Ocean."
)
# Published population estimates for the few sites where scholars have made one. These are estimates, not counts, and they differ a great deal between studies,
# so each is shown as the authors gave it, with the period it describes and where it came from. A site with no entry has no estimate here, because only a
# citable published figure is used. The full references are in data/README.md.
ancient_estimates <- tibble::tribble(
  ~id,           ~est,                                                                                               ~when,                                       ~who,
  "ostia",       "between 22,000 and 58,000 people (published estimates range this widely)",                         "in the 2nd century CE",                    "Storey (1997), Packer (1967), Calza (1941) and Meiggs (1973)",
  "knossos",     "about 25,000 people",                                                                              "in the Neopalatial period, the Late Bronze Age", "Cutler and Whitelaw (2019)",
  "teotihuacan", "about 100,000 people (earlier estimates run from 75,000 to over 200,000)",                         "in the Xolalpan and Metepec phases, about 350 to 650 CE", "Smith and others (2019)",
  "angkor_thom", "700,000 to 900,000 people across the whole Greater Angkor Region, about 3,000 square kilometers, not the walled city alone", "at its height in the 13th century CE", "Klassen and others (2021)",
  "great_zim",   "no more than 10,000 people (an older estimate is 18,000)",                                         "at its peak",                              "Chirikure and others (2017); the older figure is Huffman's, as reported by Pikirayi (2013)"
)
city_estimate <- function(id) {
  r <- ancient_estimates[ancient_estimates$id == as.character(id %||% "")[1], ]
  if (nrow(r) != 1) NULL else as.list(r)
}
city_estimate_text <- function(e) paste0("Estimated population ", e$when, ": ", e$est, ". Source: ", e$who, ". An estimate, not a count.")

ancient_site_choices <- lapply(split(ancient_sites, factor(ancient_sites$civ, levels = unique(ancient_sites$civ))), function(d) setNames(d$id, d$name))
# The starting radius for a site is fitted to what is mapped there: the distance that holds almost all of the saved streets, plus a small margin, so the circle
# ends just past the edge of the remains instead of at a guess. A few stray paths far out would stretch the circle, so the farthest 2% of points are ignored.
# The streets are sampled every 25 meters so long roads count by their length. Too little mapped, streets that run to the edge of the snapshot, or no snapshot, keeps the radius written in the site list.
fit_radius_km <- function(lines, lon, lat, saved_m = Inf) {
  if (is.null(lines) || !inherits(lines, "sf") || nrow(lines) < 5) return(NA_real_)
  crs <- utm_epsg(lon, lat)
  xy <- tryCatch({
    g <- st_transform(st_geometry(lines), crs); g <- g[!st_is_empty(g)]
    cen <- st_coordinates(st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs))[1, 1:2]
    p <- st_coordinates(st_segmentize(g, 25))[, 1:2, drop = FALSE]
    cbind(p[, 1] - cen[1], p[, 2] - cen[2])
  }, error = function(e) NULL)
  if (is.null(xy) || nrow(xy) < 20) return(NA_real_)
  d <- sqrt(xy[, 1]^2 + xy[, 2]^2); r_m <- unname(stats::quantile(d, 0.98)) * 1.04 + 50
  # Streets that run right out to the edge of what was saved (a modern town around the ruins) say nothing about where the remains end, so keep the listed radius.
  if (is.finite(saved_m) && r_m >= 0.9 * saved_m) return(NA_real_)
  min(max(0.3, ceiling(r_m / 50) / 20), max(0.3, saved_m / 1000))                      # whole 0.05 km steps, never below 0.3 or past what was saved
}
site_fit_km <- function(site) {
  hit <- ancient_bundle()[[as.character(site$id)]]
  if (!is.list(hit) || !is.list(hit$roads)) return(NA_real_)
  l <- hit$roads$osm_lines; key <- paste(site$id, hit$taken, hit$radius_m, if (is.null(l)) 0L else nrow(l), as.numeric(utils::object.size(l)))
  cached <- history_cache$fit[[site$id]]
  if (!is.null(cached) && identical(cached$key, key)) return(cached$km)
  km <- fit_radius_km(l, site$lon, site$lat, hit$radius_m)
  if (is.null(history_cache$fit)) history_cache$fit <- list()
  history_cache$fit[[site$id]] <- list(key = key, km = km); km
}
ancient_site <- function(id) {
  r <- ancient_sites[ancient_sites$id == as.character(id %||% "")[1], ]; if (nrow(r) != 1) return(NULL)
  s <- as.list(r); fit <- site_fit_km(s)
  if (is.finite(fit)) { s$listed_radius_km <- s$radius_km; s$radius_km <- fit }
  s
}
# A plan reads the way the main map types do, without the road hierarchy, which means nothing for streets that predate the idea of road classes.
city_views <- c(direction = "Direction", blocks = "Block length", density = "Street density", nature = "Nature & water", lights = "Street lights")
city_view_blurbs <- c(direction = "Which way the streets run, with a compass rose. Grids show as two colors.",
                      blocks = "Distance between junctions: short, even blocks mean a planned grid.",
                      density = "Street length per square kilometer, in hexagons.",
                      nature = "The water, woods, and parks around the site as they are mapped today. Rivers and shores have moved since ancient times.",
                      lights = "The lamps and lit streets mapped at the site today, so a view of the modern place at night.")
# Street lights is offered only where the saved snapshot found enough lamps or lit streets to draw; elsewhere it would be an empty map.
city_min_lights <- 10L
city_has_lights <- function(id) {
  e <- ancient_bundle()[[as.character(id %||% "")[1]]]$extras
  is.list(e) && isTRUE(as.integer(e$n_lamps %||% 0L) + as.integer(e$n_lit %||% 0L) >= city_min_lights)
}
city_view_keys <- function(id) { k <- names(city_views); if (city_has_lights(id)) k else setdiff(k, "lights") }
city_view_choices <- function(id) { k <- city_view_keys(id); list(names = lapply(k, function(x) tagList(span(class = "vt", city_views[[x]]), span(class = "vb", city_view_blurbs[[x]]))), values = k) }
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
# The radius slider for a site, in kilometers: from a third of the site's own radius up to three times it (never past 5 km), starting at the site's radius.
city_radius_plan <- function(site) {
  r <- site$radius_km; step <- if (r <= 1) 0.1 else 0.25
  lo <- max(0.2, round(r / 3 / step) * step); hi <- ceiling(min(5, max(r * 3, r + 1)) / step - 1e-9) * step
  list(min = lo, max = hi, step = step, value = r)
}
# The radius to draw, in kilometers, from the slider in the chosen unit. A slider left at its starting value means the site's own radius exactly, so a
# saved snapshot is used whatever the rounding of the slider's step.
city_radius_km <- function(site, value, units = "km") {
  u <- as_unit(units); plan <- city_radius_plan(site); to_km <- if (identical(u, "mi")) km_per_mile else 1
  v <- suppressWarnings(as.numeric(value)); if (length(v) != 1 || !is.finite(v)) return(site$radius_km)
  v <- v * to_km; half <- 0.5 * radius_plan_units(plan, u)$step * to_km
  if (abs(v - site$radius_km) <= half) site$radius_km else min(plan$max, max(plan$min, v))
}
city_alt <- function(d) {
  what <- switch(d$view, nature = ", with the water, woods, and parks around them", lights = ", showing the street lamps and lit streets", paste0(", colored by ", tolower(city_views[[d$view]])))
  paste0("Map of the mapped streets of ", d$site$name, what, ". ", city_coverage(d$lines)$text)
}
# The sentence added to a drawing's caption: what the streets are, the published population estimate when the site has one, and any note for the map type.
city_note <- function(s, view) {
  e <- city_estimate(s$id)
  paste0("the streets and paths shown are the site as it is today, drawn by OpenStreetMap volunteers from what has been excavated, ",
         "so they show the surviving remains and not a reconstruction of the whole ancient city",
         if (!is.null(e)) paste0("; estimated population ", e$when, ": ", e$est, " (", e$who, "), an estimate and not a count") else "",
         if (identical(view, "nature")) "; water and shores are as mapped today, and rivers and coasts have moved since ancient times"
         else if (identical(view, "lights")) "; the lamps and lit streets are modern lighting around the site, not ancient" else "")
}
city_map <- function(d, style = "classic") {
  s <- d$site
  make_map(d$lines, s$name, s$lon, s$lat, d$radius_m %||% round(s$radius_km * 1000), d$crs, label_roads = FALSE, style = style, view = d$view, show_local = TRUE, detail = "all",
           units = d$units %||% "km", relief = isTRUE(d$relief) && identical(d$view, "density"), lights = d$lights, nature = d$nature,
           note = city_note(s, d$view))
}

# A snapshot of the OpenStreetMap streets at each site, built once by data-raw/fetch_ancient_cities.R and kept in data/ancient_cities.rds.
# A site found there is drawn at once with no download, which matters because the public servers are often busy and the global ones are the only
# choice for most sites. A radius up to the saved one is cut from the snapshot; a site missing from the file, or a larger radius, is downloaded live.
ancient_bundle <- function() {
  if (!is.null(history_cache$cities)) return(history_cache$cities)
  f <- history_file("ancient_cities.rds")
  b <- if (is.null(f)) list() else tryCatch(readRDS(f), error = function(e) list())
  history_cache$cities <- if (is.list(b)) b else list()
  history_cache$cities
}
bundled_city <- function(site, radius_m) {
  hit <- ancient_bundle()[[as.character(site$id)]]
  if (is.list(hit) && is.list(hit$roads) && isTRUE(as.integer(hit$radius_m) >= as.integer(radius_m))) hit else NULL     # a smaller radius is cut from the saved streets
}

# The saved nature, water and lighting for a site, when the snapshot has them for a radius at least as large as the one asked for.
bundled_extras <- function(site, radius_m) {
  e <- ancient_bundle()[[as.character(site$id)]]$extras
  if (is.list(e) && is.list(e$nature) && isTRUE(as.integer(e$radius_m) >= as.integer(radius_m))) e else NULL
}
# Lit streets in the snapshot carry no lit tag (only their id is kept), so the tag is put back from the saved list of lit ids.
with_lit_ids <- function(lines, ids) { if (!is.null(lines) && length(ids) > 0 && "osm_id" %in% names(lines)) lines$lit_tag <- lines$lit_tag | as.character(lines$osm_id) %in% ids; lines }
get_city_view <- function(x) { x <- as.character(x %||% "direction")[1]; if (!is.na(x) && x %in% names(city_views)) x else "direction" }
# The three historical sets share one picker at the top of every sidebar, so the pages look the same whichever set is open. A click is reported to the server,
# which shows the matching set; the highlighted button is the set the sidebar belongs to.
history_sets <- c(roads = "Roman roads", desert = "Egyptian desert", cities = "Ancient cities")
set_switcher <- function(active) div(class = "set-switch", role = "group", `aria-label` = "Choose a set of historical maps",
  lapply(names(history_sets), function(k) tags$button(type = "button", class = paste("set-btn", if (identical(k, active)) "is-active"), `aria-pressed` = tolower(as.character(identical(k, active))),
                                                       onclick = sprintf("overmapsSet('%s')", k), history_sets[[k]])))
about_map <- function(...) accordion(open = FALSE, class = "more-panels", accordion_panel("About this map", ...))
history_page <- function(style = "classic") {
  have <- !is.null(roman_roads())
  roads_ui <- if (!have) tagList(set_switcher("roads"), p(class = "fine", "The road data file (data/itinere_simplified.rds) was not found next to the app, so this map cannot be drawn."))
  else layout_sidebar(class = "history-cols", fillable = FALSE,
    sidebar = sidebar(id = "h_sb", width = 360, resizable = TRUE, open = "open", title = NULL,
      set_switcher("roads"),
      p(class = "fine", "Real roads from a scholarly atlas, not a drawing: about 294,000 km of routes, drawn where the evidence, or the best guess, places them."),
      section(1, "Choose a region", selectInput("h_region", NULL, roman_region_choices, selected = "empire", selectize = FALSE)),
      section(2, "Choose a map type",
              div(class = "view-picker", radioButtons("h_view", NULL, choiceNames = roman_view_names, choiceValues = names(roman_views), selected = "kind", width = "100%")),
              conditionalPanel("input.h_view == 'days'",
                radioButtons("h_how", "How you travel", setNames(names(roman_how), roman_how), selected = "foot", inline = TRUE),
                input_switch("h_sea", "Use the sea in summer", value = FALSE))),
      section(3, "Choose a look", div(class = "style-picker", radioButtons("h_style", NULL, choiceNames = style_choice_names, choiceValues = names(styles), selected = style, inline = TRUE, width = "100%"))),
      div(class = "draw-row", actionButton("h_draw", "Draw map", icon = icon("map"), class = "btn-primary")),
      conditionalPanel("output.h_has_map == 'yes'",
        section(4, "Download",
                download_selects("h_format", "h_res", print_select("h_print"), "h_dl_cvd"),
                downloadButton("h_download", "Download", class = "btn-outline-primary"),
                uiOutput("h_print_note"))),
      about_map(p(class = "fine", source_roads, " Simplified for display."))),
    div(class = "history-map", imageOutput("h_map", width = "100%", height = "auto")))
  city0 <- city_radius_plan(ancient_site("pompeii")); cv0 <- city_view_choices("pompeii")
  cities_ui <- layout_sidebar(class = "history-cols", fillable = FALSE,
    sidebar = sidebar(id = "c_sb", width = 360, resizable = TRUE, open = "open", title = NULL,
      set_switcher("cities"),
      p(class = "fine", "The streets of ancient cities as they are mapped today. Volunteers draw the excavated remains into OpenStreetMap, so this is what survives and has been mapped, not a reconstruction. Some sites are mapped far more fully than others, and each plan says how much it found."),
      section(1, "Choose a city", selectInput("c_site", NULL, ancient_site_choices, selected = "pompeii", selectize = FALSE), uiOutput("c_note")),
      section(2, "Shape the map",
              div(class = "view-picker", radioButtons("c_view", "Map type", choiceNames = cv0$names, choiceValues = cv0$values, selected = "direction", width = "100%")),
              div(class = "radius-head", span(class = "radius-title", "Units"),
                  radioButtons("c_units", NULL, choices = c("Kilometers" = "km", "Miles" = "mi"), selected = "km", inline = TRUE, width = "auto")),
              sliderInput("c_radius", "Radius", min = city0$min, max = city0$max, value = city0$value, step = city0$step, post = " km", ticks = FALSE),
              uiOutput("c_radius_help"),
              conditionalPanel("input.c_view == 'density'", input_switch("c_relief", "Raise the density into spikes", value = FALSE))),
      section(3, "Choose a look", div(class = "style-picker", radioButtons("c_style", NULL, choiceNames = style_choice_names, choiceValues = names(styles), selected = style, inline = TRUE, width = "100%"))),
      div(class = "draw-row", actionButton("c_draw", "Draw map", icon = icon("map"), class = "btn-primary"),
          actionButton("c_reset", "Reset options", icon = icon("rotate-left"), class = "btn-outline-secondary")),
      conditionalPanel("output.c_has_map == 'yes'",
        div(class = "ctl", style = "margin-top: 1.1rem;",
            div(class = "ctl-head", span(class = "step", 4), "Download"),
            download_selects("c_format", "c_res", print_select("c_print"), "c_dl_cvd"),
            downloadButton("c_download", "Download", class = "btn-outline-primary"),
            uiOutput("c_print_note"))),
      about_map(p(class = "fine", source_cities), p(class = "fine", "Streets were saved from OpenStreetMap on the date each map shows. A larger radius than the saved one is downloaded live, which can take a little while."))),
    div(class = "history-map", uiOutput("c_status"), imageOutput("c_map", width = "100%", height = "auto")))
  desert_ui <- if (is.null(desert_roads())) tagList(set_switcher("desert"), p(class = "fine", "The route data file (data/eastern_desert_routes.rds) was not found next to the app, so this map cannot be drawn."))
  else layout_sidebar(class = "history-cols", fillable = FALSE,
    sidebar = sidebar(id = "d_sb", width = 360, resizable = TRUE, open = "open", title = NULL,
      set_switcher("desert"),
      p(class = "fine", tags$strong("Computed, not found. "), "No one has surveyed these routes. A research team asked a computer for the cheapest camel route between 457 known sites and watering places in the Eastern Desert, using terrain, slope and how easy it is to find the way, for the Ptolemaic and Roman periods."),
      p(class = "fine", desert_check),
      section(1, "Choose a look", div(class = "style-picker", radioButtons("d_style", NULL, choiceNames = style_choice_names, choiceValues = names(styles), selected = style, inline = TRUE, width = "100%"))),
      div(class = "draw-row", actionButton("d_draw", "Draw map", icon = icon("map"), class = "btn-primary")),
      conditionalPanel("output.d_has_map == 'yes'",
        section(2, "Download",
                download_selects("d_format", "d_res", print_select("d_print"), "d_dl_cvd"),
                downloadButton("d_download", "Download", class = "btn-outline-primary"),
                uiOutput("d_print_note"))),
      about_map(p(class = "fine", source_desert, " Simplified for display."))),
    div(class = "history-map", imageOutput("d_map", width = "100%", height = "auto")))
  div(class = "history-page",
      navset_hidden(id = "h_tab", nav_panel_hidden("roads", roads_ui), nav_panel_hidden("desert", desert_ui), nav_panel_hidden("cities", cities_ui)))
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
    lgt <- if (identical(get_view(view), "lights")) load_lights(lon, lat, radius_m, crs_m) else NULL
    nat <- if (identical(get_view(view), "nature")) load_nature(lon, lat, radius_m, crs_m) else NULL
    cen <- if (get_view(view) %in% c("census", "homes")) load_census(lon, lat, radius_m, crs_m) else NULL
    pl <- make_map(lines, place, lon, lat, radius_m, crs_m, label_roads = label_roads, style = style, view = view, show_local = show_local, detail = detail, center_txt = center_txt, units = units, paths = pth, lights = lgt, nature = nat, census = cen)
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
  p <- list(q = trimws(input$query %||% ""), c = choice, r = input$radius, u = as_unit(input$units), v = input$view, s = input$style, cv = if (isTRUE(input$cvd %in% cvd_modes) && input$cvd != "standard") input$cvd else NULL, a = input$area,
            l = b(input$locals, 0L), lb = b(input$labels, 1L), as = b(input$all_streets, 0L), ua = b(input$use_addr, 0L), rl = b(input$relief, 0L), ad = trimws(input$addr %||% ""))
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
  out <- list(q = txt("q"), c = num("c", 1, 50), r = num("r", 0.1, 500), u = pick("u", c("km", "mi")), v = pick("v", names(views)), s = pick("s", names(styles)), cv = pick("cv", cvd_modes),
              a = pick("a", c("dim", "city", "circle")), l = flag("l"), lb = flag("lb"), as = flag("as"), ua = flag("ua"), rl = flag("rl"), ad = txt("ad"), go = identical(x$go, "1"))
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
    pu <- radius_plan_units(plan, u, cover = is.null(value_km))       # a fresh start must still cover the place when shown in miles
    updateSliderInput(session, "radius", min = pu$min, max = pu$max, value = pu$value, step = pu$step)
    session$sendCustomMessage("radiusUnit", u)
  }
  observe({
    p <- places(); i <- selected()
    bb <- place_bbox(p, i)
    enc <- tryCatch(enclosing_radius_m(if ("boundary" %in% names(p)) p$boundary[[i]] else NULL, p$lon[i], p$lat[i], bb), error = function(e) NA_real_)
    plan <- radius_plan(if (is.finite(enc)) enc + max(150, 0.03 * enc) else enc)       # a little past the limits: the map circle is drawn 1.2% inside its radius
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
    if (!is.null(st$cv)) updateSelectInput(session, "cvd", selected = st$cv)
    if (!is.null(st$a)) updateRadioButtons(session, "area", selected = st$a)
    for (k in c("l", "lb", "as", "ua", "rl")) if (!is.null(st[[k]])) bslib::update_switch(c(l = "locals", lb = "labels", as = "all_streets", ua = "use_addr", rl = "relief")[[k]], value = st[[k]])
    if (!is.null(st$ad)) updateTextInput(session, "addr", value = st$ad)
    unit_prev(as_unit(st$u))                                   # the units were set by the link, not by a click, so nothing should be converted
    restore$pending <- st; restore$q <- st$q; auto_find(isolate(auto_find()) + 1)
  }
  observeEvent(session$clientData$url_search, {
    st <- parse_share(session$clientData$url_search)
    if (is.null(st) || !is.null(isolate(restore$pending))) return()
    nav_select("page", "current")
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
    for (k in c("locals", "all_streets", "use_addr", "inspect", "relief")) bslib::update_switch(k, value = FALSE)
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
      footer = tagList(div(class = "cmp-res", selectInput("cmp_res", "Picture resolution", choices = resolution_choices, selected = "300", selectize = FALSE)), downloadButton("cmp_dl", "Download picture (PNG)", class = "btn-outline-primary"), downloadButton("cmp_pdf", "Download PDF", class = "btn-outline-primary"), modalButton("Close"))))
  })
  cmp_file <- function(ext) function() { r <- cmp(); paste0("compare_", paste(vapply(r$places, function(p) file_slug(p$place), ""), collapse = "_"), if (ext == "png") res_suffix("png", input$cmp_res), "_", format(Sys.Date(), "%Y%m%d"), ".", ext) }
  output$cmp_dl <- downloadHandler(filename = cmp_file("png"), content = function(file) { r <- cmp(); req(r); draw_figure(r$fig, file, "png", isolate(sid()), size = r$size, res = res_pick(input$cmp_res)) })
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
  sid <- reactive(style_key(input$style, input$cvd))
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
  # Street lights needs one more download (lamps and lit walking ways), again only when that map type is chosen and kept for the place on screen.
  lights_cache <- new.env(); lights_cache$key <- NULL; lights_cache$value <- NULL
  lights_data <- reactive({
    g <- geo(); req(identical(get_view(input$view), "lights"))
    key <- paste(g$lon, g$lat, g$radius_m, if (is.null(g$shape)) "circle" else round(as.numeric(st_area(g$shape))))
    if (!identical(key, lights_cache$key)) {
      val <- withProgress(message = "Loading street lamps", value = 0.5, load_lights(g$lon, g$lat, g$radius_m, g$crs_m, g$shape))
      lights_cache$key <- key; lights_cache$value <- val
    }
    lights_cache$value
  })
  active_lights <- function() if (identical(get_view(input$view), "lights")) lights_data() else NULL
  # Nature & water also needs one more download (parks, woods, water, and the coastline), made only when that map type is chosen.
  nature_cache <- new.env(); nature_cache$key <- NULL; nature_cache$value <- NULL
  nature_data <- reactive({
    g <- geo(); req(identical(get_view(input$view), "nature"))
    key <- paste(g$lon, g$lat, g$radius_m, if (is.null(g$shape)) "circle" else round(as.numeric(st_area(g$shape))))
    if (!identical(key, nature_cache$key)) {
      val <- withProgress(message = "Loading parks and water", value = 0.5, load_nature(g$lon, g$lat, g$radius_m, g$crs_m, g$shape))
      nature_cache$key <- key; nature_cache$value <- val
    }
    nature_cache$value
  })
  active_nature <- function() if (identical(get_view(input$view), "nature")) nature_data() else NULL
  # Population needs the Census Bureau's block groups, fetched only when that map type is chosen. A failed request is not kept, so choosing the map again tries again.
  census_cache <- new.env(); census_cache$key <- NULL; census_cache$value <- NULL
  census_data <- reactive({
    g <- geo(); req(get_view(input$view) %in% c("census", "homes"))
    key <- paste(g$lon, g$lat, g$radius_m, if (is.null(g$shape)) "circle" else round(as.numeric(st_area(g$shape))))
    if (!identical(key, census_cache$key)) {
      val <- withProgress(message = "Loading Census population", value = 0.5, load_census(g$lon, g$lat, g$radius_m, g$crs_m, g$shape))
      if (isTRUE(val$failed)) return(val)
      census_cache$key <- key; census_cache$value <- val
    }
    census_cache$value
  })
  active_census <- function() if (get_view(input$view) %in% c("census", "homes")) census_data() else NULL
  build_for <- function(style) {
    g <- geo(); pth <- active_paths(); lgt <- active_lights(); nat <- active_nature(); cen <- active_census()
    make_map(g$lines, g$place, g$lon, g$lat, g$radius_m, g$crs_m, label_roads = isTRUE(input$labels), style = style, view = input$view, units = as_unit(input$units),
             show_local = isTRUE(input$locals), shape = g$shape, inside = g$inside, detail = g$detail, center_txt = g$center_txt, paths = pth,
             relief = isTRUE(input$relief), lights = lgt, nature = nat, census = cen)
  }
  built <- reactive(build_for(sid()))
  dl_style <- reactive(style_key(input$style, input$dl_cvd %||% "standard"))      # the file's own color vision, Standard unless the person changes it
  # Numbers for the tiles and the alt text; recomputed when the map type changes, never re-downloaded.
  vdata <- reactive({
    g <- geo()
    view_data(input$view, g$lines, g$radius_m, center_of(g$lon, g$lat, g$crs_m), g$shape, g$detail, g$center_txt, as_unit(input$units), g$net, paths = active_paths(), fine = isTRUE(input$relief) && identical(input$view, "density"), lights = active_lights(), nature = active_nature(), census = active_census())
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
    fmt <- if (isTRUE(input$format %in% export_formats)) input$format else "png"
    p(class = "fine", style = "margin-top: .6rem;", print_note_text(fmt, print_key(), input$res))
  })
  # The resolution menu: dots per inch for pictures, only "Vector" for PDF, SVG and EPS. The last picture choice comes back when switching to a picture.
  sync_resolution <- function(fmt_id, res_id) {
    force(fmt_id); force(res_id)   # the loop below changes its variable; without this every observer would end up watching the last pair
    last <- "300"
    observeEvent(input[[res_id]], { if (isTRUE(input[[res_id]] %in% resolution_choices)) last <<- input[[res_id]] })
    observeEvent(input[[fmt_id]], {
      if (isTRUE(input[[fmt_id]] %in% vector_formats)) updateSelectInput(session, res_id, choices = vector_choice, selected = "vector")
      else updateSelectInput(session, res_id, choices = resolution_choices, selected = last)
    }, ignoreInit = TRUE)
  }
  for (ids in list(c("format", "res"), c("h_format", "h_res"), c("c_format", "c_res"), c("d_format", "d_res"))) sync_resolution(ids[1], ids[2])
  for (k in c("h", "c", "d")) local({
    kk <- k
    output[[paste0(kk, "_print_note")]] <- renderUI({
      fmt <- input[[paste0(kk, "_format")]]; fmt <- if (isTRUE(fmt %in% export_formats)) fmt else "png"
      p(class = "fine", style = "margin-top: .6rem;", print_note_text(fmt, print_key_of(input[[paste0(kk, "_print")]]), input[[paste0(kk, "_res")]]))
    })
  })
  output$download <- downloadHandler(
    filename = function() {
      g <- tryCatch(geo(), error = function(e) NULL)
      fmt <- if (isTRUE(input$format %in% export_formats)) input$format else "png"
      paste0("overmaps_", get_view(input$view), "_", file_slug(g$place), if (print_key() != "8x10") paste0("_", print_key()), res_suffix(fmt, input$res), "_", format(Sys.Date(), "%Y%m%d"), ".", format_ext[[fmt]])
    },
    content = function(file) {
      fmt <- if (isTRUE(input$format %in% export_formats)) input$format else "png"
      g <- geo()
      cached <- file.path(export_dir(), paste0(export_key(g$place, g$lon, g$lat, g$radius_m, g$shape, g$inside, input$view, dl_style(), print_key(),
                                                          isTRUE(input$locals), isTRUE(input$labels), g$detail, g$center_txt, as_unit(input$units), fmt, isTRUE(input$relief), if (fmt %in% raster_formats) res_pick(input$res) else "vector", "v6"), ".", format_ext[[fmt]]))
      if (!file.exists(cached)) {
        tmp <- tempfile(tmpdir = export_dir(), fileext = paste0(".", format_ext[[fmt]]))
        sty <- dl_style(); draw_figure(if (identical(sty, sid())) built() else build_for(sty), tmp, fmt, sty, print_key = print_key(), res = res_pick(input$res))
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


  # The Home page cards, and the header tabs, move between the three pages. A shared link always opens the current maps.
  observeEvent(input$go_current, nav_select("page", "current"))
  observeEvent(input$go_history, nav_select("page", "history"))
  observeEvent(input$go_help, nav_select("page", "help"))
  observeEvent(input$h_set, if (isTRUE(input$h_set %in% names(history_sets))) nav_select("h_tab", input$h_set))
  observeEvent(input$cvd, session$sendCustomMessage("cvdClass", if (isTRUE(input$cvd %in% cvd_modes)) input$cvd else "standard"))     # the whole page, not only the maps, follows the choice
  observeEvent(input$cvd, {
    nm <- style_choice_names_for(if (isTRUE(input$cvd %in% cvd_modes)) input$cvd else "standard")
    for (id in c("style", "h_style", "c_style", "d_style")) if (!is.null(input[[id]])) updateRadioButtons(session, id, choiceNames = nm, choiceValues = names(styles), selected = input[[id]], inline = TRUE)
  }, ignoreInit = TRUE)
  # Ancient cities: streets are downloaded when Draw map is pressed, then the map type and look change at once from what was downloaded.
  c_state <- reactiveVal(NULL)
  c_unit_prev <- reactiveVal("km")
  apply_city_plan <- function(site, u, value_km = NULL) {
    plan <- city_radius_plan(site)
    if (!is.null(value_km)) plan$value <- min(plan$max, max(plan$min, value_km))
    pu <- radius_plan_units(plan, u)
    updateSliderInput(session, "c_radius", min = pu$min, max = pu$max, value = pu$value, step = pu$step)
    session$sendCustomMessage("sliderPost", list(id = "c_radius", post = paste0(" ", u)))
  }
  observeEvent(input$c_site, {
    s <- ancient_site(input$c_site); req(s); u <- as_unit(input$c_units); c_unit_prev(u); apply_city_plan(s, u)
    ch <- city_view_choices(s$id); keep <- if (isTRUE(isolate(input$c_view) %in% ch$values)) isolate(input$c_view) else "direction"
    updateRadioButtons(session, "c_view", choiceNames = ch$names, choiceValues = ch$values, selected = keep)     # Street lights appears only where the snapshot found lamps
  })
  observeEvent(input$c_units, {
    s <- ancient_site(input$c_site); req(s); u <- as_unit(input$c_units); prev <- c_unit_prev(); req(u != prev)
    km <- suppressWarnings(as.numeric(input$c_radius)) * (if (identical(prev, "mi")) km_per_mile else 1)
    c_unit_prev(u); apply_city_plan(s, u, if (is.finite(km)) km else NULL)
  }, ignoreInit = TRUE)
  observeEvent(input$c_reset, {
    s <- ancient_site(input$c_site); req(s)
    updateRadioButtons(session, "c_view", selected = "direction"); updateRadioButtons(session, "c_units", selected = "km")
    updateRadioButtons(session, "c_style", selected = get_style_name(input$style)); bslib::update_switch("c_relief", value = FALSE, session = session)
    c_unit_prev("km"); apply_city_plan(s, "km")
  })
  c_want_m <- reactive({ s <- ancient_site(input$c_site); req(s); round(city_radius_km(s, input$c_radius, input$c_units) * 1000) })
  output$c_radius_help <- renderUI({
    s <- ancient_site(input$c_site); req(s); saved <- ancient_bundle()[[s$id]]
    if (is.list(saved) && isTRUE(as.integer(saved$radius_m) >= c_want_m())) NULL
    else p(class = "fine", "This size is not in the saved streets, so Draw map downloads it from OpenStreetMap, which can take a little while.")
  })
  observeEvent(input$c_draw, {
    s <- ancient_site(input$c_site); req(s)
    crs <- utm_epsg(s$lon, s$lat); radius_m <- c_want_m()
    saved <- bundled_city(s, radius_m)
    roads <- if (!is.null(saved)) saved$roads else tryCatch(withProgress(message = paste0("Getting the streets of ", s$name), value = 0.3,
                        fetch_roads(s$lon, s$lat, radius_m, crs, NULL, detail = "ancient")),
                      error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL })
    if (is.null(roads)) { c_state(NULL); return() }
    lines <- prep_lines(roads, s$lon, s$lat, radius_m, crs)
    got <- city_coverage(lines)
    if (!is.null(saved) && !is.null(lines)) got$text <- paste0(got$text, " The streets come from an OpenStreetMap snapshot taken ", saved$taken, ".")
    c_state(list(site = s, lines = lines, crs = crs, got = got, radius_m = radius_m))
  })
  c_data <- reactive(c_state())
  c_ready <- reactive({ d <- c_data(); if (is.null(d) || is.null(d$lines)) NULL else d })
  output$c_has_map <- renderText(if (is.null(c_ready())) "no" else "yes"); outputOptions(output, "c_has_map", suspendWhenHidden = FALSE)
  output$c_note <- renderUI({
    s <- ancient_site(input$c_site); if (is.null(s)) return(NULL); e <- city_estimate(s$id)
    tagList(p(class = "fine", paste0(s$civ, ". ", s$note)), if (!is.null(e)) p(class = "fine", city_estimate_text(e)))
  })
  output$c_status <- renderUI({
    d <- c_data()
    if (is.null(d)) return(div(class = "fine", role = "status", "Choose a city and press Draw map. Streets not already saved with the app are downloaded from OpenStreetMap, which can take a little while."))
    stale <- !identical(d$site$id, input$c_site); resized <- !stale && !identical(as.integer(d$radius_m), as.integer(c_want_m()))
    tagList(div(class = "fine", role = "status", d$got$text),
            if (stale) div(class = "fine", role = "status", paste0("This map shows ", d$site$name, ". Press Draw map to see ", ancient_site(input$c_site)$name, ".")),
            if (resized) div(class = "fine", role = "status", "Press Draw map to use the new radius."))
  })
  c_style <- reactive(style_key(input$c_style, input$cvd))
  # Nature & water and Street lights need the water, parks and lamps of the site: read from the snapshot when it has them, otherwise downloaded once when
  # one of those map types is chosen and kept for the site on screen.
  c_ex_cache <- new.env(); c_ex_cache$key <- NULL; c_ex_cache$value <- NULL
  c_extras <- reactive({
    d <- c_ready(); req(d); req(get_city_view(input$c_view) %in% c("nature", "lights"))
    key <- paste(d$site$id, d$radius_m)
    if (!identical(key, c_ex_cache$key)) {
      val <- bundled_extras(d$site, d$radius_m)
      if (is.null(val)) val <- tryCatch(withProgress(message = "Getting water, parks and lamps", value = 0.4,
                                                    fetch_roads(d$site$lon, d$site$lat, d$radius_m, d$crs, NULL, detail = "site")),
                                        error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL })
      if (is.null(val)) return(NULL)
      c_ex_cache$key <- key; c_ex_cache$value <- val
    }
    c_ex_cache$value
  })
  make_c_plot <- function(style) {
    d <- c_ready(); req(d); d$view <- get_city_view(input$c_view); d$units <- as_unit(input$c_units); d$relief <- isTRUE(input$c_relief)
    if (d$view %in% c("nature", "lights")) {
      ex <- c_extras()
      if (is.null(ex)) d$view <- "direction"               # the extra download failed (a message says so), so show the streets
      else {
        s <- d$site
        if (d$view == "nature") d$nature <- prep_nature(ex$nature, s$lon, s$lat, d$radius_m, d$crs)
        else { d$lights <- prep_lights(ex$lights, s$lon, s$lat, d$radius_m, d$crs); d$lines <- with_lit_ids(d$lines, ex$lit_ids) }
      }
    }
    city_map(d, style)
  }
  c_plot <- reactive(make_c_plot(c_style()))
  c_dl_style <- reactive(style_key(input$c_style, input$c_dl_cvd %||% "standard"))
  output$c_map <- renderImage({
    d <- c_ready(); req(d); v <- get_city_view(input$c_view); d$view <- v
    f <- tempfile(fileext = ".png")
    p <- c_plot()
    draw_figure(withProgress(message = "Drawing the plan", value = 0.5, p), f, "png", c_style(), preview = TRUE)
    list(src = f, contentType = "image/png", alt = city_alt(d), width = "100%", height = "auto")
  }, deleteFile = TRUE)
  output$c_download <- downloadHandler(
    filename = function() { d <- c_ready(); fmt <- if (isTRUE(input$c_format %in% export_formats)) input$c_format else "png"
      paste0("streets_", d$site$id, "_", get_city_view(input$c_view), size_suffix(input$c_print), res_suffix(fmt, input$c_res), "_", format(Sys.Date(), "%Y%m%d"), ".", format_ext[[fmt]]) },
    content = function(file) { fmt <- if (isTRUE(input$c_format %in% export_formats)) input$c_format else "png"; sty <- c_dl_style(); draw_figure(if (identical(sty, c_style())) c_plot() else make_c_plot(sty), file, fmt, sty, print_key = print_key_of(input$c_print), res = res_pick(input$c_res)) })
  h_data <- reactive(roman_data(input$h_region, input$h_view, input$h_how, input$h_sea))
  h_style <- reactive(style_key(input$h_style, input$cvd))
  output$h_blurb <- renderText(unname(roman_view_blurbs[roman_pick(input$h_view, names(roman_views), "kind")]))
  # Like Current maps, the page opens on the bridges picture and draws a map when asked, then follows the options.
  h_on <- reactiveVal(FALSE); observeEvent(input$h_draw, h_on(TRUE))
  output$h_has_map <- renderText(if (h_on()) "yes" else "no"); outputOptions(output, "h_has_map", suspendWhenHidden = FALSE)
  h_plot <- reactive({ d <- h_data(); req(d); roman_map(d, h_style()) })
  h_dl_style <- reactive(style_key(input$h_style, input$h_dl_cvd %||% "standard"))
  output$h_map <- renderImage({
    req(h_on()); d <- h_data(); req(d)
    f <- tempfile(fileext = ".png")
    draw_figure(withProgress(message = "Drawing roads", value = 0.5, h_plot()), f, "png", h_style(), preview = TRUE)
    list(src = f, contentType = "image/png", alt = roman_alt(d), width = "100%", height = "auto")
  }, deleteFile = TRUE)
  output$h_download <- downloadHandler(
    filename = function() {
      d <- h_data(); fmt <- if (isTRUE(input$h_format %in% export_formats)) input$h_format else "png"
      paste0("roman_roads_", d$region, "_", d$view, size_suffix(input$h_print), res_suffix(fmt, input$h_res), "_", format(Sys.Date(), "%Y%m%d"), ".", format_ext[[fmt]])
    },
    content = function(file) {
      fmt <- if (isTRUE(input$h_format %in% export_formats)) input$h_format else "png"
      sty <- h_dl_style(); d <- h_data(); req(d); draw_figure(roman_map(d, sty), file, fmt, sty, print_key = print_key_of(input$h_print), res = res_pick(input$h_res))
    })

  # Egyptian desert: modeled caravan routes.
  d_style <- reactive(style_key(input$d_style, input$cvd))
  d_on <- reactiveVal(FALSE); observeEvent(input$d_draw, d_on(TRUE))
  output$d_has_map <- renderText(if (d_on()) "yes" else "no"); outputOptions(output, "d_has_map", suspendWhenHidden = FALSE)
  d_plot <- reactive({ d <- desert_data(); req(d); desert_map(d, d_style()) })
  d_dl_style <- reactive(style_key(input$d_style, input$d_dl_cvd %||% "standard"))
  output$d_map <- renderImage({
    req(d_on()); d <- desert_data(); req(d)
    f <- tempfile(fileext = ".png")
    draw_figure(withProgress(message = "Drawing routes", value = 0.5, d_plot()), f, "png", d_style(), preview = TRUE)
    list(src = f, contentType = "image/png", alt = desert_alt(d), width = "100%", height = "auto")
  }, deleteFile = TRUE)
  output$d_download <- downloadHandler(
    filename = function() { fmt <- if (isTRUE(input$d_format %in% export_formats)) input$d_format else "png"; paste0("eastern_desert_modeled_routes", size_suffix(input$d_print), res_suffix(fmt, input$d_res), "_", format(Sys.Date(), "%Y%m%d"), ".", format_ext[[fmt]]) },
    content = function(file) { fmt <- if (isTRUE(input$d_format %in% export_formats)) input$d_format else "png"; sty <- d_dl_style(); d <- desert_data(); req(d); draw_figure(desert_map(d, sty), file, fmt, sty, print_key = print_key_of(input$d_print), res = res_pick(input$d_res)) })

  # Expand: fold the sidebar away so the map gets the window, and bring it back on exit.
  observeEvent(input$expanded, for (id in c("sb", "h_sb", "c_sb", "d_sb")) try(bslib::toggle_sidebar(id, open = !isTRUE(input$expanded)), silent = TRUE))
}

shinyApp(ui, server)

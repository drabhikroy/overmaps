suppressMessages({ library(testthat); library(shiny); library(sf); library(dplyr); library(tibble) })

app_file <- Sys.getenv("APP_FILE", if (file.exists("app.R")) "app.R" else "../app.R")

Sys.setenv(OVERMAPS_DATA = file.path(dirname(normalizePath(app_file)), "data"))

Sys.setenv(STREET_MAPS_DATA = file.path(tempdir(), "qa_data"))

env <- get0("qa_env", envir = globalenv(), ifnotfound = NULL)

if (is.null(env)) { env <- new.env(parent = globalenv()); suppressMessages(sys.source(app_file, envir = env)) }

lon0 <- -79.8467

lat0 <- 38.9262

crs0 <- 32617

ctr <- st_transform(st_sfc(st_point(c(lon0, lat0)), crs = 4326), crs0)

cx <- st_coordinates(ctr)[1]

cy <- st_coordinates(ctr)[2]

mk <- function(pts, h, nm = NA_character_)
  st_sf(osm_id = "1", name = nm, highway = h, geometry = st_sfc(st_linestring(pts), crs = crs0))

fixture_lines <- function() {
  L <- list()
  for (i in seq(-1200, 1200, by = 200)) {
    L[[length(L) + 1]] <- mk(cbind(cx + c(i, i), cy + c(-1200, 1200)), "residential")
    L[[length(L) + 1]] <- mk(cbind(cx + c(-1200, 1200), cy + c(i, i)), "unclassified")
  }
  L[[length(L) + 1]] <- mk(cbind(cx + c(-3500, 3500), cy + c(0, 0)), "primary", "Davis Avenue")      # 6 km, crosses ring
  L[[length(L) + 1]] <- mk(cbind(cx + c(0, 0), cy + c(-3500, 3500)), "trunk", "Corridor H")
  L[[length(L) + 1]] <- mk(cbind(cx + c(-1500, 1500), cy + c(600, 600)), "secondary", "Beverly Pike")
  L[[length(L) + 1]] <- mk(cbind(cx + c(-1500, 1500), cy + c(-600, -600)), "tertiary", "Third Street")
  L[[length(L) + 1]] <- mk(cbind(cx + c(100, 400), cy + c(100, 400)), "service")                     # must be classed local
  bind_rows(L)
}

fixture_roads <- function(l = fixture_lines()) list(osm_lines = st_transform(l, 4326))

with_get <- function(fn, code) {            # replace the one-server downloader; restore it and forget server health afterwards
  old <- env$overpass_get; env$overpass_get <- fn
  on.exit({ env$overpass_get <- old; env$mirror_state$bad_until <- list() }, add = TRUE); force(code)
}

render_png <- function(p, w = 8, h = 10, dpi = 100) {
  f <- tempfile(fileext = ".png")
  showtext::showtext_opts(dpi = dpi)
  ggplot2::ggsave(f, p, width = w, height = h, dpi = dpi, bg = env$bg, device = ragg::agg_png)
  f
}

subtitle_of <- function(p) p$patches$annotation$subtitle

title_of <- function(p) p$patches$annotation$title

orig <- list(geocode = env$geocode, fetch_roads = env$fetch_roads, geocode_address = env$geocode_address, photon_search = env$photon_search, geocode_lookup = env$geocode_lookup, census_search = env$census_search)

restore_orig <- function() { env$geocode <- orig$geocode; env$fetch_roads <- orig$fetch_roads; env$geocode_address <- orig$geocode_address; env$photon_search <- orig$photon_search; env$geocode_lookup <- orig$geocode_lookup; env$census_search <- orig$census_search }

with_stubs <- function(code, boundary = NULL, addr = NULL, photon = NULL, lookup = NULL, census = NULL) {
  withr::defer(restore_orig(), envir = parent.frame())
  calls <- new.env(); calls$fetch <- list(); calls$geo <- character(); calls$addr <- list(); calls$photon <- list(); calls$lookup <- list(); calls$census <- list()
  env$census_search <- function(q, limit = 5) { calls$census[[length(calls$census) + 1]] <- q; if (is.function(census)) census(q) else census }
  env$photon_search <- function(q, kind = "place", near_bbox = NULL, limit = 6, bias = NULL) { calls$photon[[length(calls$photon) + 1]] <- list(q = q, kind = kind, near = near_bbox, bias = bias); if (is.null(photon)) NULL else photon[[kind]] }
  env$geocode_lookup <- function(osm_type, osm_id) { calls$lookup[[length(calls$lookup) + 1]] <- list(type = osm_type, id = osm_id); lookup }
  if (!is.null(addr)) env$geocode_address <- function(q, bbox = NULL, limit = 1, bounded = FALSE) { calls$addr[[length(calls$addr) + 1]] <- list(q = q, bbox = bbox, limit = limit, bounded = bounded); if (is.function(addr)) addr(q, bbox, limit, bounded) else if (inherits(addr, "error")) stop(addr) else if (identical(addr, "none")) NULL else utils::head(addr, limit) }
  env$geocode <- function(q) { calls$geo <- c(calls$geo, q); tibble(label = c("Elkins, Randolph County, West Virginia, United States", "Other, Place"), lat = c(lat0, 10), lon = c(lon0, 10),
                                                                  boundary = list(boundary, NULL)) }
  env$fetch_roads <- function(lon, lat, radius_m, crs_m, shape = NULL, on_try = NULL, detail = "all") { calls$fetch[[length(calls$fetch) + 1]] <- list(lon = lon, lat = lat, radius_m = radius_m, crs_m = crs_m, shape = shape, detail = detail); fixture_roads() }
  force(code); calls
}

rgb_lum <- function(h) { v <- grDevices::col2rgb(h)[, 1] / 255; v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4); sum(v * c(0.2126, 0.7152, 0.0722)) }

contrast <- function(a, b) { l <- sort(c(rgb_lum(a), rgb_lum(b)), decreasing = TRUE); (l[1] + 0.05) / (l[2] + 0.05) }

file_bytes <- function(f) readBin(f, "raw", file.size(f))

jpeg_dim <- function(f) {                      # walk the JPEG marker segments to the frame header
  b <- file_bytes(f); i <- 3L
  while (i + 8L <= length(b)) {
    if (b[i] != as.raw(0xFF)) return(NULL)
    m <- as.integer(b[i + 1L])
    if (m %in% c(0xC0, 0xC1, 0xC2)) return(c(w = as.integer(b[i + 7L]) * 256L + as.integer(b[i + 8L]), h = as.integer(b[i + 5L]) * 256L + as.integer(b[i + 6L])))
    i <- i + 2L + as.integer(b[i + 2L]) * 256L + as.integer(b[i + 3L])
  }
  NULL
}

tiff_dim <- function(f) {                      # little-endian TIFF: read ImageWidth (256) and ImageLength (257) from the first directory
  b <- file_bytes(f); u16 <- function(o) as.integer(b[o]) + 256L * as.integer(b[o + 1L]); u32 <- function(o) u16(o) + 65536 * u16(o + 2L)
  stopifnot(rawToChar(b[1:2]) == "II", u16(3L) == 42L)
  ifd <- u32(5L) + 1L; n <- u16(ifd); out <- c(w = NA, h = NA)
  for (k in seq_len(n)) { e <- ifd + 2L + (k - 1L) * 12L; tag <- u16(e); typ <- u16(e + 2L)
    v <- if (typ == 3L) u16(e + 8L) else u32(e + 8L)
    if (tag == 256L) out[["w"]] <- v; if (tag == 257L) out[["h"]] <- v }
  out
}

demo_lines_prepped <- function() env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0)

bg_of <- function(p) p$patches$annotation$theme$plot.background$fill

test_that("ST1 get_style falls back to Classic for anything unexpected", {
  for (bad in list(NULL, NA, "", "nope", "<script>", 3, c("dusk", "mono"), character(0), list()))
    expect_equal(env$get_style(bad)$label, if (identical(bad, c("dusk", "mono"))) "Dusk" else "Classic", info = deparse(bad))
  expect_equal(env$get_style("dusk")$label, "Dusk"); expect_equal(env$get_style("mono")$label, "Mono")
})

test_that("ST1b every style is complete, distinct, and readable", {
  need <- c("label", "bg", "ink", "muted", "faint", "ring", "cols")
  for (k in names(env$styles)) {
    p <- env$styles[[k]]
    expect_true(all(need %in% names(p)), info = k)
    expect_length(p$cols, 5); expect_equal(length(unique(p$cols)), 5, info = k)      # five roles, five different colors
    expect_true(all(grepl("^#[0-9a-fA-F]{6}$", c(p$bg, p$ink, p$muted, p$faint, p$ring, p$cols))), info = k)
    for (txt in c("ink", "muted", "faint")) expect_gte(contrast(p[[txt]], p$bg), 4.5)   # WCAG AA for text, fine print included
    expect_gt(contrast(p$cols[1], p$bg), contrast(p$cols[5], p$bg))                      # local streets recede, highways stand out
  }
  for (k in c("dusk", "mono")) for (i in 1:4) expect_true(contrast(env$styles[[k]]$cols[i], env$styles[[k]]$bg) >= 3, info = paste(k, i))   # new palettes: all through-route colors clear the 3:1 graphics threshold
  expect_equal(env$styles$classic$cols, env$roles$col)                 # Classic keeps the approved palette
})

sf1 <- function(pts, ...) st_sf(osm_id = "1", name = NA_character_, highway = "residential", ..., geometry = st_sfc(st_linestring(pts), crs = crs0))

mk_lines <- function(specs) {       # specs: list(list(pts, maxspeed)) -> prepped lines with `km`, `role`
  l <- do.call(rbind, lapply(specs, function(x) sf1(x[[1]], maxspeed = x[[2]])))
  env$prep_lines(list(osm_lines = st_transform(l, 4326)), lon0, lat0, 3000, crs0)
}

ctr_m <- function() env$center_of(lon0, lat0, crs0)

ang_pts <- function(deg, len = 1000, off = c(0, 0)) { r <- deg * pi / 180; cbind(cx + off[1] + c(0, len * sin(r)), cy + off[2] + c(0, len * cos(r))) }

gj_ring <- function(pts) lapply(seq_len(nrow(pts)), \(i) list(pts[i, 1], pts[i, 2]))

gj_poly <- function(pts) list(type = "Polygon", coordinates = list(gj_ring(pts)))

square_pts <- cbind(lon0 + c(0, .02, .02, 0, 0), lat0 + c(0, 0, .02, .02, 0))

sq_shape <- function(half = 1000) st_sfc(st_polygon(list(cbind(cx + half * c(-1, 1, 1, -1, -1), cy + half * c(-1, -1, 1, 1, -1)))), crs = crs0)

veil_layer <- function(p, want_m2, tol = 0.01) {      # the dimming layer is the one whose polygon has the expected area
  for (ly in p[[1]]$layers) { d <- ly$data
    if (inherits(d, "sf") && nrow(d) == 1 && abs(as.numeric(st_area(d)) - want_m2) / want_m2 < tol) return(ly) }
  NULL
}

has_veil <- function(p) any(vapply(p[[1]]$layers, function(ly) isTRUE(ly$aes_params$alpha == 0.74), logical(1)))

overpass_xml <- function() paste0('<?xml version="1.0" encoding="UTF-8"?>\n<osm version="0.6" generator="Overpass API">\n<note>x</note><meta osm_base="2026-11-11T00:00:00Z"/>\n',
  '<node id="1" lat="38.9200" lon="-79.8500"/><node id="2" lat="38.9210" lon="-79.8500"/>\n',
  '<way id="10"><nd ref="1"/><nd ref="2"/><tag k="highway" v="residential"/><tag k="name" v="Test Street"/></way>\n</osm>')

ok_resp <- function() httr2::response(200, body = charToRaw(overpass_xml()))

reset_mirrors <- function() env$mirror_state$bad_until <- list()

bb_test <- c(xmin = -79.86, ymin = 38.91, xmax = -79.84, ymax = 38.93)

mirrors <- function() env$overpass_mirrors

mk_status <- function(f) function(req) if (grepl("/status$", req$url)) httr2::response(200, body = charToRaw("Connected as: 1\n2 slots available now.\n")) else f(req)

Sys.setenv(STREET_MAPS_OVERPASS = paste(env$overpass_mirrors, collapse = ","))

test_that("G6 the counts panel and the expand button are in the page, and nothing opens a popup", {
  html <- as.character(env$ui(NULL))
  expect_match(html, "Download counts", fixed = TRUE); expect_match(html, 'id="counts"', fixed = TRUE)
  expect_match(html, 'id="expand-btn"', fixed = TRUE); expect_match(html, "streetMapsToggleExpand()", fixed = TRUE); expect_match(html, " Expand", fixed = TRUE)
  expect_match(html, 'id="sb"', fixed = TRUE)                                                    # the sidebar can be folded by the app
  expect_false(grepl("Pop out", html, fixed = TRUE))
  expect_false(grepl("window.open", env$app_js, fixed = TRUE))                                  # popup blockers can never get in the way
  expect_match(env$app_js, "requestFullscreen", fixed = TRUE); expect_match(env$app_js, "fullscreenchange", fixed = TRUE)
  expect_match(env$app_js, "Shiny.setInputValue('expanded'", fixed = TRUE); expect_match(env$app_js, "is-expanded", fixed = TRUE)
  expect_match(env$app_css, ".is-expanded .expand-btn .when-normal { display: none; }", fixed = TRUE)
  expect_match(env$app_css, ".is-expanded .navbar .navbar-brand, .is-expanded .navbar .nav-item:not(.tools-item), .is-expanded .cvd-pick { display: none; }", fixed = TRUE)   # the header shrinks to the exit button
})

test_that("G6b expanding folds the sidebar away and leaving brings it back", {
  calls <- list(); testthat::local_mocked_bindings(toggle_sidebar = function(id, open = NULL, ...) { calls[[length(calls) + 1]] <<- list(id = id, open = open); invisible() }, .package = "bslib")
  testServer(env$server, {
    session$setInputs(expanded = TRUE); session$setInputs(expanded = FALSE); session$setInputs(expanded = TRUE)
  })
  # every page's sidebar folds together, so Expand behaves the same on Current maps and on the three Historical sets
  ids <- c("sb", "h_sb", "c_sb", "d_sb")
  expect_equal(vapply(calls, function(x) x$id, ""), rep(ids, 3)); expect_equal(vapply(calls, function(x) x$open, TRUE), rep(c(FALSE, TRUE, FALSE), each = 4))
})

launcher_path <- Sys.getenv("LAUNCH_FILE", file.path(dirname(normalizePath(app_file)), "launch.R"))

lenv <- new.env(parent = globalenv())

suppressMessages(sys.source(launcher_path, envir = lenv))

with_env <- function(val, code) { old <- Sys.getenv("STREET_MAPS_OVERPASS", unset = NA); Sys.setenv(STREET_MAPS_OVERPASS = val)
  on.exit(if (is.na(old)) Sys.unsetenv("STREET_MAPS_OVERPASS") else Sys.setenv(STREET_MAPS_OVERPASS = old)); force(code) }

bbox_at <- function(lon, lat, d = 0.05) c(xmin = lon - d, ymin = lat - d, xmax = lon + d, ymax = lat + d)

mk_lines <- function(...) { l <- list(...); st_sf(id = seq_along(l), geometry = st_sfc(lapply(l, function(m) st_linestring(matrix(m, ncol = 2, byrow = TRUE))), crs = 32617)) }

net_of <- function(ln, r = 1000) { cl <- st_buffer(st_sfc(st_point(c(0, 0)), crs = 32617), r); env$network_stats(ln, env$segments_of(ln), cl, as.numeric(st_area(cl)) / 1e6, r * 0.03) }

open_link <- function(link, after = function() NULL) {
  ups <- list()
  rec <- function(nm) function(session, inputId, ...) { a <- list(...); ups[[length(ups) + 1]] <<- c(list(fn = nm, id = inputId), a); invisible() }
  testthat::local_mocked_bindings(updateRadioButtons = rec("radio"), updateTextInput = rec("text"), updateSelectInput = rec("select"), updateSliderInput = rec("slider"), .package = "shiny", .env = parent.frame())
  testthat::local_mocked_bindings(update_switch = function(id, value = NULL, ...) { ups[[length(ups) + 1]] <<- list(fn = "switch", id = id, value = value); invisible() }, .package = "bslib", .env = parent.frame())
  env$parse_share <- function(qs) link
  withr::defer(env$parse_share <- orig_parse, envir = parent.frame())
  ups
}

orig_parse <- env$parse_share

cmp_g <- function() list(lines = demo_lines_prepped(), place = "Elkins, West Virginia", lon = lon0, lat = lat0, radius_m = 3000L, crs_m = crs0, shape = NULL, center_txt = NULL)

cmp_geo <- function(calls, label = "Morgantown, Monongalia County, West Virginia, United States") env$geocode <- function(q) { calls$geo <- c(calls$geo, q); tibble(label = label, lat = lat0 + 0.002, lon = lon0 + 0.002) }

ins_lines <- function() { l <- env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0); l$maxspeed[l$name %in% "Third Street"] <- "35 mph"; l$maxspeed[l$name %in% "Corridor H"] <- "50"; l }

ins_panel <- list(x0 = 0.1, x1 = 0.9, y0 = 0.2, y1 = 0.8, xr = cx + c(-3000, 3000), yr = cy + c(-3000, 3000))

at_xy <- function(x, y, pn = ins_panel) c(fx = pn$x0 + (x - pn$xr[1]) / diff(pn$xr) * (pn$x1 - pn$x0), fy = pn$y0 + (pn$yr[2] - y) / diff(pn$yr) * (pn$y1 - pn$y0))

click <- function(x, y, lines = ins_lines()) { f <- at_xy(x, y); env$inspect_street(lines, ins_panel, f[["fx"]], f[["fy"]], 3000, crs0) }

bw_roads <- function() {
  l <- fixture_lines(); n <- nrow(l); l$sidewalk <- NA_character_; l$cycleway.left <- NA_character_; l$cycleway <- NA_character_; l[["sidewalk:right"]] <- NA_character_
  l$sidewalk[1] <- "both"; l$cycleway.left[2] <- "lane"; l$cycleway[3] <- "shared_lane"; l$sidewalk[4] <- "no"; l$sidewalk[5] <- "separate"; l$cycleway.left[5] <- "track"; l[["sidewalk:right"]][6] <- "yes"
  list(osm_lines = st_transform(l, 4326))
}

bw_paths <- function() {
  mk <- function(x1, y1, x2, y2) sf::st_linestring(matrix(c(cx + x1, cy + y1, cx + x2, cy + y2), ncol = 2, byrow = TRUE))
  p <- sf::st_sf(highway = c("cycleway", "footway", "footway", "path", "pedestrian", "path"), footway = c(NA, "sidewalk", "crossing", NA, NA, NA), bicycle = c(NA, NA, NA, NA, NA, "designated"),
                 geometry = sf::st_sfc(mk(-500, 300, 500, 300), mk(-500, 320, 500, 320), mk(0, 300, 0, 320), mk(-400, -300, 400, -300), mk(-400, -350, 400, -350), mk(-300, 700, 300, 700), crs = crs0))
  list(osm_lines = st_transform(p, 4326))
}

pl_items <- tibble(label = c("Elkins, Randolph County, West Virginia, United States", "Elkins Park, Pennsylvania, United States"), lon = c(lon0, -75.1), lat = c(lat0, 40.07), osm_type = c("R", "N"), osm_id = c(123456, 777), is_addr = FALSE, where = "")

ad_items <- tibble(label = c("401 Davis Avenue, Elkins, West Virginia", "401 Davis Street, Elkins, West Virginia"), lon = c(lon0 + 0.003, lon0 + 0.004), lat = c(lat0 + 0.002, lat0 + 0.003), osm_type = "W", osm_id = c(1, 2), is_addr = TRUE, where = "")

test_that("H4 the Historical maps page is reached from the header, draws a map, and downloads a real file", {
  ui <- as.character(env$ui(NULL)); expect_match(ui, 'id="go_history"', fixed = TRUE); expect_match(ui, "Historical maps", fixed = TRUE); expect_false(grepl("Explore history", ui, fixed = TRUE))
  m <- as.character(env$history_page("classic")); for (id in c("h_region", "h_view", "h_how", "h_sea", "h_style", "h_format", "h_download", "h_map")) expect_match(m, paste0('id="', id, '"'), fixed = TRUE)
  expect_match(m, "Itiner-e", fixed = TRUE); expect_match(m, "CC BY 4.0", fixed = TRUE)
  shown <- NULL; out <- list()
  testthat::local_mocked_bindings(showModal = function(ui, ...) { shown <<- as.character(ui); invisible() }, .package = "shiny")
  testServer(env$server, {
    session$flushReact()
    session$setInputs(h_region = "britain", h_view = "days", h_how = "courier", h_sea = FALSE, h_style = "dusk", h_format = "png", h_draw = 1); session$flushReact()
    out$blurb <<- output$h_blurb; img <- output$h_map; out$src <<- img$src; out$alt <<- img$alt
    p <- output$h_download; out$dl <<- basename(p); out$ok <<- file.exists(p) && file.size(p) > 20000 && identical(readBin(p, "raw", 4)[2:4], charToRaw("PNG"))
    session$setInputs(h_format = "pdf"); out$pdf <<- basename(output$h_download); session$setInputs(h_format = "../../etc/passwd"); out$bad <<- basename(output$h_download)
  })
  expect_match(out$blurb, "How long it took"); expect_match(out$alt, "Britain"); expect_true(out$ok)
  expect_match(out$dl, "^roman_roads_britain_days_[0-9]{8}\\.png$"); expect_match(out$pdf, "\\.pdf$"); expect_match(out$bad, "\\.png$")
})

ph_feat <- function(props, xy = c(-86.52, 39.16)) list(properties = props, geometry = list(coordinates = as.list(xy)))

test_that("R18a Photon house addresses (place=house) are kept as addresses and no label reads NA", {
  ft <- list(
    ph_feat(list(osm_key = "place", osm_value = "house", osm_type = "N", osm_id = 11, housenumber = "412", street = "East Kirkwood Avenue", city = "Bloomington", state = "Indiana", postcode = "47408", country = "United States")),
    ph_feat(list(osm_key = "place", osm_value = "city", osm_type = "R", osm_id = 12, name = "Bloomington", county = "Monroe County", state = "Indiana", country = "United States")),
    ph_feat(list(osm_key = "highway", osm_value = "residential", osm_type = "W", osm_id = 13, name = "Kirkwood Avenue", street = "Kirkwood Avenue", city = "Bloomington", state = "Indiana")),
    ph_feat(list(osm_key = "building", osm_value = "yes", osm_type = "W", osm_id = 14, housenumber = "9", street = "Elm Street", city = "Ellettsville", state = "Indiana")),
    ph_feat(list(osm_key = "place", osm_value = "house", osm_type = "N", osm_id = 15, housenumber = "7", street = "Oak Lane", state = "Indiana")))
  pl <- env$photon_rows(ft, "place")
  expect_equal(pl$label[1], "412 East Kirkwood Avenue, Bloomington, Indiana"); expect_true(pl$is_addr[1]); expect_equal(pl$where[1], "Bloomington, Indiana, United States")
  expect_false(pl$is_addr[2]); expect_equal(pl$label[2], "Bloomington, Monroe County, Indiana, United States"); expect_equal(pl$where[2], "")
  expect_true(all(pl$is_addr[c(1, 3, 4, 5)])); expect_false(any(grepl("NA", pl$label, fixed = TRUE))); expect_false(any(grepl("NA", pl$where, fixed = TRUE)))
  expect_equal(pl$label[5], "7 Oak Lane, Indiana")           # a house with no town still gets a clean label
  ad <- env$photon_rows(ft, "address"); expect_true(all(ad$is_addr)); expect_equal(nrow(ad), 4)
  expect_false(env$photon_is_address(list(osm_key = "place", osm_value = "city", name = "X"))); expect_false(env$photon_is_address(list(osm_key = "boundary", street = "Main")))
  expect_null(env$photon_rows(list(), "place")); expect_null(env$photon_rows(list(ph_feat(list(osm_key = "natural", name = "A hill"))), "place"))
})

test_that("R18b Census matches become suggestion rows with readable labels, and a U.S. place is reduced to city and state", {
  m <- list(matchedAddress = "412 E KIRKWOOD AVE, BLOOMINGTON, IN, 47408", coordinates = list(x = -86.52, y = 39.16), addressComponents = list(city = "BLOOMINGTON", state = "IN", zip = "47408"))
  r <- env$census_rows(list(m, m, list(matchedAddress = "bad", coordinates = list(x = "a", y = NA))))
  expect_equal(nrow(r), 1); expect_equal(r$label, "412 E Kirkwood Ave, Bloomington, IN 47408")
  expect_equal(r$where, "Bloomington, IN"); expect_true(r$is_addr); expect_equal(c(r$lon, r$lat), c(-86.52, 39.16)); expect_equal(names(r), c("label", "lon", "lat", "osm_type", "osm_id", "is_addr", "where"))
  expect_null(env$census_rows(list())); expect_equal(env$title_case("1ST AVE"), "1st Ave")
  expect_equal(env$us_city_state("Bloomington, Monroe County, Indiana, United States"), "Bloomington, Indiana"); expect_equal(env$us_city_state("Paris, France"), "")
  expect_equal(env$us_city_state("Elkins, Randolph County, West Virginia, United States"), "Elkins, West Virginia"); expect_equal(env$us_city_state("Texas"), "")
  mock <- function(req) httr2::response_json(body = list(result = list(addressMatches = list(m))))
  expect_equal(httr2::with_mocked_responses(mock, env$census_search("412 E Kirkwood Ave, Bloomington, IN"))$where, "Bloomington, IN")
  expect_null(httr2::with_mocked_responses(function(req) httr2::response(status_code = 503), env$census_search("x")))
})

test_that("R18c an address OpenStreetMap lacks falls back to the Census file for U.S. places only, and the one box does the same", {
  cen <- tibble(label = "412 E Kirkwood Ave, Bloomington, IN 47408", lon = -86.52, lat = 39.16, osm_type = "", osm_id = NA_real_, is_addr = TRUE, where = "Bloomington, IN")
  none <- function(q, bbox, limit, bounded) NULL
  calls <- with_stubs(NULL, addr = none, census = cen)
  r <- env$find_address("412 E Kirkwood Ave", "Bloomington, Monroe County, Indiana, United States", c(39.1, 39.2, -86.6, -86.4))
  expect_false(r$outside); expect_equal(r$hits$label, cen$label); expect_equal(calls$census[[1]], "412 E Kirkwood Ave, Bloomington, Indiana")
  calls2 <- with_stubs(NULL, addr = none, census = cen); r2 <- env$find_address("1 Rue X", "Paris, France", c(48, 49, 2, 3))
  expect_length(calls2$census, 0); expect_null(r2$hits)
  calls3 <- with_stubs(NULL, addr = none, census = cen); got <- list()
  testServer(env$server, { session$setInputs(suggest = FALSE, query = "412 E Kirkwood Ave, Bloomington, IN", find = 1); session$flushReact(); got$p <<- tryCatch(places(), error = function(e) NULL); got$pa <<- pending_addr() })
  expect_length(calls3$census, 1); expect_equal(calls3$geo[1], "Bloomington, IN"); expect_equal(env$city_of_label("412 E Kirkwood Ave, Bloomington, IN 47408"), "Bloomington, IN"); expect_equal(env$city_of_label("412, East Kirkwood Avenue, Bloomington, Monroe County, Indiana, 47408, United States"), "Bloomington, Monroe County, Indiana, United States")
})

test_that("R18d typing a U.S. address that Photon lacks suggests it from the Census file, and a number with no town leans toward the place on the map", {
  cen <- tibble(label = "412 E Kirkwood Ave, Bloomington, IN 47408", lon = -86.52, lat = 39.16, osm_type = "", osm_id = NA_real_, is_addr = TRUE, where = "Bloomington, IN")
  calls <- with_stubs(NULL, photon = list(place = NULL, address = NULL), census = cen); got <- list()
  testServer(env$server, {
    session$setInputs(suggest = TRUE, query = "Elkins", find = 1); session$flushReact()
    session$setInputs(typed = list(kind = "place", text = "412 E Kirkwood Ave, Bloomington, IN", t = 1)); got$n <<- if (is.null(sugg$place$items)) 0 else nrow(sugg$place$items)
    got$html <<- as.character(output$place_sugg$html)
    Sys.sleep(0.45); session$setInputs(typed = list(kind = "place", text = "412 E Kirkwood Ave", t = 2)); got$nocity <<- length(calls$census)
  })
  expect_equal(got$n, 1); expect_match(got$html, "412 E Kirkwood Ave, Bloomington, IN 47408", fixed = TRUE); expect_equal(got$nocity, 1)      # no comma, no city: Census is not asked
  b <- calls$photon[[length(calls$photon)]]$bias; expect_equal(round(b, 3), round(c(lat0, lon0), 3))
})

test_that("R18e the Historical maps page has the same pickers as the main page and no pop-up window", {
  m <- as.character(env$history_page("classic"))
  for (id in c("h_tab", "c_site", "c_view", "c_style", "c_draw", "c_format", "c_download", "c_map", "h_style", "h_view")) expect_match(m, paste0('id="', id, '"'), fixed = TRUE)
  expect_false(grepl("modal", m, fixed = TRUE)); expect_false(grepl("overmapsHistoryFull", env$app_js, fixed = TRUE))
  expect_equal(lengths(regmatches(m, gregexpr("Choose a look", m, fixed = TRUE))), 3L); expect_equal(lengths(regmatches(m, gregexpr('class="style-picker"', m, fixed = TRUE))), 3L)      # one look picker on each of the three tabs
  expect_equal(lengths(regmatches(m, gregexpr('class="view-picker"', m, fixed = TRUE))), 2L); expect_false(grepl("selectInput.*h_style", m)); expect_false(grepl('id="h_style" class="shiny-input-select', m, fixed = TRUE))
  expect_false(grepl("the roads of the Roman Empire", as.character(env$ui(NULL)), fixed = TRUE))      # the old lower-case wording is gone everywhere
})

test_that("R18f the ancient sites are well formed and grouped by civilization", {
  s <- env$ancient_sites
  expect_equal(anyDuplicated(s$id), 0L); expect_true(all(abs(s$lat) <= 90 & abs(s$lon) <= 180)); expect_true(all(s$radius_km >= 0.3 & s$radius_km <= 3))
  expect_true(all(nzchar(s$note)) && all(grepl("\\.$", s$note))); expect_true(all(grepl(", [A-Z]", s$name))); expect_true(all(unlist(env$ancient_site_choices) %in% s$id))
  expect_gte(length(unique(s$civ)), 8); expect_true(all(table(s$civ) >= 1)); expect_true(all(s$lat >= -60 & s$lat <= 60)); expect_false(any(grepl("[–—]", c(s$note, s$name)))); expect_false(any(grepl("n't|'s |'re|'ll", s$note)))
  expect_equal(env$ancient_site("pompeii")$name, "Pompeii, Italy"); expect_null(env$ancient_site("nowhere")); expect_null(env$ancient_site(NULL)); expect_null(env$ancient_site(c("a", "b")[0]))
  expect_equal(env$get_city_view("blocks"), "blocks"); expect_equal(env$get_city_view("hierarchy"), "direction"); expect_equal(env$get_city_view(NULL), "direction")
})

test_that("R18g an ancient city is downloaded once with its own street classes, drawn in each map type, reports coverage, and downloads", {
  withr::defer(env$ancient_sites <- orig_sites); orig_sites <- env$ancient_sites
  env$ancient_sites <- dplyr::bind_rows(orig_sites, tibble(id = "test", name = "Testville, Elkins", civ = "Roman", lat = lat0, lon = lon0, radius_km = 1.5, note = "A fixture."))
  calls <- with_stubs(NULL); out <- list()
  testServer(env$server, {
    session$setInputs(h_tab = "cities", c_site = "test", c_view = "blocks", c_style = "dusk", c_format = "png"); session$flushReact()
    out$before <<- as.character(output$c_status$html); out$has0 <<- output$c_has_map
    session$setInputs(c_draw = 1); session$flushReact()
    out$status <<- as.character(output$c_status$html); out$has <<- output$c_has_map; out$note <<- as.character(output$c_note$html)
    img <- output$c_map; out$alt <<- img$alt; out$ok <<- is.character(img$src) && nzchar(img$src)
    session$setInputs(c_view = "density"); session$flushReact(); out$alt2 <<- output$c_map$alt; session$setInputs(c_view = "direction"); out$alt3 <<- output$c_map$alt
    p <- output$c_download; out$dl <<- basename(p); out$file <<- file.exists(p) && file.size(p) > 20000
    session$setInputs(c_format = "pdf"); out$pdf <<- basename(output$c_download); session$setInputs(c_format = "../x"); out$bad <<- basename(output$c_download)
    session$setInputs(c_site = "pompeii"); session$flushReact(); out$stale <<- as.character(output$c_status$html)
  })
  expect_match(out$before, "press Draw map", ignore.case = TRUE); expect_equal(out$has0, "no")
  expect_length(calls$fetch, 1); expect_equal(calls$fetch[[1]]$detail, "ancient"); expect_equal(calls$fetch[[1]]$radius_m, 1500)
  expect_equal(out$has, "yes"); expect_match(out$status, "mapped streets and paths here"); expect_match(out$note, "Roman. A fixture.", fixed = TRUE); expect_true(out$ok)
  expect_match(out$alt, "Testville.*block length", ignore.case = TRUE); expect_match(out$alt2, "street density"); expect_match(out$alt3, "direction")
  expect_match(out$dl, "^streets_test_direction_[0-9]{8}\\.png$"); expect_true(out$file); expect_match(out$pdf, "\\.pdf$"); expect_match(out$bad, "\\.png$")
  expect_match(out$stale, "Pompeii, Italy"); expect_match(out$stale, "This map shows Testville")
})

test_that("R18h a failed or empty ancient city download says so and draws nothing; a thin site carries a warning; the caption states what the plan is", {
  withr::defer(env$ancient_sites <- orig_sites); orig_sites <- env$ancient_sites
  env$ancient_sites <- dplyr::bind_rows(orig_sites, tibble(id = "test", name = "Testville, Elkins", civ = "Roman", lat = lat0, lon = lon0, radius_km = 1.5, note = "A fixture."))
  out <- list(); shown <- character()
  testthat::local_mocked_bindings(showNotification = function(ui, ...) { shown <<- c(shown, as.character(ui)); invisible() }, .package = "shiny")
  with_stubs(NULL); env$fetch_roads <- function(...) stop("all servers busy")
  testServer(env$server, { session$setInputs(h_tab = "cities", c_site = "test", c_draw = 1); session$flushReact(); out$has <<- output$c_has_map; out$status <<- as.character(output$c_status$html) })
  expect_equal(out$has, "no"); expect_true(any(grepl("all servers busy", shown))); expect_match(out$status, "press Draw map", ignore.case = TRUE)
  with_stubs(NULL); env$fetch_roads <- function(...) list(osm_lines = NULL)
  testServer(env$server, { session$setInputs(h_tab = "cities", c_site = "test", c_draw = 1); session$flushReact(); out$has2 <<- output$c_has_map; out$status2 <<- as.character(output$c_status$html) })
  expect_equal(out$has2, "no"); expect_match(out$status2, "no mapped streets or paths here")
  lines <- demo_lines_prepped(); lines$osm_id <- seq_len(nrow(lines)); thin <- lines[1:5, ]
  expect_match(env$city_coverage(thin)$text, "That is few"); expect_false(grepl("That is few", env$city_coverage(lines)$text)); expect_equal(env$city_coverage(NULL)$n, 0L)
  d <- list(site = list(name = "Testville, Elkins", lon = lon0, lat = lat0, radius_km = 1.5), lines = lines, crs = crs0, view = "direction")
  cap <- env$city_map(d, "classic")$patches$annotation$caption; cap <- paste(cap, collapse = " "); expect_match(cap, "not a reconstruction"); expect_match(cap, "Data \u00a9 OpenStreetMap contributors")
})

test_that("R19a an ancient city saved with the app is drawn with no download, says when the snapshot was taken, and a different radius is downloaded live", {
  withr::defer({ env$ancient_sites <- orig_sites; env$history_cache$cities <- NULL }); orig_sites <- env$ancient_sites
  env$ancient_sites <- dplyr::bind_rows(orig_sites, tibble(id = "test", name = "Testville, Elkins", civ = "Roman", lat = lat0, lon = lon0, radius_km = 1.5, note = "A fixture."))
  env$history_cache$cities <- list(test = list(roads = fixture_roads(), taken = "November 13, 2026", radius_m = 1500L))
  calls <- with_stubs(NULL); out <- list()
  testServer(env$server, { session$setInputs(h_tab = "cities", c_site = "test", c_draw = 1); session$flushReact()
    out$has <<- output$c_has_map; out$status <<- as.character(output$c_status$html) })
  expect_length(calls$fetch, 0); expect_equal(out$has, "yes"); expect_match(out$status, "snapshot taken November 13, 2026")
  env$history_cache$cities <- list(test = list(roads = fixture_roads(), taken = "November 13, 2026", radius_m = 900L))
  calls <- with_stubs(NULL)
  testServer(env$server, { session$setInputs(h_tab = "cities", c_site = "test", c_radius = 2.5, c_units = "km", c_draw = 1); session$flushReact(); out$status2 <<- as.character(output$c_status$html) })
  expect_length(calls$fetch, 1); expect_false(grepl("snapshot", out$status2))
})

test_that("R19b a status check that failed for a real reason is reported with that reason, and the fetch script and a missing bundle are handled", {
  expect_equal(env$explain_failure("status check failed: Timeout was reached")[1], "slow")
  expect_match(env$explain_failure("status check failed: Timeout was reached")[2], "quick status check")
  expect_equal(env$explain_failure("status check failed: Failed to connect to host: Connection refused")[1], "unreachable")
  expect_equal(env$explain_failure("status check failed: Could not resolve host: example.org")[1], "unreachable")
  expect_equal(env$explain_failure("status check failed: something odd")[1], "slow")
  env$history_cache$cities <- NULL; withr::defer(env$history_cache$cities <- NULL)
  withr::local_envvar(OVERMAPS_DATA = file.path(tempdir(), "no_such_data_folder"))
  expect_length(env$ancient_bundle(), 0); expect_null(env$bundled_city(list(id = "pompeii"), 800))
  script <- readLines(file.path(dirname(normalizePath(app_file)), "data-raw", "fetch_ancient_cities.R"), warn = FALSE)
  expect_true(any(grepl("ancient_cities.rds", script, fixed = TRUE))); expect_no_error(parse(text = script))
})

test_that("FE1 a map download whose relation lists a way outside the box is still read, and the fetch script is parseable", {
  doc <- paste0('<?xml version="1.0"?><osm version="0.6"><node id="1" lat="0.001" lon="0.001"/><node id="2" lat="0.002" lon="0.002"/>',
                '<way id="10"><nd ref="1"/><nd ref="2"/><tag k="highway" v="residential"/></way>',
                '<relation id="50"><member type="way" ref="10" role="outer"/><member type="way" ref="99" role="inner"/><tag k="type" v="multipolygon"/></relation></osm>')
  res <- env$parse_osm_doc(doc, osmdata::opq(bbox = c(0, 0, 0.01, 0.01)))
  expect_equal(nrow(res$osm_lines), 1L)
})

test_that("R19c the full text of a failed request keeps the cause curl reported, once, and a refused connection is classed as unreachable", {
  parent <- structure(class = c("error", "condition"), list(message = "Could not connect to server", call = NULL))
  e <- structure(class = c("rlang_error", "error", "condition"), list(message = "Failed to perform HTTP request.", call = NULL, parent = parent))
  txt <- env$error_text(e)
  expect_match(txt, "Failed to perform HTTP request", fixed = TRUE)
  expect_equal(lengths(regmatches(txt, gregexpr("Could not connect to server", txt))), 1L)
  expect_equal(env$explain_failure(paste("status check failed:", txt))[1], "unreachable")
})

root_dir <- dirname(normalizePath(app_file))

read_root <- function(f) paste(readLines(file.path(root_dir, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("REL1 the version is the same in app.R, DESCRIPTION, CITATION.cff, and the newest CHANGELOG section", {
  v <- env$OVERMAPS_VERSION
  expect_match(v, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_equal(sub("^Version: *", "", grep("^Version:", readLines(file.path(root_dir, "DESCRIPTION")), value = TRUE)), v)
  expect_equal(gsub("[\" ]", "", sub("^version: *", "", grep("^version:", readLines(file.path(root_dir, "CITATION.cff")), value = TRUE))), v)
  heads <- grep("^## ", readLines(file.path(root_dir, "CHANGELOG.md")), value = TRUE)
  expect_equal(sub("^## *", "", heads[1]), v)
})

test_that("REL2 the About panel names the version, the license, and the releases page", {
  html <- paste(as.character(env$ui(NULL)), collapse = " ")
  expect_match(html, paste0("Overmaps ", env$OVERMAPS_VERSION, ". Copyright Abhik Roy"), fixed = TRUE)
  expect_match(html, "PolyForm Noncommercial License 1.0.0", fixed = TRUE)
  expect_match(html, "github.com/drabhikroy/overmaps/releases/latest", fixed = TRUE)
})

test_that("REL3 the privacy copy names every service that receives typed text", {
  src <- read_root("app.R")
  expect_match(src, "Photon", fixed = TRUE); expect_match(src, "Census Bureau", fixed = TRUE)
  expect_match(src, "check it before you share it", fixed = TRUE)
  sec <- read_root("SECURITY.md")
  for (h in c("nominatim.openstreetmap.org", "photon.komoot.io", "geocoding.geo.census.gov", "overpass-api.de", "overpass.private.coffee",
              "maps.mail.ru", "ovp.dresel.systems", "api.openstreetmap.org")) expect_match(sec, h, fixed = TRUE)
})

test_that("REL4 the documents follow the writing rules: no em or en dashes, no contractions, none of the banned words", {
  docs <- c("README.md", "CHANGELOG.md", "CONTRIBUTING.md", "SECURITY.md", "CODE_OF_CONDUCT.md", "RELEASE_TEMPLATE.md", "data/README.md", "data-raw/fetch_ancient_cities.R")
  contraction <- "\\b[A-Za-z]+n't\\b|\\b(it|that|there|here|he|she|what|let|who|where)'s\\b|\\b(I|you|we|they)'(m|re|ve|ll|d)\\b"
  banned <- "\\b(actionable|robust|ensure[sd]?|leverag(e|es|ed|ing)|utiliz(e|es|ed|ing)|enhanc(e|es|ed|ing|ement)|seamless(ly)?)\\b"
  for (f in docs) {
    txt <- read_root(f)
    expect_false(grepl("—|–", txt), info = paste(f, "has an em or en dash"))
    expect_false(grepl(contraction, txt, ignore.case = TRUE, perl = TRUE), info = paste(f, "has a contraction"))
    expect_false(grepl(banned, txt, ignore.case = TRUE, perl = TRUE), info = paste(f, "uses a banned word"))
  }
  expect_false(grepl("—|–", read_root("app.R")), info = "app.R has an em or en dash")
})

test_that("REL6 every image the README shows is in the repository and has alt text", {
  md <- read_root("README.md")
  imgs <- regmatches(md, gregexpr('(!\\[[^]]*\\]\\(docs/[^)]+\\))|(<img src="docs/[^"]+"[^>]*>)', md))[[1]]
  expect_gte(length(imgs), 4)
  for (i in imgs) {
    path <- sub('.*(docs/[^)" ]+).*', "\\1", i)
    expect_true(file.exists(file.path(root_dir, path)), info = path)
    expect_false(grepl("^!\\[\\]|alt=\"\"", i), info = paste("no alt text:", path))
  }
})

test_that("NX1 the file has its own color vision menu, Historical pages open on the bridges picture, and Help and Exit expand are fixed", {
  a <- env$ancient_sites
  expect_equal(anyDuplicated(a$id), 0L); expect_true(all(is.finite(a$lat) & abs(a$lat) <= 90 & is.finite(a$lon) & abs(a$lon) <= 180)); expect_true(all(a$radius_km >= 0.3 & a$radius_km <= 3))
  expect_true(all(c("carthage", "karnak", "chaco", "aksum", "anyang") %in% a$id))
  html <- paste(as.character(env$ui(NULL)), collapse = "\n")
  expect_match(html, 'id="theme-btn"', fixed = TRUE); expect_match(html, "Can I use a dark page?", fixed = TRUE)
  expect_match(env$app_css, "html[data-bs-theme='dark']", fixed = TRUE); expect_match(env$app_css, ".history-cols.main", fixed = TRUE); expect_match(env$app_js, "overmaps-theme", fixed = TRUE)
  for (id in c("dl_cvd", "h_dl_cvd", "c_dl_cvd", "d_dl_cvd")) expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("Color vision of the file</label>", html, fixed = TRUE))), 4L)
  expect_match(html, 'id="h_draw"', fixed = TRUE); expect_match(html, 'id="d_draw"', fixed = TRUE); expect_match(html, "help-grow", fixed = TRUE)
  src <- read_root("app.R")
  expect_match(src, ".is-expanded .navbar { background: transparent !important", fixed = TRUE)
  expect_match(src, ".help-page .tab-content { height: clamp(", fixed = TRUE); expect_match(src, "html:has(body.cb-gray) { filter: grayscale(1); }", fixed = TRUE)
  expect_false(grepl("#1f3a5a", sub(".*app_css <- ", "", sub("\\nbrand_svg <- .*", "", sub("(?s)^.*?(:root \\{ --line)", "\\1", src, perl = TRUE), perl = TRUE)), fixed = TRUE))
  testServer(env$server, {
    session$setInputs(style = "classic", dl_cvd = "tritan", cvd = "standard"); expect_equal(sid(), "classic"); expect_equal(dl_style(), "classic~tritan")
    session$setInputs(dl_cvd = "standard"); expect_equal(dl_style(), "classic")
    session$setInputs(h_tab = "roads"); session$flushReact(); expect_equal(output$h_has_map, "no")
    session$setInputs(h_draw = 1); session$flushReact(); expect_equal(output$h_has_map, "yes")
    session$setInputs(d_draw = 1); session$flushReact(); expect_equal(output$d_has_map, "yes")
  })
})

test_that("REL5 the documents state the real counts of ancient sites and civilizations", {
  expect_equal(nrow(env$ancient_sites), 90L); expect_equal(length(unique(env$ancient_sites$civ)), 22L)
  expect_match(read_root("README.md"), "ninety ancient sites"); expect_match(read_root("README.md"), "22 regions")
  expect_match(read_root("CHANGELOG.md"), "22 regions")
})

layer_geoms <- function(p) vapply(p[[1]]$layers, function(l) class(l$geom)[1], "")

has_spikes <- function(p) any(vapply(p[[1]]$layers, function(l) is.data.frame(l$data) && "face" %in% names(l$data), TRUE))

lit_lines <- function(share = 0.4) { l <- demo_lines_prepped(); set.seed(5); l$lit_tag <- runif(nrow(l)) < share; l }

lamp_points <- function(n = 30) sf::st_sf(geometry = sf::st_sfc(lapply(seq_len(n), function(i) sf::st_point(c(ctr_xy()[1] + i * 20, ctr_xy()[2]))), crs = crs0))

ctr_xy <- function() as.numeric(sf::st_coordinates(ctr_m()))

nat_world <- function() {
  blob <- function(x, y, r) { th <- seq(0, 2 * pi, length.out = 33)[-33]; m <- cbind(ctr_xy()[1] + x + r * cos(th), ctr_xy()[2] + y + r * sin(th)); st_polygon(list(rbind(m, m[1, ]))) }
  polys <- st_sf(natural = c("water", NA, "wood", NA), leisure = c(NA, "park", NA, NA), landuse = c(NA, NA, NA, "farmland"),
                 geometry = st_sfc(blob(-1200, 800, 500), blob(0, 0, 900), blob(0, 0, 400), blob(1500, -1500, 300), crs = crs0))      # a park with a wood inside it, a lake, and farmland (ignored)
  t <- seq(0, 1, length.out = 30)
  mk <- function(pts, wat = NA, nat = NA) st_sf(waterway = wat, natural = nat, geometry = st_sfc(st_linestring(pts), crs = crs0))
  ln <- rbind(mk(cbind(ctr_xy()[1] - 3000 + 6000 * t, ctr_xy()[2] + 500), "river"), mk(cbind(ctr_xy()[1] + 2500 + 0 * t, ctr_xy()[2] - 6000 + 12000 * t), NA, "coastline"))
  list(osm_polygons = st_transform(polys, 4326), osm_lines = st_transform(ln, 4326))                                                  # the coast runs south to north, so the sea lies to the east
}

test_that("DS3 the history window has the desert tab, and the server draws it and downloads a real file", {
  m <- as.character(env$history_page("classic")); for (id in c("d_style", "d_format", "d_download", "d_map")) expect_match(m, paste0('id="', id, '"'), fixed = TRUE)
  expect_match(m, "Egyptian desert", fixed = TRUE); expect_match(m, "Computed, not found", fixed = TRUE); expect_match(m, "zenodo.4291876", fixed = TRUE)
  out <- list()
  testServer(env$server, {
    session$setInputs(d_style = "dusk", d_format = "png", d_draw = 1); session$flushReact()
    img <- output$d_map; out$alt <<- img$alt; p <- output$d_download; out$dl <<- basename(p)
    out$ok <<- file.exists(p) && file.size(p) > 20000 && identical(readBin(p, "raw", 4)[2:4], charToRaw("PNG"))
    session$setInputs(d_format = "../../etc/passwd"); out$bad <<- basename(output$d_download)
  })
  expect_match(out$alt, "Eastern Desert"); expect_true(out$ok); expect_match(out$dl, "^eastern_desert_modeled_routes_[0-9]{8}\\.png$"); expect_match(out$bad, "\\.png$")
})

test_that("SP2 the history tabs run Roman roads, Egyptian desert, Ancient cities, and no history page carries its own full screen button because Expand serves both map pages", {
  m <- as.character(env$history_page("classic"))
  pos <- vapply(c('data-value="roads"', 'data-value="desert"', 'data-value="cities"'), function(x) regexpr(x, m, fixed = TRUE)[[1]], 1); expect_true(all(pos > 0)); expect_false(is.unsorted(pos))
  expect_false(grepl("map-full-btn", m, fixed = TRUE)); expect_false(grepl("overmapsMapFull", env$app_js, fixed = TRUE))
  for (id in c("h_sb", "c_sb", "d_sb")) expect_match(m, paste0('id="', id, '"'), fixed = TRUE)
  expect_match(env$app_css, ":not([data-page='history']) .expand-btn { display: none; }", fixed = TRUE)
})

test_that("RS2 every download has a file type and a resolution side by side, and the choice reaches the file and its name", {
  ui <- as.character(env$ui(NULL)); m <- as.character(env$history_page("classic"))
  for (id in c("res")) expect_match(ui, sprintf('id="%s"', id), fixed = TRUE); for (id in c("h_res", "c_res", "d_res")) expect_match(m, sprintf('id="%s"', id), fixed = TRUE)
  expect_match(ui, "EPS", fixed = TRUE); expect_false(grepl("Vector files stay sharp", ui, fixed = TRUE)); expect_match(env$app_css, "grid-template-columns: 1fr 1fr; column-gap", fixed = TRUE)
  expect_false(grepl("(300 dpi)", paste(names(env$export_formats), collapse = " "), fixed = TRUE))
  out <- list()
  testServer(env$server, {
    session$setInputs(d_style = "classic", d_format = "png", d_res = "150"); session$flushReact(); p <- output$d_download; out$d150 <<- dim(png::readPNG(p))[1:2]; out$dn <<- basename(p)
    session$setInputs(d_res = "300"); out$d300 <<- basename(output$d_download); session$setInputs(d_res = "nonsense"); out$dbad <<- basename(output$d_download)
    session$setInputs(d_format = "pdf", d_res = "600"); out$dpdf <<- basename(output$d_download)
  })
  expect_equal(out$d150, c(1500, 1200)); expect_match(out$dn, "^eastern_desert_modeled_routes_150dpi_[0-9]{8}\\.png$"); expect_match(out$d300, "^eastern_desert_modeled_routes_[0-9]{8}\\.png$")
  expect_match(out$dbad, "^eastern_desert_modeled_routes_[0-9]{8}\\.png$"); expect_match(out$dpdf, "^eastern_desert_modeled_routes_[0-9]{8}\\.pdf$")
})

extras_res <- function() {
  w <- nat_world()
  ctr <- ctr_xy(); pt <- function(dx, dy) st_point(ctr + c(dx, dy))
  ln <- st_sf(osm_id = c("11", "12", "13"), highway = c("footway", "residential", "residential"), lit = c("yes", "yes", "no"), waterway = NA_character_, natural = NA_character_,
              geometry = st_sfc(st_linestring(rbind(ctr + c(-200, 0), ctr + c(200, 0))), st_linestring(rbind(ctr + c(-200, 100), ctr + c(200, 100))), st_linestring(rbind(ctr + c(-200, -100), ctr + c(200, -100))), crs = crs0))
  w$osm_lines <- dplyr::bind_rows(w$osm_lines, st_transform(ln, 4326))
  w$osm_points <- st_transform(st_sf(osm_id = paste0("p", 1:3), highway = c("street_lamp", "street_lamp", "bus_stop"), geometry = st_sfc(pt(10, 10), pt(-50, 20), pt(0, 0), crs = crs0)), 4326)
  w
}

test_that("CE1 one request asks for water, parks, lamps, and lit ways, and the answer is split for each map with only the columns used", {
  q <- env$site_extras_query(c(xmin = -80, ymin = 38.9, xmax = -79.8, ymax = 39), 60)
  for (w in c("coastline", "riverbank", "street_lamp", '["lit"]')) expect_match(q, w, fixed = TRUE)
  expect_false(grepl("building", q)); expect_equal(env$nature_query(c(xmin = -80, ymin = 38.9, xmax = -79.8, ymax = 39), 60), sub("\\);out body", ");out body", env$nature_query(c(xmin = -80, ymin = 38.9, xmax = -79.8, ymax = 39), 60)))
  e <- env$keep_site_extras(extras_res())
  expect_equal(sort(e$lit_ids), c("11", "12")); expect_equal(e$n_lit, 2L); expect_equal(e$n_lamps, 2L)
  expect_true(all(c("natural", "waterway") %in% names(e$nature$osm_lines))); expect_false("highway" %in% names(e$nature$osm_polygons))
  expect_true(all(names(e$lights$osm_points) %in% c("osm_id", "highway", "geometry")))
  nat <- env$prep_nature(e$nature, lon0, lat0, 4000, crs0); expect_false(is.null(nat$water)); expect_gt(nat$area_km2, 0)
  lg <- env$prep_lights(e$lights, lon0, lat0, 4000, crs0); expect_equal(nrow(lg$lamps), 2L)
  expect_equal(env$keep_site_extras(list())$n_lamps, 0L)
})

test_that("CE2 street lights is offered only for sites whose snapshot found lamps, and lit ids are put back on the streets", {
  withr::defer(env$history_cache$cities <- NULL)
  env$history_cache$cities <- list(pompeii = list(roads = list(osm_lines = NULL), radius_m = 800L, extras = list(radius_m = 800L, nature = list(), n_lamps = 6L, n_lit = 7L)),
                                   ostia = list(roads = list(osm_lines = NULL), radius_m = 1000L, extras = list(radius_m = 1000L, nature = list(), n_lamps = 2L, n_lit = 1L)))
  expect_true(env$city_has_lights("pompeii")); expect_false(env$city_has_lights("ostia")); expect_false(env$city_has_lights("timgad")); expect_false(env$city_has_lights(NULL))
  expect_true("lights" %in% env$city_view_choices("pompeii")$values); expect_false("lights" %in% env$city_view_choices("ostia")$values)
  expect_equal(length(env$city_view_choices("pompeii")$names), length(env$city_view_choices("pompeii")$values))
  expect_true(all(c("nature", "lights") %in% names(env$city_views))); expect_equal(env$get_city_view("nature"), "nature"); expect_equal(env$get_city_view("lights"), "lights")
  expect_null(env$bundled_extras(env$ancient_site("pompeii"), 900)); expect_false(is.null(env$bundled_extras(env$ancient_site("pompeii"), 800))); expect_null(env$bundled_extras(env$ancient_site("timgad"), 500))   # a larger radius than saved is not served
  l <- fixture_lines(); l$osm_id <- as.character(seq_len(nrow(l))); l$lit_tag <- FALSE
  expect_equal(sum(env$with_lit_ids(l, c("1", "2"))$lit_tag), 2L); expect_equal(sum(env$with_lit_ids(l, character())$lit_tag), 0L); expect_null(env$with_lit_ids(NULL, "1"))
})

test_that("CE3 an ancient city draws as nature & water and as street lights from one extra download, and falls back to the streets if it fails", {
  withr::defer({ env$ancient_sites <- orig_sites; env$history_cache$cities <- NULL }); orig_sites <- env$ancient_sites
  env$ancient_sites <- dplyr::bind_rows(orig_sites, tibble(id = "test", name = "Testville, Elkins", civ = "Roman", lat = lat0, lon = lon0, radius_km = 1.5, note = "A fixture."))
  env$history_cache$cities <- list()
  calls <- with_stubs(NULL)
  l <- fixture_lines(); l$osm_id <- as.character(seq_len(nrow(l)))
  env$fetch_roads <- function(lon, lat, radius_m, crs_m, shape = NULL, on_try = NULL, detail = "all") {
    calls$fetch[[length(calls$fetch) + 1]] <- list(radius_m = radius_m, detail = detail)
    if (identical(detail, "site")) env$keep_site_extras(extras_res()) else list(osm_lines = st_transform(l, 4326))
  }
  out <- list()
  testServer(env$server, {
    session$setInputs(h_tab = "cities", c_site = "test", c_view = "direction", c_style = "classic", c_format = "png", c_units = "km", c_radius = 1.5); session$flushReact()
    session$setInputs(c_draw = 1); session$flushReact()
    session$setInputs(c_view = "nature"); session$flushReact(); out$alt_n <<- output$c_map$alt; out$ok_n <<- is.character(output$c_map$src) && file.size(output$c_map$src) > 20000
    session$setInputs(c_view = "lights"); session$flushReact(); out$alt_l <<- output$c_map$alt; out$ok_l <<- is.character(output$c_map$src) && file.size(output$c_map$src) > 20000
    out$dl <<- basename(output$c_download)
    session$setInputs(c_view = "direction"); session$flushReact(); out$alt_d <<- output$c_map$alt
  })
  d <- vapply(calls$fetch, function(x) x$detail, "")
  expect_equal(sum(d == "ancient"), 1L); expect_equal(sum(d == "site"), 1L)                       # streets once, water and lamps once, shared by both map types
  expect_match(out$alt_n, "water, woods, and parks"); expect_match(out$alt_l, "street lamps"); expect_match(out$alt_d, "colored by direction")
  expect_true(out$ok_n); expect_true(out$ok_l); expect_match(out$dl, "^streets_test_lights_[0-9]{8}\\.png$")
  # a failing extra download keeps the streets on screen and says why
  env$history_cache$cities <- list(); calls2 <- with_stubs(NULL)
  env$fetch_roads <- function(lon, lat, radius_m, crs_m, shape = NULL, on_try = NULL, detail = "all") { if (identical(detail, "site")) stop(shiny::safeError("down")) else list(osm_lines = st_transform(l, 4326)) }
  out2 <- list()
  testServer(env$server, {
    session$setInputs(h_tab = "cities", c_site = "test", c_view = "nature", c_style = "classic", c_format = "png", c_units = "km", c_radius = 1.5); session$flushReact()
    session$setInputs(c_draw = 1); session$flushReact(); out2$ok <<- is.character(output$c_map$src) && file.size(output$c_map$src) > 20000
  })
  expect_true(out2$ok)
})

test_that("CE4 the file type menu has a non-selectable divider and alphabetical vector formats, and every pair of menus swaps its own resolution", {
  expect_equal(unname(env$export_formats), c("png", "jpeg", "tiff", "eps", "pdf", "svg")); expect_equal(unname(env$vector_formats), sort(unname(env$vector_formats)))
  h <- as.character(env$format_select("zz")); expect_match(h, 'value="" disabled', fixed = TRUE)
  expect_true(regexpr('disabled', h) > regexpr('value="tiff"', h, fixed = TRUE) && regexpr('disabled', h) < regexpr('value="eps"', h, fixed = TRUE))
  src <- strsplit(read_root("app.R"), "\n")[[1]]; i <- grep("^  sync_resolution <- function", src)
  expect_true(any(grepl("force\\(fmt_id\\); force\\(res_id\\)", src[i + 1])))                                 # without this, a loop variable left every menu watching the last pair
})

cvd_sim <- function(hex, mode) {
  m <- switch(mode,
    deutan = matrix(c(0.367322, 0.860646, -0.227968, 0.280085, 0.672501, 0.047413, -0.011820, 0.042940, 0.968881), 3, byrow = TRUE),
    protan = matrix(c(0.152286, 1.052583, -0.204868, 0.114503, 0.786281, 0.099216, -0.003882, -0.048116, 1.051998), 3, byrow = TRUE),
    tritan = matrix(c(1.255528, -0.076749, -0.178779, -0.078411, 0.930809, 0.147602, 0.004733, 0.691367, 0.303900), 3, byrow = TRUE),
    diag(3))
  rgb <- t(grDevices::col2rgb(hex)) / 255; lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  out <- pmin(1, pmax(0, lin %*% t(m)))
  l <- (out %*% t(matrix(c(0.4122214708, 0.5363325363, 0.0514459929, 0.2119034982, 0.6806995451, 0.1073969566, 0.0883024619, 0.2817188376, 0.6299787005), 3, byrow = TRUE)))^(1 / 3)
  cbind(L = 0.2104542553 * l[, 1] + 0.7936177850 * l[, 2] - 0.0040720468 * l[, 3], a = 1.9779984951 * l[, 1] - 2.4285922050 * l[, 2] + 0.4505937099 * l[, 3],
        b = 0.0259040371 * l[, 1] + 0.7827717662 * l[, 2] - 0.8086757660 * l[, 3])
}

cvd_gap <- function(h1, h2, mode) 100 * sqrt(sum((cvd_sim(h1, mode) - cvd_sim(h2, mode))^2))

cvd_min_adjacent <- function(cols, mode, wrap = FALSE) {
  n <- length(cols); idx <- cbind(seq_len(n - 1), seq_len(n - 1) + 1); if (wrap) idx <- rbind(idx, c(n, 1))
  min(vapply(seq_len(nrow(idx)), function(k) cvd_gap(cols[idx[k, 1]], cols[idx[k, 2]], mode), 0))
}

test_that("CV1 a color vision mode changes only the colors that carry meaning, and bad values fall back to Standard", {
  expect_equal(unname(env$cvd_modes), c("standard", "deutan", "protan", "tritan", "gray"))
  expect_equal(env$split_style("dusk~tritan"), list(name = "dusk", mode = "tritan")); expect_equal(env$split_style("dusk"), list(name = "dusk", mode = "standard"))
  expect_equal(env$split_style("nope~blue")$name, "classic"); expect_equal(env$split_style("dusk~nope")$mode, "standard"); expect_equal(env$split_style(NULL)$mode, "standard"); expect_equal(env$split_style(NA)$name, "classic")
  expect_equal(env$style_key("dusk", "tritan"), "dusk~tritan"); expect_equal(env$style_key("dusk", "standard"), "dusk"); expect_equal(env$style_key("dusk", NULL), "dusk"); expect_equal(env$style_key("zzz", "../x"), "classic")
  for (nm in names(env$styles)) {
    std <- env$get_style(nm); expect_identical(std, env$styles[[nm]])
    for (md in c("deutan", "protan", "tritan", "gray")) {
      p <- env$get_style(paste0(nm, "~", md))
      expect_equal(p$bg, std$bg); expect_equal(p$ink, std$ink); expect_equal(p$label, std$label)                       # the paper and the text never change
      expect_equal(lengths(p[c("cols", "speed", "dir", "dens")]), lengths(std[c("cols", "speed", "dir", "dens")]))
      expect_setequal(names(p$nature), names(std$nature)); expect_setequal(names(p$night), names(std$night))
      expect_true(all(grepl("^#[0-9a-fA-F]{6}$", unlist(p[c("cols", "speed", "dir", "dens", "nature", "night")]))))
    }
  }
  expect_identical(env$get_style("dusk~deutan"), env$get_style("dusk~protan"))
})

test_that("CV2 every color vision palette keeps its neighbors apart when seen the way that person sees them", {
  floor_gap <- 6                                                     # the lowest gap a pair may have; the palettes were built to reach about 8 or more
  for (nm in c("classic", "dusk")) {
    for (md in c("deutan", "protan", "tritan")) {
      p <- env$get_style(paste0(nm, "~", md)); lab <- paste(nm, md)
      expect_gte(cvd_min_adjacent(p$speed, md), floor_gap); expect_gte(cvd_min_adjacent(p$dir, md, wrap = TRUE), floor_gap)
      expect_gte(cvd_min_adjacent(p$cols, md), floor_gap); expect_gte(cvd_min_adjacent(p$dens, md), floor_gap)
      n <- p$nature; pairs <- list(c("water", "wood"), c("water", "park"), c("wood", "park"), c("river", "wood"), c("river", "park"), c("river", "street"), c("water", "street"))
      for (pr in pairs) expect_gte(cvd_gap(n[[pr[1]]], n[[pr[2]]], md), floor_gap)
    }
    g <- env$get_style(paste0(nm, "~gray"))
    expect_true(all(vapply(unlist(g[c("cols", "speed", "dir", "dens", "nature")]), function(h) { v <- grDevices::col2rgb(h)[, 1]; v[1] == v[2] && v[2] == v[3] }, TRUE)))     # truly gray
    expect_gte(cvd_min_adjacent(g$cols, "none"), floor_gap); expect_gte(cvd_min_adjacent(g$speed, "none"), floor_gap); expect_gte(cvd_min_adjacent(g$dens, "none"), floor_gap)
    n <- g$nature; for (pr in list(c("water", "wood"), c("water", "park"), c("wood", "park"), c("river", "wood"), c("river", "park"), c("river", "street"), c("water", "street"))) expect_gte(cvd_gap(n[[pr[1]]], n[[pr[2]]], "none"), floor_gap)
    expect_equal(g$night$glow, "#ffffff")                              # a lamp glows white when there is no hue to carry it
  }
})

test_that("CV3 the header has the Color vision menu and the page tabs, and the Home page names the source of every map set", {
  ui <- as.character(env$ui(NULL))
  expect_match(ui, 'id="cvd"', fixed = TRUE); expect_match(ui, "Color vision", fixed = TRUE)
  for (lab in c("Standard", "Deuteranopia", "Protanopia", "Tritanopia", "Grayscale")) expect_match(ui, lab, fixed = TRUE)
  pos <- vapply(c('data-value="home"', 'data-value="current"', 'data-value="history"', 'data-value="settings"', 'data-value="help"'), function(x) regexpr(x, ui, fixed = TRUE)[[1]], 1); expect_true(all(pos > 0)); expect_false(is.unsorted(pos))
  expect_match(ui, 'id="go_current"', fixed = TRUE); expect_match(ui, 'id="go_history"', fixed = TRUE); expect_match(ui, "Geek out on streets", fixed = TRUE)
  home <- as.character(env$home_page())
  for (src in c("OpenStreetMap contributors", "Photon", "Census Bureau", "Itiner-e", "Hotham", "Mani\u00e8re", "zenodo.4291876", "Overpass")) expect_match(home, src, fixed = TRUE)
  expect_equal(lengths(regmatches(home, gregexpr("home-source", home, fixed = TRUE))), 2L)
  expect_false(grepl("source-note", ui, fixed = TRUE)); expect_gte(lengths(regmatches(ui, gregexpr("About this map", ui, fixed = TRUE))), 4L)      # the sources live in About this map on every map page
  expect_false(grepl("history_open", ui, fixed = TRUE)); expect_false(grepl("Explore history", ui, fixed = TRUE))
  expect_match(env$app_css, "data-page='current'", fixed = TRUE); expect_match(env$app_js, "data-page", fixed = TRUE); expect_false(grepl("shiny-modal", env$app_css, fixed = TRUE))
})

test_that("CV4 the chosen color vision reaches every page's drawing key, and a share link carries it", {
  out <- list()
  testServer(env$server, {
    session$setInputs(style = "dusk", cvd = "tritan", c_style = "classic", h_style = "dusk", d_style = "classic"); session$flushReact()
    out$main <<- sid(); out$city <<- c_style(); out$roads <<- h_style(); out$desert <<- d_style()
    session$setInputs(cvd = "../../x"); out$bad <<- sid(); session$setInputs(cvd = "gray"); out$gray <<- sid()
  })
  expect_equal(out$main, "dusk~tritan"); expect_equal(out$city, "classic~tritan"); expect_equal(out$roads, "dusk~tritan"); expect_equal(out$desert, "classic~tritan"); expect_equal(out$bad, "dusk"); expect_equal(out$gray, "dusk~gray")
  base <- list(query = "Bloomington, IN", radius = 2, units = "km", view = "speed", style = "dusk", area = "dim")
  p <- env$share_params(c(base, list(cvd = "deutan")), 1L); expect_equal(p$cv, "deutan"); expect_null(env$share_params(c(base, list(cvd = "standard")), 1L)$cv)
  st <- env$parse_share(sub("^\\?", "", env$share_url(p, ""))); expect_equal(st$cv, "deutan"); expect_equal(st$s, "dusk")
  expect_null(env$parse_share("q=Bloomington&cv=purple")$cv); expect_null(env$parse_share("q=Bloomington&cv=%3Cscript%3E")$cv)
})

test_that("CV5 maps draw in every color vision mode, and a deep link opens the current maps", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point()
  for (k in c("classic~deutan", "dusk~protan", "classic~tritan", "dusk~gray", "classic~gray")) { f <- tempfile(fileext = ".png"); env$draw_figure(p, f, "png", k, preview = TRUE); expect_true(file.exists(f) && file.size(f) > 2000) }
  expect_equal(env$night_palette(env$get_style("classic~gray"), "classic~gray")$glow, "#ffffff"); expect_equal(env$night_palette(env$get_style("dusk"), "dusk")$glow, env$styles$dusk$night$glow)
  src <- paste(readLines(Sys.getenv("APP_FILE"), warn = FALSE), collapse = "\n")
  expect_match(src, 'nav_select("page", "current")', fixed = TRUE); expect_match(src, 'observeEvent(input$go_history, nav_select("page", "history"))', fixed = TRUE)
})

test_that("HP1 the Help tab follows the other apps: topics, questions that open one at a time, sources, and an About block", {
  h <- as.character(env$help_page())
  for (topic in c("Getting started", "Reading the maps", "Historical maps", "Downloads and sharing", "Privacy and problems", "Sources and credits")) expect_match(h, topic, fixed = TRUE)
  expect_gte(lengths(regmatches(h, gregexpr("<details", h, fixed = TRUE))), 20L); expect_match(h, "<summary>", fixed = TRUE)
  for (x in c("Itiner-e", "Hotham", "OpenStreetMap contributors", "Census Bureau", "Iconify", "Noto", "PolyForm Noncommercial License 1.0.0", "releases/latest", "Copyright 2026 Abhik Roy", env$OVERMAPS_VERSION)) expect_match(h, x, fixed = TRUE)
  expect_false(grepl("[\u2013\u2014]", h)); expect_false(grepl("\\b(can't|don't|won't|isn't|it's|you're|doesn't)\\b", h, ignore.case = TRUE))
  expect_false(grepl("\\b(actionable|robust|ensure[sd]?|leverag|utiliz|enhanc|seamless)", h, ignore.case = TRUE))
  ui <- as.character(env$ui(NULL)); expect_match(ui, 'id="go_help"', fixed = TRUE);
})

test_that("HP2 Home has the pastel icons, the quiet map background, and no Good to know block", {
  home <- as.character(env$home_page()); expect_match(home, 'src="icons/map-current.svg"', fixed = TRUE); expect_match(home, 'src="icons/map-historical.svg"', fixed = TRUE)
  expect_false(grepl("Good to know", home, fixed = TRUE)); expect_false(grepl("fa-", home, fixed = TRUE)); expect_match(env$app_css, "home-map.svg", fixed = TRUE)
  for (f in c("icons/map-current.svg", "icons/map-historical.svg", "home-map.svg", "favicon.svg", "favicon.ico", "favicon-32.png", "apple-touch-icon.png")) expect_true(file.exists(file.path(dirname(Sys.getenv("APP_FILE")), "www", f)), info = f)
  src <- paste(readLines(Sys.getenv("APP_FILE"), warn = FALSE), collapse = "\n")                    # head tags are lifted out of the page body, so they are read from the source
  for (x in c('href = "favicon.svg"', 'href = "favicon.ico"', 'href = "apple-touch-icon.png"', 'href = "favicon-32.png"')) expect_match(src, x, fixed = TRUE)
})

test_that("HP3 the color vision choice recolors the page itself, not only the maps", {
  for (cls in c("cb-deutan", "cb-protan", "cb-tritan", "cb-gray")) expect_match(env$app_css, paste0("body.", cls), fixed = TRUE)
  expect_match(env$app_css, ".btn-primary", fixed = TRUE); expect_match(env$app_js, "cvdClass", fixed = TRUE)
  src <- paste(readLines(Sys.getenv("APP_FILE"), warn = FALSE), collapse = "\n"); expect_match(src, 'sendCustomMessage("cvdClass"', fixed = TRUE)
})

test_that("HP4 the header and both map pages share one frame, and the historical sets switch from the sidebar", {
  m <- as.character(env$history_page("classic"))
  expect_equal(lengths(regmatches(m, gregexpr('class="set-switch"', m, fixed = TRUE))), 3L); expect_equal(lengths(regmatches(m, gregexpr("set-btn is-active", m, fixed = TRUE))), 3L)
  expect_false(grepl("history-intro", m, fixed = TRUE)); expect_equal(lengths(regmatches(m, gregexpr('accordion-title">About this map', m, fixed = TRUE))), 3L)
  expect_match(env$app_css, "--nav-h", fixed = TRUE); expect_match(env$app_js, "overmapsSet", fixed = TRUE)
  out <- list(); testServer(env$server, { session$setInputs(h_set = "desert"); session$flushReact(); session$setInputs(h_set = "../x"); session$flushReact(); out$ok <<- TRUE }); expect_true(out$ok)
})

test_that("HP5 an ancient site's starting radius is fitted to what is mapped there, just past the edge", {
  p <- env$ancient_sites[env$ancient_sites$id == "pompeii", ]; lon <- p$lon; lat <- p$lat
  crs <- env$utm_epsg(lon, lat); ctr <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326), crs))[1, 1:2]
  mk <- function(x0, y0, x1, y1) sf::st_linestring(rbind(ctr + c(x0, y0), ctr + c(x1, y1)))
  lines <- sf::st_transform(sf::st_sf(osm_id = as.character(1:12), geometry = sf::st_sfc(c(lapply(1:6, function(i) mk(-400, i * 60 - 200, 400, i * 60 - 200)), lapply(1:6, function(i) mk(i * 60 - 200, -400, i * 60 - 200, 400))), crs = crs)), 4326)
  r <- env$fit_radius_km(lines, lon, lat, 3000); expect_gte(r, 0.4); expect_lte(r, 0.6)                 # the streets reach about 0.45 km, so the circle ends a little past that
  expect_true(is.na(env$fit_radius_km(lines, lon, lat, 450)));   # streets that run to the edge of the snapshot say nothing about where the remains end expect_true(is.na(env$fit_radius_km(NULL, lon, lat))); expect_true(is.na(env$fit_radius_km(lines[1:3, ], lon, lat)))
  withr::defer({ env$history_cache$cities <- NULL; env$history_cache$fit <- NULL })
  env$history_cache$cities <- list(pompeii = list(roads = list(osm_lines = lines), taken = "x", radius_m = 3000L))
  s <- env$ancient_site("pompeii"); expect_equal(s$listed_radius_km, 0.8); expect_gte(s$radius_km, 0.4); expect_lte(s$radius_km, 0.6)
  plan <- env$city_radius_plan(s); expect_equal(plan$value, s$radius_km)
  env$history_cache$cities <- list(); env$history_cache$fit <- NULL; expect_equal(env$ancient_site("pompeii")$radius_km, 0.8)       # no snapshot keeps the listed radius
})

test_that("ST1 Settings holds the color vision, privacy and counts, the header ends with Settings, Help and the page toggle, and the empty pages center the graphic over the text", {
  ui <- as.character(env$ui(NULL))
  settings <- as.character(env$settings_page())
  for (id in c('id="cvd"', 'id="suggest"', 'id="remember"', 'id="forget"', 'id="counts"')) expect_match(settings, id, fixed = TRUE)
  expect_false(grepl("cvd-pick", sub(".*<nav", "", regmatches(ui, regexpr("<nav.*?</nav>", ui))[1]), fixed = TRUE))     # the header no longer carries the Color vision menu
  tail_pos <- vapply(c('id="expand-btn"', 'data-value="settings"', 'data-value="help"', 'id="theme-btn"'), function(x) regexpr(x, ui, fixed = TRUE)[[1]], 1)
  expect_true(all(tail_pos > 0)); expect_false(is.unsorted(tail_pos))
  expect_false(grepl("Settings and privacy", ui, fixed = TRUE))
  css <- env$app_css
  expect_match(css, "#map:empty::before", fixed = TRUE); expect_match(css, ".history-map .shiny-image-output:empty::before", fixed = TRUE)
  expect_match(css, ".stage:not(:has(.stats-slot > *))", fixed = TRUE); expect_match(css, ".history-page .bslib-sidebar-layout { height: calc(100vh - var(--nav-h)) !important", fixed = TRUE)
})


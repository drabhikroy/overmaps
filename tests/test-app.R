# Test suite for app.R. Run from the project root: Rscript tests/run_tests.R
suppressMessages({ library(testthat); library(shiny); library(sf); library(dplyr); library(tibble) })
app_file <- Sys.getenv("APP_FILE", if (file.exists("app.R")) "app.R" else "../app.R")
Sys.setenv(OVERMAPS_DATA = file.path(dirname(normalizePath(app_file)), "data"))      # the bundled history data sits next to the app, whatever folder the tests run from
Sys.setenv(STREET_MAPS_DATA = file.path(tempdir(), "qa_data"))      # downloads in tests must never touch a real download log
env <- get0("qa_env", envir = globalenv(), ifnotfound = NULL)      # coverage runs inject an instrumented env
if (is.null(env)) { env <- new.env(parent = globalenv()); suppressMessages(sys.source(app_file, envir = env)) }

# ---- Fixtures ---------------------------------------------------------------
lon0 <- -79.8467; lat0 <- 38.9262
crs0 <- 32617
ctr <- st_transform(st_sfc(st_point(c(lon0, lat0)), crs = 4326), crs0)
cx <- st_coordinates(ctr)[1]; cy <- st_coordinates(ctr)[2]
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

# ---- R1 projection choice ---------------------------------------------------
test_that("R1 utm_epsg returns the correct UTM zone (independent known values)", {
  expect_equal(env$utm_epsg(-79.8467, 38.9262), 32617)   # Elkins, WV: zone 17N
  expect_equal(env$utm_epsg(-0.12, 51.5), 32630)         # London: 30N
  expect_equal(env$utm_epsg(151.2, -33.9), 32756)        # Sydney: 56S
  expect_equal(env$utm_epsg(139.7, 35.7), 32654)         # Tokyo: 54N
  expect_equal(env$utm_epsg(0, 10), 32631)               # prime meridian belongs to zone 31
  expect_equal(env$utm_epsg(-180, 10), 32601)
})
test_that("R1b utm_epsg never returns a nonexistent zone (zones are 1-60)", {
  z <- function(e) e %% 100
  expect_lte(z(env$utm_epsg(180, 10)), 60)
  expect_lte(z(env$utm_epsg(179.99999, 10)), 60)
})

# ---- R2 road classes --------------------------------------------------------
test_that("R2 role_of matches an independent class table", {
  tbl <- c(motorway = 1, trunk = 1, motorway_link = 1, trunk_link = 1,
           primary = 2, primary_link = 2, secondary = 3, secondary_link = 3,
           tertiary = 4, tertiary_link = 4, unclassified = 5, residential = 5, service = 5, footway = 5)
  got <- as.integer(factor(env$role_of(names(tbl)), levels = env$roles$role))
  expect_equal(got, unname(tbl))
})

# ---- R3 titles --------------------------------------------------------------
test_that("R3 short_title", {
  expect_equal(env$short_title("Elkins, Randolph County, West Virginia, United States"), "Elkins, West Virginia")
  expect_equal(env$short_title("Paris, France"), "Paris, France")
  expect_equal(env$short_title("Paris"), "Paris")
  expect_equal(env$short_title("Springfield, Hampden County, Massachusetts, United States"), "Springfield, Massachusetts")
})

# ---- R4 data preparation ----------------------------------------------------
test_that("R4 prep_lines clips inside the ring, keeps lengths consistent, classifies", {
  lines <- env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0)
  expect_s3_class(lines, "sf")
  xy <- st_coordinates(lines)
  d <- sqrt((xy[, "X"] - cx)^2 + (xy[, "Y"] - cy)^2)
  expect_lte(max(d), 3000 * 0.988 + 0.5)                          # nothing past the clip circle
  expect_gt(max(d), 3000 * 0.988 - 5)                             # long roads do reach it (not over-clipped)
  expect_equal(sum(lines$km), sum(as.numeric(st_length(lines))) / 1000, tolerance = 1e-9)
  expect_equal(as.character(lines$role[lines$highway == "service"]), "Local streets")
  # independent length of the primary road: a 6 km road clipped to a circle of radius 2964 m => 5.928 km
  expect_equal(sum(lines$km[lines$highway == "primary"]), 2 * 3000 * 0.988 / 1000, tolerance = 1e-3)
})
test_that("R4b prep_lines handles empty, point-only, NA-class and missing-name input", {
  expect_null(env$prep_lines(list(osm_lines = NULL), lon0, lat0, 3000, crs0))
  expect_null(env$prep_lines(list(osm_lines = fixture_roads()$osm_lines[0, ]), lon0, lat0, 3000, crs0))
  far <- mk(cbind(cx + c(9000, 9500), cy + c(9000, 9000)), "residential")           # entirely outside the ring
  expect_null(env$prep_lines(fixture_roads(far), lon0, lat0, 3000, crs0))
  l <- fixture_lines(); l$highway[1] <- NA
  expect_equal(nrow(env$prep_lines(fixture_roads(l), lon0, lat0, 3000, crs0)), nrow(env$prep_lines(fixture_roads(fixture_lines()), lon0, lat0, 3000, crs0)) - 1)
  nn <- fixture_roads(); nn$osm_lines$name <- NULL
  expect_s3_class(env$prep_lines(nn, lon0, lat0, 3000, crs0), "sf")
})
test_that("R4c metamorphic: more radius => at least as much street; splitting a line keeps length", {
  r <- fixture_roads()
  k <- function(m) sum(env$prep_lines(r, lon0, lat0, m, crs0)$km)
  expect_lte(k(1000), k(2000)); expect_lte(k(2000), k(5000))
  l <- fixture_lines(); half <- bind_rows(mk(cbind(cx + c(-1000, 0), cy + c(300, 300)), "residential"),
                                          mk(cbind(cx + c(0, 1000), cy + c(300, 300)), "residential"))
  whole <- mk(cbind(cx + c(-1000, 1000), cy + c(300, 300)), "residential")
  a <- sum(env$prep_lines(fixture_roads(half), lon0, lat0, 3000, crs0)$km)
  b <- sum(env$prep_lines(fixture_roads(whole), lon0, lat0, 3000, crs0)$km)
  expect_equal(a, b, tolerance = 1e-9)
})

# ---- R5 map ----------------------------------------------------------------
render_png <- function(p, w = 8, h = 10, dpi = 100) {
  f <- tempfile(fileext = ".png")
  showtext::showtext_opts(dpi = dpi)
  ggplot2::ggsave(f, p, width = w, height = h, dpi = dpi, bg = env$bg, device = ragg::agg_png)
  f
}
subtitle_of <- function(p) p$patches$annotation$subtitle
title_of <- function(p) p$patches$annotation$title

test_that("R5 make_map subtitle numbers agree with independent arithmetic", {
  lines <- env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0)
  p <- env$make_map(lines, "Test, Place", lon0, lat0, 3000, crs0)
  tot <- sum(as.numeric(st_length(lines))) / 1000
  loc <- sum(as.numeric(st_length(lines[lines$highway %in% c("residential", "unclassified", "service"), ]))) / 1000
  thr <- sum(as.numeric(st_length(lines[lines$highway %in% c("trunk", "primary", "secondary"), ]))) / 1000
  s <- as.character(subtitle_of(p))
  expect_match(s, sprintf("^%d km of street within 3 km", round(tot)))
  expect_match(s, sprintf("%d%% of it is local", round(100 * loc / tot)))
  expect_match(s, sprintf("add up to %d km", round(thr)))
  expect_equal(as.character(title_of(p)), "Test, Place")
})
test_that("R5b make_map renders at screen and print size across radii and degenerate inputs", {
  for (rad in c(1000, 2500, 5000)) {
    lines <- env$prep_lines(fixture_roads(), lon0, lat0, rad, crs0)
    expect_s3_class(env$make_map(lines, "Test", lon0, lat0, rad, crs0), "ggplot")
  }
  only_local <- fixture_lines() |> filter(highway %in% c("residential", "unclassified"))
  p <- env$make_map(env$prep_lines(fixture_roads(only_local), lon0, lat0, 3000, crs0), "Local only", lon0, lat0, 3000, crs0)
  f <- render_png(p); expect_true(file.exists(f)); expect_gt(file.size(f), 5000)
  one <- mk(cbind(cx + c(-500, 500), cy + c(0, 0)), "primary", "Solo Road")
  p <- env$make_map(env$prep_lines(fixture_roads(one), lon0, lat0, 3000, crs0), "One road", lon0, lat0, 3000, crs0)
  expect_gt(file.size(render_png(p)), 5000)
})
test_that("R5c labels: unicode and hostile road names do not crash; labels stay inside the ring", {
  l <- fixture_lines()
  l$name[l$name %in% "Davis Avenue"] <- "Şehit Çarşı Caddesi 大通り شارع"
  l$name[l$name %in% "Beverly Pike"] <- "<b>Pike</b> & *Co* `x`"
  lines <- env$prep_lines(fixture_roads(l), lon0, lat0, 3000, crs0)
  lab <- env$make_labels(lines, 3000, crs0, ctr)
  expect_true(is.null(lab) || all(sqrt((lab$x - cx)^2 + (lab$y - cy)^2) < 3000))
  p <- env$make_map(lines, "Unicode", lon0, lat0, 3000, crs0)
  expect_gt(file.size(render_png(p)), 5000)
})

# ---- S1 security: markup injection through the place name -------------------
test_that("S1 place-name markup is not interpreted (no local image embedding via <img>)", {
  skip_if_not_installed("png")
  red <- tempfile(fileext = ".png"); ragg::agg_png(red, 200, 200); grid::grid.rect(gp = grid::gpar(fill = "#ff0000", col = NA)); invisible(dev.off())
  lines <- env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0)
  evil <- sprintf("<img src='%s' width='300'>", red)
  img <- png::readPNG(render_png(env$make_map(lines, evil, lon0, lat0, 3000, crs0)))
  pure_red <- sum(img[, , 1] > 0.95 & img[, , 2] < 0.05 & img[, , 3] < 0.05)
  expect_equal(pure_red, 0)
})
test_that("S1b markdown characters in a place name render literally", {
  lines <- env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0)
  p <- env$make_map(lines, "Saint_Louis *Park*", lon0, lat0, 3000, crs0)
  # The title element must not turn the text into markup: either it is escaped or not a markdown textbox.
  html <- commonmark::markdown_html(as.character(title_of(p)))
  expect_false(grepl("<em>|<strong>|<img|<b>|<code>", html))
  expect_match(html, "Saint_Louis \\*Park\\*", fixed = FALSE)   # characters survive literally
  for (name in c("<b>bold</b>", "a & b", "back\\slash", "[link](x)", "# heading", "`code`", "~~s~~", "x|y"))
    expect_false(grepl("<(b|a|h1|code|del|em|strong)[ >]", commonmark::markdown_html(env$escape_markup(name), extensions = TRUE)), info = name)
})

# ---- S2 security / robustness: server-side input validation -----------------
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
test_that("S2 radius is clamped server-side (client slider can be bypassed)", {
  calls <- with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 1e6, labels = TRUE, draw = 1)
    try(built(), silent = TRUE)
  })
  expect_true(length(calls$fetch) == 0 || calls$fetch[[1]]$radius_m <= 50000)
  calls <- with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = -5, labels = TRUE, draw = 1)
    try(built(), silent = TRUE)
  })
  expect_true(length(calls$fetch) == 0 || calls$fetch[[1]]$radius_m >= 500)
})
test_that("S2b malformed radius/choice give a validation message, not a crash or a bogus request", {
  for (bad in list(NA, "abc", NULL)) {
    calls <- with_stubs(NULL)
    testServer(env$server, {
      session$setInputs(query = "Elkins", find = 1)
      session$setInputs(choice = "1", radius = bad, labels = TRUE, draw = 1)
      expect_error(built(), class = "shiny.silent.error")
    })
    expect_length(calls$fetch, 0)
  }
  for (bad in c("99", "abc", "0", "-1")) {
    calls <- with_stubs(NULL)
    testServer(env$server, {
      session$setInputs(query = "Elkins", find = 1)
      session$setInputs(choice = bad, radius = 3, labels = TRUE, draw = 1)
      expect_error(built(), class = "shiny.silent.error")
    })
    expect_length(calls$fetch, 0)
  }
})
test_that("S2c oversized search text is not forwarded to the geocoder", {
  calls <- with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = strrep("a", 5000), find = 1)
    try(places(), silent = TRUE)
  })
  expect_true(all(nchar(calls$geo) <= 300))
})
test_that("S2d polar / unsupported-UTM latitudes are refused with a message", {
  for (lat in c(85, -85, 89.9)) {
    calls <- with_stubs(NULL)
    env$geocode <- function(q) tibble(label = "Pole, Somewhere", lat = lat, lon = 10)
    testServer(env$server, {
      session$setInputs(query = "pole", find = 1)
      session$setInputs(choice = "1", radius = 3, labels = TRUE, draw = 1)
      expect_error(built(), class = "shiny.silent.error")
    })
    expect_length(calls$fetch, 0)
  }
})

# ---- F1 server workflow -----------------------------------------------------
test_that("F1 find -> choose -> draw produces a map using the chosen candidate", {
  calls <- with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    expect_equal(nrow(places()), 2)
    session$setInputs(choice = "2", radius = 2, labels = FALSE, draw = 1)
    expect_error(built(), class = "shiny.silent.error")   # the stub's roads are near Elkins, not the 2nd candidate
  })
  expect_equal(calls$fetch[[1]]$lon, 10); expect_equal(calls$fetch[[1]]$radius_m, 2000)
  expect_equal(calls$fetch[[1]]$crs_m, 32632)
})
test_that("F1a find -> draw with the first candidate renders a map", {
  with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, draw = 1)
    expect_s3_class(built(), "ggplot")
  })
})
test_that("F1b draw before find gives a validation message", {
  with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(draw = 1, choice = "1", radius = 3, labels = TRUE)
    expect_error(built(), class = "shiny.silent.error")
  })
})
test_that("F1c empty OSM result gives a message", {
  with_stubs(NULL)
  env$fetch_roads <- function(...) list(osm_lines = NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, draw = 1)
    expect_error(built(), class = "shiny.silent.error")
  })
})
test_that("F2 download produces a 2400 x 3000 px PNG", {
  with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, draw = 1)
    f <- output$download
    expect_true(file.exists(f))
    dim <- dim(png::readPNG(f)); expect_equal(dim[1:2], c(3000, 2400))
  })
})

# ---- I1 integration: failure and cache --------------------------------------
test_that("I1 fetch_roads serves a cached result without network", {
  key <- file.path(env$roads_cache_dir(), sprintf("roads_%.4f_%.4f_%d.rds", 12.3456, 45.6789, 2000L))
  saveRDS(list(marker = "cached"), key)
  expect_equal(env$fetch_roads(12.3456, 45.6789, 2000L, 32633)$marker, "cached")
})
test_that("I1c geocode failure path: unreachable service errors (caught by the app) rather than hanging", {
  skip_if(Sys.getenv("QA_ONLINE") == "1", "online run")
  skip_on_ci()   # a hosted runner has internet access, so the services are reachable there
  t0 <- Sys.time(); expect_error(env$geocode("Elkins")); expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 25)
})

# ---- P1 performance ---------------------------------------------------------
test_that("P1 a dense synthetic 5 km city draws in reasonable time", {
  skip_if(Sys.getenv("OVERMAPS_SKIP_PERF") == "1", "performance checks skipped")
  set.seed(7)
  n <- 4000; L <- vector("list", n)
  for (i in seq_len(n)) {
    s <- cbind(cx + runif(1, -5000, 5000) + cumsum(rnorm(20, 0, 40)), cy + runif(1, -5000, 5000) + cumsum(rnorm(20, 0, 40)))
    L[[i]] <- mk(s, sample(c("residential", "residential", "residential", "tertiary", "secondary", "primary"), 1), if (i %% 40 == 0) paste("Road", i) else NA_character_)
  }
  r <- fixture_roads(bind_rows(L))
  t_prep <- system.time(lines <- env$prep_lines(r, lon0, lat0, 5000, crs0))[["elapsed"]]
  t_map <- system.time(p <- env$make_map(lines, "Dense", lon0, lat0, 5000, crs0))[["elapsed"]]
  t_png <- system.time(render_png(p, dpi = 80))[["elapsed"]]
  message(sprintf("PERF prep %.1fs  build %.1fs  render(80dpi) %.1fs  segments~%d", t_prep, t_map, t_png, nrow(lines)))
  expect_lt(t_prep + t_map + t_png, 90)
})

# ---- Clipping: no street length may be lost when clipping -----------
test_that("R4d clipping conserves length for multi-part, self-crossing and mixed geometries", {
  set.seed(3); L <- vector("list", 150)
  for (i in seq_along(L)) L[[i]] <- mk(cbind(cx + runif(1, -2500, 2500) + cumsum(rnorm(20, 0, 40)), cy + runif(1, -2500, 2500) + cumsum(rnorm(20, 0, 40))), "residential")
  r <- fixture_roads(bind_rows(L))
  clip <- st_buffer(ctr, 3000 * 0.988)
  indep <- sum(as.numeric(st_length(st_collection_extract(suppressWarnings(st_intersection(st_geometry(st_transform(r$osm_lines, crs0)), clip)), "LINESTRING"))))
  got <- sum(env$prep_lines(r, lon0, lat0, 3000, crs0)$km) * 1000
  expect_equal(got, indep, tolerance = 1e-6)
})
test_that("R4e a road that leaves and re-enters the ring keeps both pieces", {
  pts <- rbind(c(-3500, 2000), c(-1500, 2000), c(-600, 3200), c(600, 3200), c(1500, 2000), c(3500, 2000))
  u <- mk(cbind(cx + pts[, 1], cy + pts[, 2]), "residential", "U Street")
  lines <- env$prep_lines(fixture_roads(u), lon0, lat0, 3000, crs0)
  expect_equal(nrow(lines), 2)
  expect_equal(sum(lines$km) * 1000, 3454, tolerance = 0.005)       # independent dense-sampling value
})
test_that("R4f explode_lines handles GEOMETRYCOLLECTION and non-line parts", {
  gc <- st_sf(id = 1, geometry = st_sfc(st_geometrycollection(list(st_point(c(0, 0)), st_linestring(rbind(c(0, 0), c(0, 10))), st_linestring(rbind(c(5, 0), c(5, 3))))), crs = 32617))
  out <- env$explode_lines(gc)
  expect_equal(nrow(out), 2); expect_equal(sum(as.numeric(st_length(out))), 13)
})

# ---- Cache ------------------------------------------------------
test_that("I2 prune_cache keeps only the newest files", {
  d <- env$roads_cache_dir(); unlink(list.files(d, pattern = "^roads_.*\\.rds$", full.names = TRUE))   # isolate from other tests' caches
  f <- file.path(d, sprintf("roads_prune_%02d.rds", 1:15)); for (x in f) saveRDS(1, x)
  Sys.setFileTime(f, Sys.time() - (15:1) * 10)
  removed <- env$prune_cache(keep = 5)
  left <- list.files(d, pattern = "^roads_prune_"); expect_equal(length(left), 5)
  expect_setequal(left, basename(f)[11:15])
})

# ---- boundary: radius limits --------------------------------------------------
test_that("S2e radius boundaries: 0.25 -> 0.5 km, 0.5 -> 0.5 km, 5.5 -> 5.5 km, 60 -> 50 km (the absolute limits)", {
  for (case in list(c(0.25, 500), c(0.5, 500), c(1, 1000), c(5.5, 5500), c(2.5, 2500), c(60, 50000))) {
    calls <- with_stubs(NULL)
    testServer(env$server, {
      session$setInputs(query = "Elkins", find = 1)
      session$setInputs(choice = "1", radius = case[1], labels = TRUE, draw = 1)
      try(built(), silent = TRUE)
    })
    expect_equal(calls$fetch[[1]]$radius_m, case[2])
  }
})

# ---- extra checks: security and edge cases ---------------
test_that("S1c escape_markup exact behavior", {
  e <- env$escape_markup
  expect_equal(e("<img src=x>"), "&lt;img src=x&gt;")
  expect_equal(e("a & b"), "a &amp; b")
  expect_equal(e("*x* _y_ `z` [a](b) #h ~s~ |t| !i \\"), "\\*x\\* \\_y\\_ \\`z\\` \\[a\\]\\(b\\) \\#h \\~s\\~ \\|t\\| \\!i \\\\")
  expect_equal(e("Elkins, West Virginia"), "Elkins, West Virginia")      # ordinary names are unchanged
  expect_equal(e("São Paulo"), "São Paulo"); expect_equal(e(""), "")
})
test_that("R5d a label whose offset position falls outside the ring is dropped, an interior one is kept", {
  edge <- mk(cbind(cx + c(-1500, 1500), cy + c(2700, 2700)), "primary", "Edge Road")     # long enough for its name, but its label would sit ~2795 m out, past the safe margin
  mid  <- mk(cbind(cx + c(-1000, 1000), cy + c(0, 0)), "primary", "Middle Road")
  lines <- env$prep_lines(fixture_roads(bind_rows(edge, mid)), lon0, lat0, 3000, crs0)
  lab <- env$make_labels(lines, 3000, crs0, ctr)
  expect_true("Middle Road" %in% lab$name)
  expect_false("Edge Road" %in% lab$name)
})
test_that("S3 candidate list and notifications are HTML-escaped (hostile place names)", {
  hostile <- "<img src=x onerror=alert(1)>"
  h <- as.character(selectInput("choice", "Which one?", choices = setNames(1:2, c(hostile, "ok"))))
  expect_false(grepl("<img src=x", h, fixed = TRUE))
  expect_match(shiny:::processDeps(paste("Place lookup failed:", hostile), NULL)$html, "&lt;img", fixed = TRUE)
})
test_that("S4 the geocoder request is built with an identifying user agent and URL-encoded query", {
  skip_if_not_installed("httr2")
  req <- httr2::request("https://nominatim.openstreetmap.org/search") |>
    httr2::req_url_query(q = "a&b=c d#e", format = "jsonv2", limit = 5) |> httr2::req_user_agent(env$user_agent)
  expect_match(req$url, "q=a%26b%3Dc%20d%23e", fixed = TRUE)
  expect_true(nzchar(env$user_agent))
})

# ---- offline integration tests of the live-service code paths -----------------
test_that("I4 geocode parses a Nominatim-style response (string coordinates -> numbers) and sends the right request", {
  seen <- NULL
  body <- list(list(display_name = "Elkins, Randolph County, West Virginia, United States", lat = "38.9262", lon = "-79.8467"),
               list(display_name = "Elkins, Arkansas, United States", lat = "36.0", lon = "-94.0"))
  res <- httr2::with_mocked_responses(function(req) { seen <<- req; httr2::response_json(body = body) }, env$geocode("Elkins, WV"))
  expect_equal(nrow(res), 2); expect_type(res$lat, "double"); expect_equal(res$lon[1], -79.8467); expect_equal(res$lat[2], 36)
  expect_match(seen$url, "q=Elkins%2C%20WV", fixed = TRUE); expect_match(seen$url, "limit=5", fixed = TRUE)
  expect_equal(seen$options$useragent, env$user_agent)
})
test_that("I4b geocode returns NULL for an empty result and propagates HTTP errors", {
  expect_null(httr2::with_mocked_responses(function(req) httr2::response_json(body = list()), env$geocode("zzzz")))
  expect_error(httr2::with_mocked_responses(function(req) httr2::response(status_code = 429), env$geocode("x")))
})
test_that("F3 a failing place lookup is handled (no crash, no map)", {
  with_stubs(NULL); env$geocode <- function(q) stop("HTTP 503 Service Unavailable")
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    expect_null(places())
  })
})

test_that("I5 places at the antimeridian are refused before any Overpass request", {
  withr::defer(rm(list = intersect(c("osmdata_sf", "set_overpass_url"), ls(env)), envir = env))
  env$osmdata_sf <- function(...) stop("must not be called"); env$set_overpass_url <- function(u) stop("must not be called")
  expect_error(env$fetch_roads(179.99, -17.8, 3000L, 32660), "180")
  expect_error(env$fetch_roads(-179.99, -17.8, 5000L, 32601), "180")
})
test_that("I5b legitimate high-latitude and high-longitude places are not refused by the antimeridian guard", {
  with_get(function(...) list(osm_lines = "ok"), {
    expect_equal(env$fetch_roads(100.1234, 83.5, 5000L, 32647)$osm_lines, "ok")       # 83.5 N, widest degree span
    expect_equal(env$fetch_roads(179.5, -17.8, 3000L, 32660)$osm_lines, "ok")
  })
})
test_that("S5 user-facing errors are sanitized and friendly messages are safe errors", {
  expect_true(isTRUE(getOption("shiny.sanitize.errors")))
  e <- with_get(function(...) stop("boom"), tryCatch(env$fetch_roads(77.7777, 8.8888, 3000L, 32644), error = function(e) e))
  expect_s3_class(e, "shiny.custom.error"); expect_false(grepl("boom", conditionMessage(e)))      # the internal message never reaches the user
})

# =====================================================================================
# Rework: high-resolution preview, map styles, multi-format export, redesigned UI
# =====================================================================================
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

# ---- ST1 styles ---------------------------------------------------------------
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
test_that("ST2 each style draws on its own paper color and the legend keeps all roles", {
  skip_if_not_installed("png")
  lines <- demo_lines_prepped()
  for (k in names(env$styles)) {
    p <- env$make_map(lines, "Style test", lon0, lat0, 3000, crs0, style = k)
    expect_equal(bg_of(p), env$styles[[k]]$bg)
    f <- tempfile(fileext = ".png"); env$draw_figure(p, f, "png", k, preview = TRUE)
    img <- png::readPNG(f); expect_equal(dim(img)[1:2], c(2500, 2000))
    corner <- grDevices::rgb(img[3, 3, 1], img[3, 3, 2], img[3, 3, 3])
    expect_equal(toupper(corner), toupper(env$styles[[k]]$bg), info = k)
  }
})

# ---- E1 exports ---------------------------------------------------------------
test_that("E1 every format writes a valid file with the right size", {
  skip_if_not_installed("png")
  lines <- demo_lines_prepped(); p <- env$make_map(lines, "Export test", lon0, lat0, 3000, crs0)
  out <- sapply(unname(env$export_formats), function(fmt) { f <- tempfile(fileext = paste0(".", env$format_ext[[fmt]])); env$draw_figure(p, f, fmt, "classic"); f })
  expect_equal(dim(png::readPNG(out[["png"]]))[1:2], c(3000, 2400))
  expect_equal(unname(jpeg_dim(out[["jpeg"]])), c(2400, 3000)); expect_equal(file_bytes(out[["jpeg"]])[1:2], as.raw(c(0xFF, 0xD8)))
  expect_equal(unname(tiff_dim(out[["tiff"]])), c(2400, 3000))
  expect_equal(rawToChar(file_bytes(out[["pdf"]])[1:5]), "%PDF-")
  if (nzchar(Sys.which("pdfinfo"))) {                                                                            # page count and size from poppler, when installed
    info <- system2("pdfinfo", out[["pdf"]], stdout = TRUE)
    expect_true(any(grepl("^Pages:\\s+1$", info))); expect_true(any(grepl("^Page size:\\s+576 x 720 pts", info)))   # 8 x 10 in at 72 pt/in
  }
  svg <- paste(readLines(out[["svg"]], warn = FALSE), collapse = "\n")
  expect_match(svg, "<svg[^>]+width=\"576pt\"[^>]+height=\"720pt\""); expect_match(svg, "viewBox=\"0 0 576 720\"")
  expect_true(all(file.size(out) > 20000))
  if (requireNamespace("xml2", quietly = TRUE)) expect_s3_class(xml2::read_xml(out[["svg"]]), "xml_document")  # well-formed
})
test_that("E1b vector text is sized like the raster (PDF vs PNG at 100 dpi have the same layout)", {
  skip_if(Sys.which("pdftoppm") == "", "pdftoppm not available"); skip_if_not_installed("png")
  p <- env$make_map(demo_lines_prepped(), "Elkins, West Virginia", lon0, lat0, 3000, crs0)
  pdf <- tempfile(fileext = ".pdf"); env$draw_figure(p, pdf, "pdf", "classic")
  prefix <- tempfile(); system2("pdftoppm", c("-r", "100", "-png", "-singlefile", pdf, prefix))
  a <- png::readPNG(paste0(prefix, ".png")); b <- png::readPNG(render_png(p, dpi = 100))
  expect_equal(dim(a)[1:2], dim(b)[1:2])
  bgc <- col2rgb(env$bg)[, 1] / 255
  ink_mask <- function(img) (abs(img[, , 1] - bgc[1]) + abs(img[, , 2] - bgc[2]) + abs(img[, , 3] - bgc[3])) > 0.3
  ma <- ink_mask(a); mb <- ink_mask(b)
  extent <- function(m) { cols <- which(colSums(m[1:150, ]) > 0); c(min(cols), max(cols)) }      # the title and subtitle block
  expect_lt(max(abs(extent(ma) - extent(mb))), 15)                                                  # a dpi mismatch would shift this by 100+ px
  expect_lt(abs(mean(ma) - mean(mb)) / mean(mb), 0.08)
})
test_that("E2 draw_figure rejects unsupported formats and never leaves a device open", {
  p <- env$make_map(demo_lines_prepped(), "x", lon0, lat0, 3000, crs0)
  before <- grDevices::dev.list()
  for (bad in list("exe", "PNG", "../x", NA, NULL, c("png", "pdf"), character(0), 1))
    expect_error(env$draw_figure(p, tempfile(), bad), "Unsupported", info = deparse(bad))
  expect_equal(grDevices::dev.list(), before)
  broken <- ggplot2::ggplot(data.frame(a = 1)) + ggplot2::geom_point(ggplot2::aes(x = no_such_column))
  for (fmt in unname(env$export_formats)) expect_error(env$draw_figure(broken, tempfile(), fmt))
  expect_equal(grDevices::dev.list(), before)                                                      # failed drawing closed its device
})
test_that("E2b draw_figure restores the text-size setting and unknown styles draw as Classic", {
  p <- env$make_map(demo_lines_prepped(), "x", lon0, lat0, 3000, crs0)
  for (fmt in c("png", "pdf")) { env$draw_figure(p, tempfile(), fmt, "dusk"); expect_equal(showtext::showtext_opts()$dpi, env$preview_dpi) }
  expect_error(env$draw_figure(p, tempfile(fileext = ".png"), "png", "nonsense"), NA)
})
test_that("E3 file_slug makes safe file names", {
  s <- env$file_slug
  expect_equal(s("Elkins, West Virginia"), "elkins_west_virginia")
  expect_equal(s("../../etc/passwd"), "etc_passwd"); expect_equal(s("a/b\\c:d*e?f\"g<h>i|j"), "a_b_c_d_e_f_g_h_i_j")
  expect_equal(s(""), "map"); expect_equal(s(NULL), "map"); expect_equal(s(NA), "map"); expect_equal(s(NA_character_), "map"); expect_equal(s("!!!"), "map")
  expect_lte(nchar(s(strrep("abc ", 100))), 40)
  expect_true(all(grepl("^[a-z0-9_]+$", sapply(c("São Paulo, Brasil", "大阪", "x y", "Elkins  --  WV"), s))))
})

# ---- statistics shared by the tiles and the subtitle ----------------------------
test_that("ST3 road_stats agrees with independent arithmetic", {
  lines <- demo_lines_prepped(); s <- env$road_stats(lines)
  len <- function(h) sum(as.numeric(st_length(lines[lines$highway %in% h, ]))) / 1000
  expect_equal(s$total_km, sum(as.numeric(st_length(lines))) / 1000, tolerance = 1e-9)
  expect_equal(s$local_km, len(c("residential", "unclassified", "service")), tolerance = 1e-9)
  expect_equal(s$through_km, len(c("trunk", "primary", "secondary")), tolerance = 1e-9)
  expect_equal(s$local_pct, round(100 * s$local_km / s$total_km))
  expect_equal(nrow(s$by_role), 5)
  only_one <- env$prep_lines(fixture_roads(mk(cbind(cx + c(-500, 500), cy + c(0, 0)), "primary", "Solo")), lon0, lat0, 3000, crs0)
  s1 <- env$road_stats(only_one); expect_equal(s1$local_km, 0); expect_equal(s1$local_pct, 0); expect_gt(s1$through_km, 0)
})
test_that("ST3b fig_alt describes the map in plain text", {
  a <- env$fig_alt("Elkins, West Virginia", list(noun = "colored by road class", subtitle = "258 km of street, 56% local."), 3000)
  expect_match(a, "Elkins, West Virginia", fixed = TRUE); expect_match(a, "within 3 km", fixed = TRUE)
  expect_match(a, "colored by road class", fixed = TRUE); expect_match(a, "258 km"); expect_match(a, "56%")
  expect_match(env$fig_alt("X", list(noun = "n", subtitle = "s"), 2500), "within 2.5 km", fixed = TRUE)   # fractional radii read naturally
})

# ---- server: preview, live restyle, downloads -----------------------------------
test_that("N1 the preview is a 2000 x 2500 px PNG with a text description", {
  skip_if_not_installed("png"); with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, style = "classic", draw = 1)
    img <- output$map                                                   # testServer hands back the image as a data: URI
    expect_match(img$src, "^data:image/png;base64,")
    expect_equal(dim(png::readPNG(base64enc::base64decode(sub("^data:image/png;base64,", "", img$src))))[1:2], c(2500, 2000))
    expect_match(img$alt, "Elkins, West Virginia"); expect_match(img$alt, "within 3 km")
  })
})
test_that("N2 changing style or labels redraws from the same streets, with no new request", {
  calls <- with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, style = "dusk", draw = 1)
    expect_equal(bg_of(built()), env$styles$dusk$bg)
    session$setInputs(style = "mono");   expect_equal(bg_of(built()), env$styles$mono$bg)
    session$setInputs(style = "classic"); expect_equal(bg_of(built()), env$styles$classic$bg)
    session$setInputs(style = "<img src=x onerror=1>"); expect_equal(bg_of(built()), env$styles$classic$bg)   # hostile value: Classic
    session$setInputs(style = NULL);     expect_equal(bg_of(built()), env$styles$classic$bg)
    session$setInputs(labels = FALSE);   expect_s3_class(built(), "ggplot")
  })
  expect_length(calls$fetch, 1)
})
test_that("N3 the summary tiles show the independent totals and appear only after a map exists", {
  with_stubs(NULL)
  testServer(env$server, {
    expect_equal(output$has_map, "no")
    expect_error(output$stats)                                         # nothing to summarize yet
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, draw = 1)
    expect_equal(output$has_map, "yes")
    lines <- env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0)
    html <- paste(as.character(output$stats$html), collapse = "")
    km <- function(x) paste0(format(round(x), big.mark = ","), " km")
    expect_match(html, km(sum(lines$km)), fixed = TRUE)
    expect_match(html, paste0(round(100 * sum(lines$km[lines$role == "Local streets"]) / sum(lines$km)), "%"), fixed = TRUE)
  })
})
test_that("N3b a failed draw leaves has_map FALSE", {
  with_stubs(NULL); env$fetch_roads <- function(...) list(osm_lines = NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, draw = 1)
    expect_equal(output$has_map, "no")
  })
})
test_that("N4 the download handler writes the format that was chosen, and anything else becomes PNG", {
  skip_if_not_installed("png"); with_stubs(NULL)
  magic <- function(f) file_bytes(f)[1:4]
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, style = "dusk", draw = 1)
    for (fmt in unname(env$export_formats)) {
      session$setInputs(format = fmt); f <- output$download
      switch(fmt,
        png  = expect_equal(magic(f), as.raw(c(0x89, 0x50, 0x4E, 0x47))),
        jpeg = expect_equal(magic(f)[1:3], as.raw(c(0xFF, 0xD8, 0xFF))),
        tiff = expect_equal(magic(f)[1:2], as.raw(c(0x49, 0x49))),
        pdf  = expect_equal(rawToChar(magic(f)), "%PDF"),
        svg  = expect_match(readLines(f, n = 3, warn = FALSE)[1], "<?xml", fixed = TRUE))
    }
    for (bad in list("exe", "../../x", NA, NULL, c("png", "pdf"))) {
      session$setInputs(format = bad); f <- output$download
      expect_equal(magic(f), as.raw(c(0x89, 0x50, 0x4E, 0x47)), info = deparse(bad))
    }
    session$setInputs(format = "png")
    img <- png::readPNG(output$download)                                  # the download uses the chosen style too
    expect_equal(toupper(grDevices::rgb(img[3, 3, 1], img[3, 3, 2], img[3, 3, 3])), toupper(env$styles$dusk$bg))
  })
})

# ---- UI contract ----------------------------------------------------------------
test_that("U1 the page offers the controls the app depends on, and the old fixed-size plot is gone", {
  html <- paste(as.character(env$ui(NULL)), collapse = "\n")
  for (fmt in unname(env$export_formats)) expect_match(html, sprintf("<option value=\"%s\"", fmt), fixed = TRUE)
  for (k in names(env$styles)) expect_match(html, sprintf("value=\"%s\"", k), fixed = TRUE)
  for (id in c("query", "find", "choice_placeholder_not_expected"[0], "radius", "labels", "view", "style", "draw", "format", "download", "map", "stats"))
    expect_match(html, sprintf("id=\"%s\"", id), fixed = TRUE)
  expect_false(grepl("640px", html, fixed = TRUE)); expect_false(grepl("height:800px", html, fixed = TRUE))
  expect_match(html, "output.has_map", fixed = TRUE)                       # download controls wait for a map
})
test_that("U1b every form control has an accessible name", {
  html <- paste(as.character(env$ui(NULL)), collapse = "\n")
  for (id in c("query", "radius", "format")) expect_match(html, sprintf("for=\"%s\"", id), fixed = TRUE, info = id)
  expect_match(html, "visually-hidden", fixed = TRUE)
  expect_match(html, "aria-hidden", fixed = TRUE)                           # decorative logo is hidden from screen readers
})
test_that("U2 the theme builds, with or without web fonts", {
  expect_s3_class(env$make_theme(FALSE), "bs_theme")
  expect_s3_class(env$get_theme(), "bs_theme")
  expect_no_error(bslib::bs_theme_dependencies(env$get_theme()))
})
test_that("U3 colors in the page come from the same palette as the Classic map", {
  html <- paste(as.character(env$ui(NULL)), collapse = "\n")
  for (col in env$styles$classic$cols[c(1, 2, 3, 5)]) expect_match(html, col, fixed = TRUE)   # logo strokes
})

# ---- P2 performance of every output on a dense city ---------------------------------
test_that("P2 preview and every download render in reasonable time on a dense 5 km city", {
  skip_if(Sys.getenv("OVERMAPS_SKIP_PERF") == "1", "performance checks skipped")
  set.seed(7); n <- 4000; L <- vector("list", n)
  for (i in seq_len(n)) {
    s <- cbind(cx + runif(1, -5000, 5000) + cumsum(rnorm(20, 0, 40)), cy + runif(1, -5000, 5000) + cumsum(rnorm(20, 0, 40)))
    L[[i]] <- mk(s, sample(c("residential", "residential", "residential", "tertiary", "secondary", "primary"), 1), if (i %% 40 == 0) paste("Road", i) else NA_character_)
  }
  lines <- env$prep_lines(fixture_roads(bind_rows(L)), lon0, lat0, 5000, crs0)
  t_map <- system.time(p <- env$make_map(lines, "Dense", lon0, lat0, 5000, crs0))[["elapsed"]]
  t <- c(map = t_map, sapply(c("preview", unname(env$export_formats)), function(fmt) {
    f <- tempfile(); system.time(if (fmt == "preview") env$draw_figure(p, f, "png", "classic", preview = TRUE) else env$draw_figure(p, f, fmt, "classic"))[["elapsed"]] }))
  message("PERF2 ", paste(sprintf("%s %.1fs", names(t), t), collapse = " | "))
  expect_lt(max(t), 60)
})


# ---- V: map types (speed limits, direction, street density) -----------------------------------------------
sf1 <- function(pts, ...) st_sf(osm_id = "1", name = NA_character_, highway = "residential", ..., geometry = st_sfc(st_linestring(pts), crs = crs0))
mk_lines <- function(specs) {       # specs: list(list(pts, maxspeed)) -> prepped lines with `km`, `role`
  l <- do.call(rbind, lapply(specs, function(x) sf1(x[[1]], maxspeed = x[[2]])))
  env$prep_lines(list(osm_lines = st_transform(l, 4326)), lon0, lat0, 3000, crs0)
}
ctr_m <- function() env$center_of(lon0, lat0, crs0)
ang_pts <- function(deg, len = 1000, off = c(0, 0)) { r <- deg * pi / 180; cbind(cx + off[1] + c(0, len * sin(r)), cy + off[2] + c(0, len * cos(r))) }   # deg clockwise from north

test_that("V1 get_view falls back to the hierarchy for anything unexpected", {
  for (bad in list(NULL, NA, "", "bogus", "<img src=x>", 3, character(0), NA_character_)) expect_equal(env$get_view(bad), "hierarchy")
  for (v in names(env$views)) expect_equal(env$get_view(v), v)
  expect_equal(env$get_view(c("speed", "density")), "speed")                  # only the first value counts
})

test_that("V2 parse_speed accepts plain numbers and rejects everything else", {
  p <- env$parse_speed(c("25 mph", "30", "35mph", " 40 ", "50 km/h", "30;50", "none", "walk", "RU:urban", "signals", "0", "-5", "500", "", NA))
  expect_equal(p$value, c(25, 30, 35, 40, 50, 30, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  expect_equal(p$mph, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, rep(FALSE, 9)))
})

test_that("V3 speed classes use the right bin edges and the majority unit", {
  us <- env$speed_of(c("20 mph", "25 mph", "30 mph", "35 mph", "40 mph", "45 mph", "50 mph", "55 mph", "60 mph", NA, "none"))
  expect_equal(us$unit, "mph")
  expect_equal(as.character(us$class), c("up to 25", "up to 25", "30–35", "30–35", "40–45", "40–45", "50–55", "50–55", "over 55", "No limit mapped", "No limit mapped"))
  eu <- env$speed_of(c("30", "50", "70", "100", "120", "20"))
  expect_equal(eu$unit, "km/h")
  expect_equal(as.character(eu$class), c("up to 30", "31–50", "51–70", "71–100", "over 100", "up to 30"))
  mixed <- env$speed_of(c("50", "60", "30 mph"))                               # km/h wins; 30 mph = 48.3 km/h
  expect_equal(mixed$unit, "km/h"); expect_equal(as.character(mixed$class)[3], "31–50"); expect_equal(mixed$value[3], 30 * 1.609344)
  mixed2 <- env$speed_of(c("25 mph", "35 mph", "50"))                          # mph wins; 50 km/h = 31 mph
  expect_equal(mixed2$unit, "mph"); expect_equal(as.character(mixed2$class)[3], "30–35")
  none <- env$speed_of(c(NA, "none", "walk"))
  expect_true(all(as.character(none$class) == "No limit mapped")); expect_length(env$speed_of(character(0))$class, 0)
})

test_that("V4 prep_lines keeps the speed tag, and tolerates its absence", {
  l <- st_transform(rbind(sf1(ang_pts(0), maxspeed = "25 mph"), sf1(ang_pts(90), maxspeed = NA_character_)), 4326)
  p <- env$prep_lines(list(osm_lines = l), lon0, lat0, 3000, crs0)
  expect_true("maxspeed" %in% names(p)); expect_equal(sort(p$maxspeed, na.last = TRUE), c("25 mph", NA))
  l2 <- l[, c("osm_id", "name", "highway", "geometry")]                         # no maxspeed column at all (nothing tagged in the area)
  p2 <- env$prep_lines(list(osm_lines = l2), lon0, lat0, 3000, crs0)
  expect_true(all(is.na(p2$maxspeed)))
  vd <- env$view_data("speed", p2, 3000, ctr_m()); expect_match(vd$subtitle, "no posted speed limits"); expect_equal(vd$tiles[[2]]$value, "0%")
})

test_that("V5 speed view reports coverage and the most common limit from independent arithmetic", {
  # 1 km tagged 25 mph, 2 km tagged 35 mph, 1 km untagged, 0.5 km "none"
  lines <- mk_lines(list(list(ang_pts(0, 1000, c(-300, 0)), "25 mph"), list(ang_pts(0, 2000, c(0, -900)), "35 mph"),
                         list(ang_pts(0, 1000, c(300, 0)), NA), list(ang_pts(0, 500, c(600, 0)), "none")))
  vd <- env$view_data("speed", lines, 3000, ctr_m())
  tot <- sum(lines$km); tagged <- sum(lines$km[lines$maxspeed %in% c("25 mph", "35 mph")])
  expect_equal(vd$tiles[[2]]$value, paste0(round(100 * tagged / tot), "%"))
  expect_equal(vd$tiles[[3]]$value, "35 mph")
  expect_match(vd$subtitle, paste0(round(100 * tagged / tot), "% of the"), fixed = TRUE)
  expect_equal(unname(vd$km_by["No limit mapped"]), sum(lines$km[!(lines$maxspeed %in% c("25 mph", "35 mph"))]), tolerance = 1e-9)
  expect_equal(sum(vd$km_by), tot, tolerance = 1e-9)                              # every kilometer lands in exactly one class
})

test_that("V6 segments_of reports lengths and bearings for known geometry", {
  s <- env$segments_of(mk_lines(list(list(ang_pts(0), NA), list(ang_pts(90, 600, c(0, 200)), NA), list(ang_pts(45, 500, c(100, 100)), NA), list(ang_pts(135, 400, c(-100, -100)), NA))))
  o <- s[order(s$len, decreasing = TRUE), ]
  expect_equal(o$len, c(1000, 600, 500, 400), tolerance = 1e-6)
  expect_equal(o$deg[1] %% 180, 0, tolerance = 1e-6); expect_equal(o$deg[2], 90, tolerance = 1e-6); expect_equal(o$deg[3], 45, tolerance = 1e-6); expect_equal(o$deg[4], 135, tolerance = 1e-6)
  expect_equal(nrow(env$segments_of(st_sf(geometry = st_sfc(crs = crs0)))), 0)
  zero <- sf1(cbind(c(cx, cx), c(cy, cy)))                                       # a zero-length line contributes nothing
  expect_equal(nrow(env$segments_of(zero)), 0)
  bent <- sf1(cbind(cx + c(0, 100, 100), cy + c(0, 0, 100)))                      # two pieces: east, then north
  b <- env$segments_of(bent); expect_equal(b$piece, c(1, 1)); expect_equal(b$deg, c(90, 0), tolerance = 1e-6)
  expect_equal(b$mx, cx + c(50, 100)); expect_equal(b$my, cy + c(0, 50))
})

test_that("V7 dir_bin and the length-weighted piece heading", {
  d <- c(0, 11.24, 11.26, 33.7, 45, 90, 135, 157.4, 157.6, 168.74, 168.76, 179.99, 180, 190)
  expect_equal(env$dir_bin(d), c(1, 1, 2, 2, 3, 5, 7, 8, 8, 8, 1, 1, 1, 1))   # 190 degrees is 10 degrees: N-S
  expect_length(env$dir_labels, 8); expect_equal(env$dir_labels[c(1, 5)], c("N–S", "E–W"))
  segs <- tibble(piece = c(1L, 1L, 2L), len = c(10, 10, 5), deg = c(179, 1, 90))   # 179 and 1 degrees average to north-south, not to 90
  h <- env$piece_heading(segs, 3)
  expect_lt(min(h[1] %% 180, 180 - h[1] %% 180), 0.01); expect_equal(h[2], 90); expect_true(is.na(h[3]))
  longer <- tibble(piece = c(1L, 1L), len = c(30, 10), deg = c(0, 45))            # the longer piece dominates
  expect_lt(abs(env$piece_heading(longer, 1) - atan(10 / 30) * 90 / pi), 1e-6)     # doubled angles: vector (30, 10) -> 18.43 degrees -> heading 9.22
})

test_that("V8 direction view: shares, grid alignment, and the rose add up", {
  lines <- mk_lines(list(list(ang_pts(0, 1000, c(-100, -500)), NA), list(ang_pts(90, 1000, c(-500, 100)), NA), list(ang_pts(45, 1000, c(0, -300)), NA)))
  vd <- env$view_data("direction", lines, 3000, ctr_m())
  expect_equal(vd$tiles[[3]]$value, "67%")                                         # N-S and E-W are 2 of the 3 equal km; 45 degrees is not grid-aligned
  expect_match(vd$tiles[[2]]$note, "33% of street length", fixed = TRUE)
  expect_equal(sum(vd$rose$h), 1, tolerance = 1e-9)                                # the rose is a distribution
  expect_equal(nrow(vd$rose), 72); expect_equal(vd$rose$h[vd$rose$a == 0], vd$rose$h[vd$rose$a == 180])   # opposite directions mirror
  expect_equal(max(vd$rose$h[vd$rose$a %in% c(0, 180)]) * 2, 1 / 3, tolerance = 1e-9)
  one <- mk_lines(list(list(ang_pts(0), NA))); v1 <- env$view_data("direction", one, 3000, ctr_m())
  expect_equal(v1$tiles[[2]]$value, "N–S"); expect_equal(v1$tiles[[3]]$value, "100%")
})

test_that("V9 hexagon density conserves street length and respects the ring", {
  lines <- mk_lines(list(list(ang_pts(0, 2400, c(0, -1200)), NA), list(ang_pts(90, 1800, c(-900, 300)), NA), list(ang_pts(30, 1000, c(200, 200)), NA)))
  segs <- env$segments_of(lines); hx <- env$hex_density(segs, 3000, ctr_m())
  cells <- hx$cells; area_km2 <- as.numeric(st_area(cells)) / 1e6
  expect_equal(sum(cells$dens * area_km2), sum(cells$len) / 1000, tolerance = 1e-9)   # density x area gives back the street km held by the cells
  expect_gte(min(cells$frac), 0.3 - 1e-9); expect_lte(max(cells$frac), 1 + 1e-9)
  expect_lte(max(as.numeric(st_distance(st_cast(st_boundary(cells), "MULTIPOINT"), ctr_m()))), 3000 + 1)
  full <- cells[cells$frac > 0.999, ]; expect_equal(as.numeric(st_area(full)), rep(3 * sqrt(3) / 2 * (3000 / 11)^2, nrow(full)), tolerance = 1e-3)
  expect_equal(hx$width_m, sqrt(3) * 3000 / 11)
  expect_gte(sum(cells$len), 0.9 * sum(segs$len))                                    # only edge slivers are dropped
  # a street running through the exact middle of one hexagon: that cell holds all of it
  s1 <- env$segments_of(mk_lines(list(list(cbind(cx + c(-50, 50), cy + c(0, 0)), NA)))); h1 <- env$hex_density(s1, 3000, ctr_m())
  expect_equal(nrow(h1$cells), 1); expect_equal(h1$cells$len, 100, tolerance = 1e-6)
})

test_that("V10 density view summarizes honestly, including degenerate cases", {
  lines <- demo_lines_prepped(); vd <- env$view_data("density", lines, 3000, ctr_m())
  expect_true(all(c("cls") %in% names(vd$cells))); expect_false(anyNA(vd$cells$cls))
  expect_length(vd$levels, 6); expect_true(all(as.character(vd$cells$cls) %in% vd$levels))
  expect_equal(as.character(vd$cells$cls), as.character(cut(vd$cells$dens, c(0, 2, 5, 10, 15, 20, Inf), c("under 2", "2\u20135", "5\u201310", "10\u201315", "15\u201320", "20 or more"), right = FALSE)))   # fixed classes, checked independently
  expect_match(vd$tiles[[2]]$value, "km/km²"); expect_match(vd$subtitle, "hexagons", fixed = TRUE)
  uniform <- mk_lines(list(list(cbind(cx + c(-50, 50), cy), NA)))                  # a single hexagon: one class, no error
  v1 <- env$view_data("density", uniform, 3000, ctr_m()); expect_length(v1$levels, 6); expect_equal(nrow(v1$cells), 1); expect_false(anyNA(v1$cells$cls))
  expect_s3_class(env$make_map(uniform, "One cell", lon0, lat0, 3000, crs0, view = "density"), "patchwork")
})

test_that("V11 every map type builds in every style and renders; unknown types draw the hierarchy", {
  skip_if_not_installed("png")
  lines <- demo_lines_prepped(); lines$maxspeed <- ifelse(lines$highway == "primary", "35 mph", NA)
  for (v in names(env$views)) for (st in names(env$styles)) {
    p <- env$make_map(lines, "Elkins, West Virginia", lon0, lat0, 3000, crs0, style = st, view = v)
    expect_s3_class(p, "ggplot"); expect_equal(bg_of(p), env$styles[[st]]$bg)
  }
  f <- tempfile(fileext = ".png"); env$draw_figure(env$make_map(lines, "T", lon0, lat0, 3000, crs0, view = "bogus"), f, "png", "classic")
  expect_gt(file.size(f), 5000)
  for (v in names(env$views)) {                                                    # vector exports of the two composite layouts
    g <- tempfile(fileext = ".pdf"); env$draw_figure(env$make_map(lines, "T", lon0, lat0, 3000, crs0, view = v), g, "pdf", "classic")
    expect_gt(file.size(g), 3000)
  }
})

test_that("V12 hierarchy view data matches road_stats; make_map without a view is unchanged", {
  lines <- demo_lines_prepped(); s <- env$road_stats(lines); vd <- env$view_data("hierarchy", lines, 3000, ctr_m())
  expect_equal(vd$tiles[[1]]$value, paste0(format(round(s$total_km), big.mark = ","), " km"))
  expect_equal(vd$tiles[[3]]$value, paste0(s$local_pct, "%")); expect_match(vd$subtitle, paste0(s$local_pct, "% of it is local"), fixed = TRUE)
  expect_equal(bg_of(env$make_map(lines, "T", lon0, lat0, 3000, crs0)), env$styles$classic$bg)
})

test_that("V13 the server switches map type from the same streets, validates the input, and names downloads by type", {
  skip_if_not_installed("png")
  calls <- with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", draw = 1)
    html <- function() paste(as.character(output$stats$html), collapse = "")
    expect_match(html(), "Through routes", fixed = TRUE)
    session$setInputs(view = "speed");     expect_match(html(), "Limits mapped", fixed = TRUE); expect_match(output$map$alt, "posted speed limit", fixed = TRUE)
    session$setInputs(view = "direction"); expect_match(html(), "Main direction", fixed = TRUE); expect_match(output$map$alt, "direction each street runs", fixed = TRUE)
    session$setInputs(view = "density");   expect_match(html(), "Peak density", fixed = TRUE); expect_match(output$map$alt, "street density", fixed = TRUE)
    session$setInputs(view = "direction"); expect_length(built()$patches$plots, 2)     # the figure itself follows the map type: the map plus a row with the compass rose and share bars
    session$setInputs(view = "hierarchy"); expect_length(built()$patches$plots, 0)
    session$setInputs(view = "<script>alert(1)</script>"); expect_match(html(), "Through routes", fixed = TRUE)
    session$setInputs(view = NULL); expect_match(html(), "Through routes", fixed = TRUE)
    session$setInputs(view = "direction", format = "png")
    expect_gt(file.size(output$download), 5000)                                       # the download follows the chosen map type (its file name is checked in the browser run)
  })
  expect_length(calls$fetch, 1)                                                      # map types never trigger another download
})


test_that("V14 grid alignment counts only streets within 10 degrees of an axis", {
  grid <- function(deg) env$view_data("direction", mk_lines(list(list(ang_pts(0, 1000, c(-300, -500)), NA), list(ang_pts(deg, 1000, c(300, -500)), NA))), 3000, ctr_m())$tiles[[3]]$value
  expect_equal(grid(8), "100%"); expect_equal(grid(15), "50%"); expect_equal(grid(100), "100%"); expect_equal(grid(110), "50%"); expect_equal(grid(172), "100%")
})

test_that("V15 the compass rose mirrors every direction and weights by length", {
  lines <- mk_lines(list(list(ang_pts(0, 1000, c(-100, -500)), NA), list(ang_pts(90, 2000, c(-1000, 100)), NA)))   # 1 km north-south, 2 km east-west
  r <- env$view_data("direction", lines, 3000, ctr_m())$rose
  expect_equal(sort(r$a), seq(0, 355, by = 5))
  expect_equal(r$h[r$a == 0], 1 / 6); expect_equal(r$h[r$a == 180], 1 / 6); expect_equal(r$h[r$a == 90], 1 / 3); expect_equal(r$h[r$a == 270], 1 / 3)
  expect_equal(sum(r$h[r$a > 80 & r$a < 100]), 1 / 3)                              # nothing leaks into neighboring bins
})

test_that("V16 hexagons mostly outside the ring are dropped even when a street ends in them", {
  th <- seq(0, 2 * pi, length.out = 181)[-181]
  segs <- tibble(piece = seq_along(th), len = 10, mx = cx + 2955 * cos(th), my = cy + 2955 * sin(th), deg = 0)   # a street segment every 2 degrees along the ring
  hx <- env$hex_density(segs, 3000, ctr_m())$cells
  expect_gte(min(hx$frac), 0.3 - 1e-9)
  expect_lt(sum(hx$len), sum(segs$len))                                              # some segments fell in slivers and were left out, as documented
  expect_gt(nrow(hx), 20)
})


test_that("V17 the direction bars show each bin's share of street length", {
  lines <- mk_lines(list(list(ang_pts(0, 1000, c(-100, -500)), NA), list(ang_pts(90, 3000, c(-1500, 100)), NA)))   # 1 km north-south, 3 km east-west
  b <- env$share_bars(env$view_data("direction", lines, 3000, ctr_m())$sh8)
  expect_equal(nrow(b), 8); expect_equal(sum(b$pct), 100, tolerance = 1e-9)
  expect_equal(b$pct[b$lab == "N–S"], 25); expect_equal(b$pct[b$lab == "E–W"], 75)
  expect_equal(levels(b$lab)[8], "N–S")                                        # N-S is drawn at the top of the bars
})


# ---- C: city limits and the local-streets switch ----------------------------------------------------------------------
gj_ring <- function(pts) lapply(seq_len(nrow(pts)), \(i) list(pts[i, 1], pts[i, 2]))     # the nested-list shape httr2 gives for a coordinate pair
gj_poly <- function(pts) list(type = "Polygon", coordinates = list(gj_ring(pts)))
square_pts <- cbind(lon0 + c(0, .02, .02, 0, 0), lat0 + c(0, 0, .02, .02, 0))
sq_shape <- function(half = 1000) st_sfc(st_polygon(list(cbind(cx + half * c(-1, 1, 1, -1, -1), cy + half * c(-1, -1, 1, 1, -1)))), crs = crs0)

test_that("C1 parse_boundary keeps polygons and rejects everything else without erroring", {
  p <- env$parse_boundary(gj_poly(square_pts))
  expect_s3_class(p, "sfc_POLYGON"); expect_equal(st_crs(p)$epsg, 4326); expect_equal(unname(st_bbox(p)[c("xmin", "ymax")]), c(lon0, lat0 + .02))
  mp <- env$parse_boundary(list(type = "MultiPolygon", coordinates = list(list(gj_ring(square_pts)), list(gj_ring(cbind(lon0 + 1 + c(0, .01, .01, 0, 0), lat0 + c(0, 0, .01, .01, 0)))))))
  expect_s3_class(mp, "sfc_MULTIPOLYGON")
  for (bad in list(NULL, list(type = "Point", coordinates = list(lon0, lat0)), list(type = "LineString", coordinates = gj_ring(square_pts)),
                   list(type = "Polygon", coordinates = "garbage"), list(type = "Polygon"), list(), "text", NA))
    expect_null(env$parse_boundary(bad), info = deparse(bad)[1])
})

test_that("C2 geocode asks for boundaries and returns one per match (NULL when a match has none)", {
  seen <- NULL
  body <- list(list(display_name = "Elkins, WV", lat = "38.9", lon = "-79.8", geojson = gj_poly(square_pts), boundingbox = list("38.8", "39.0", "-79.9", "-79.7")),
               list(display_name = "Elk Pt", lat = "39", lon = "-80", geojson = list(type = "Point", coordinates = list(-80, 39))),
               list(display_name = "No geojson", lat = "40", lon = "-81"))
  r <- httr2::with_mocked_responses(function(req) { seen <<- req$url; httr2::response_json(body = body) }, env$geocode("Elkins"))
  expect_match(seen, "polygon_geojson=1", fixed = TRUE)
  expect_equal(names(r), c("label", "lat", "lon", "boundary", "south", "north", "west", "east")); expect_equal(nrow(r), 3)
  expect_s3_class(r$boundary[[1]], "sfc_POLYGON"); expect_null(r$boundary[[2]]); expect_null(r$boundary[[3]])
  expect_equal(r$lat, c(38.9, 39, 40))                                                  # the circle option still centers on Nominatim's own point
})

test_that("C3 city_shape repairs, simplifies, and measures a boundary", {
  th <- seq(0, 2 * pi, length.out = 400)[-400]; ring <- cbind(cx + 2000 * cos(th), cy + 2000 * sin(th)); ring <- rbind(ring, ring[1, ])
  b <- st_transform(st_sfc(st_polygon(list(ring)), crs = crs0), 4326)
  sh <- env$city_shape(b, crs0)
  expect_equal(st_crs(sh)$epsg, crs0); expect_equal(as.numeric(st_area(sh)), pi * 2000^2, tolerance = 0.01)
  expect_equal(env$shape_radius(sh), 2000, tolerance = 0.01)
  expect_lt(nrow(st_coordinates(sh)), 399)                                               # fine detail was simplified away
  bow <- st_transform(st_sfc(st_polygon(list(cbind(cx + c(0, 1000, 1000, 0, 0), cy + c(0, 1000, 0, 1000, 0)))), crs = crs0), 4326)   # a self-crossing "bowtie"
  fixed <- env$city_shape(bow, crs0); expect_true(!is.null(fixed) && all(st_is_valid(fixed)) && as.numeric(st_area(fixed)) > 0)
  expect_null(env$city_shape(NULL, crs0))
  expect_null(env$city_shape(st_transform(st_sfc(st_point(c(cx, cy)), crs = crs0), 4326), crs0))
})

test_that("C4 prep_lines clips to the boundary with the expected length", {
  sh <- sq_shape(1000); r <- env$shape_radius(sh)
  l <- st_transform(rbind(mk(cbind(cx + c(-5000, 5000), cy + c(0, 0)), "primary", "Long Road"), mk(cbind(cx + c(1500, 1500), cy + c(-500, 500)), "residential")), 4326)
  p <- env$prep_lines(list(osm_lines = l), lon0, lat0, as.integer(round(r)), crs0, sh)
  expect_equal(nrow(p), 1)                                                               # the street wholly outside the boundary is gone
  expect_equal(p$km * 1000, 2000 - 2 * 0.012 * r, tolerance = 1e-4)                      # inside the boundary, minus the small inset
  expect_true(all(lengths(st_within(st_geometry(p), st_buffer(sh, 0.01))) == 1))
  lines <- demo_lines_prepped(); wide <- env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0, sq_shape(4000))
  expect_gt(sum(wide$km), sum(lines$km))                                                 # a bigger boundary holds more street
})

test_that("C5 text says where the map covers, for both kinds of area", {
  sh <- sq_shape(2000); r <- as.integer(round(env$shape_radius(sh))); ctr <- st_sfc(st_point(c(cx, cy)), crs = crs0)
  lines <- env$prep_lines(fixture_roads(), lon0, lat0, r, crs0, sh)
  for (v in names(env$views)) {
    city <- env$view_data(v, lines, r, ctr, sh); circ <- env$view_data(v, lines, 3000, ctr)
    expect_equal(city$scope, "inside the city limits of"); expect_equal(circ$scope, "within 3 km of the center of")
    expect_false(grepl("km of the center", city$subtitle, fixed = TRUE)); expect_false(grepl("km of the center", city$tiles[[1]]$note, fixed = TRUE))
  }
  expect_match(env$view_data("hierarchy", lines, r, ctr, sh)$subtitle, "of street inside the city limits")
  expect_match(env$view_data("density", lines, r, ctr, sh)$caption, "inside the boundary", fixed = TRUE)
  expect_match(env$view_data("density", lines, 3000, ctr)$caption, "inside the ring", fixed = TRUE)
  a <- env$fig_alt("Elkins", env$view_data("speed", lines, r, ctr, sh), r); expect_match(a, "streets inside the city limits of Elkins", fixed = TRUE)
})

test_that("C6 hiding local streets changes only the drawing, and says so", {
  lines <- demo_lines_prepped(); n_local <- sum(lines$role == env$roles$role[5]); km_local <- round(sum(lines$km[lines$role == env$roles$role[5]]))
  feat <- function(p) sum(vapply(p[[1]]$layers, \(l) if (inherits(l$data, "sf")) nrow(l$data) else 0L, 1L))
  shown <- env$make_map(lines, "T", lon0, lat0, 3000, crs0, label_roads = FALSE, show_local = TRUE)
  hidden <- env$make_map(lines, "T", lon0, lat0, 3000, crs0, label_roads = FALSE, show_local = FALSE)
  expect_equal(feat(shown) - feat(hidden), n_local); expect_gt(n_local, 0)
  expect_match(hidden$patches$annotation$caption, paste0("the ", km_local, " km of local streets are not drawn"), fixed = TRUE)
  expect_false(grepl("not drawn", shown$patches$annotation$caption, fixed = TRUE))
  expect_equal(hidden$patches$annotation$subtitle, shown$patches$annotation$subtitle)    # the numbers still count every street
  for (v in c("speed", "direction", "density")) {                                        # other map types always draw every street
    a <- env$make_map(lines, "T", lon0, lat0, 3000, crs0, view = v, show_local = TRUE); b <- env$make_map(lines, "T", lon0, lat0, 3000, crs0, view = v, show_local = FALSE)
    expect_equal(feat(a), feat(b)); expect_false(grepl("not drawn", b$patches$annotation$caption, fixed = TRUE))
  }
  expect_true(all(c("Highways & major routes", "Arterials") %in% env$roles$role))
})

test_that("C7 labels and hexagons stay inside the boundary", {
  th <- seq(0, 2 * pi, length.out = 90)[-90]; r0 <- 2300 * (1 + 0.25 * cos(3 * th))
  ring <- cbind(cx + r0 * cos(th), cy + r0 * sin(th)); sh <- st_sfc(st_polygon(list(rbind(ring, ring[1, ]))), crs = crs0); r <- env$shape_radius(sh)
  # Streets out toward the three places where this boundary pulls in (60, 180, 300 degrees, 1,725 m out), plus a named street
  # tangent to the boundary there: its label would sit just outside the boundary and must be dropped.
  dirs <- c(60, 180, 300) * pi / 180
  extra <- do.call(rbind, c(lapply(dirs, \(a) mk(cbind(cx + c(0, 3000 * cos(a)), cy + c(0, 3000 * sin(a))), "residential")),
                           list(mk(cbind(cx + 1690 * cos(dirs[1]) + c(-1, 1) * 250 * -sin(dirs[1]), cy + 1690 * sin(dirs[1]) + c(-1, 1) * 250 * cos(dirs[1])), "primary", "Edge Road"))))
  l <- env$prep_lines(fixture_roads(rbind(fixture_lines(), extra)), lon0, lat0, as.integer(round(r)), crs0, sh); ctr <- st_sfc(st_point(c(cx, cy)), crs = crs0)
  lab <- env$make_labels(l, r, crs0, ctr, sh)
  expect_false("Edge Road" %in% lab$name)
  expect_true(!is.null(lab) && nrow(lab) > 0)
  expect_true(all(lengths(st_intersects(st_as_sf(lab, coords = c("x", "y"), crs = crs0), st_buffer(sh, 0.01))) == 1))
  hx <- env$hex_density(env$segments_of(l), r, ctr, sh)$cells
  expect_true(all(lengths(st_within(st_geometry(hx), st_buffer(sh, 0.01))) == 1))
  expect_equal(sum(hx$dens * as.numeric(st_area(hx)) / 1e6), sum(hx$len) / 1000, tolerance = 1e-9)
})

test_that("C8 the server draws city limits when it can, and falls back to a circle with an explanation otherwise", {
  skip_if_not_installed("png")
  bnd <- st_transform(sq_shape(1500), 4326)
  run <- function(area, boundary) {
    calls <- with_stubs(NULL, boundary = boundary); out <- NULL
    testServer(env$server, {
      session$setInputs(query = "Elkins", find = 1)
      session$setInputs(choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = area, draw = 1)
      g <- geo(); out <<- list(shape = g$shape, radius = g$radius_m, stats = paste(as.character(output$stats$html), collapse = ""))
    })
    c(out, list(fetch = calls$fetch))
  }
  city <- run("city", bnd)
  expect_false(is.null(city$shape)); expect_equal(city$radius, round(sqrt(3000^2 / pi)), tolerance = 0.01)   # a 3 km x 3 km square: the circle of equal area has r = 1,693 m
  expect_length(city$fetch, 1); expect_false(is.null(city$fetch[[1]]$shape)); expect_match(city$stats, "inside the city limits", fixed = TRUE)
  circ <- run("circle", bnd); expect_null(circ$shape); expect_equal(circ$radius, 3000); expect_null(circ$fetch[[1]]$shape); expect_match(circ$stats, "km of the center", fixed = TRUE)
  none <- run(NULL, bnd); expect_null(none$shape)                                       # no area input behaves like the circle
  nob <- run("city", NULL); expect_null(nob$shape); expect_equal(nob$radius, 3000)       # no boundary for this match: circle
  huge <- run("city", st_transform(sq_shape(20000), 4326)); expect_null(huge$shape); expect_equal(huge$radius, 3000)   # 1,600 km2 > 1,500 km2 limit
  just_ok <- run("city", st_transform(sq_shape(19000), 4326)); expect_false(is.null(just_ok$shape))   # 1,444 km2 is within the limit
})

test_that("C9 city downloads are cached by boundary, not shared with circles or other boundaries", {
  n <- 0; get_stub <- function(...) { n <<- n + 1; list(osm_lines = paste("call", n)) }
  with_get(get_stub, local({
  a <- sq_shape(900); b <- sq_shape(950)
  r1 <- env$fetch_roads(77.1, 12.2, 900L, 32643, a); r2 <- env$fetch_roads(77.1, 12.2, 900L, 32643, a)
  expect_equal(c(r1$osm_lines, r2$osm_lines), c("call 1", "call 1")); expect_equal(n, 1)   # same boundary: cached
  expect_equal(env$fetch_roads(77.1, 12.2, 900L, 32643, b)$osm_lines, "call 2")           # different boundary: new download
  expect_equal(env$fetch_roads(77.1, 12.2, 900L, 32643)$osm_lines, "call 3")             # the circle around the same point is its own request
  rect <- function(w, h) st_sfc(st_polygon(list(cbind(cx + c(0, w, w, 0, 0), cy + c(0, 0, h, h, 0)))), crs = crs0)
  r_a <- env$fetch_roads(77.1, 12.2, 900L, 32643, rect(2000, 1000))$osm_lines             # same lower-left corner, different far corner
  expect_false(identical(r_a, env$fetch_roads(77.1, 12.2, 900L, 32643, rect(2500, 1000))$osm_lines))
  expect_false(identical(r_a, env$fetch_roads(77.1, 12.2, 900L, 32643, rect(2000, 1400))$osm_lines))
  }))
})

test_that("C10 the control panel offers the new choices", {
  html <- as.character(env$ui(NULL))
  expect_match(html, 'name="area"', fixed = TRUE); expect_match(html, "City limits", fixed = TRUE); expect_match(html, "Show local streets", fixed = TRUE)
  expect_match(html, "input.area != (&#39;|')city")                                # the radius slider appears for both circle modes, never for city-only
  expect_match(html, "Circle, city limits outlined", fixed = TRUE); expect_match(html, "Circle only", fixed = TRUE); expect_match(html, "City limits only", fixed = TRUE)
  expect_match(html, "value=\"dim\" checked", fixed = TRUE)                           # the circle with the city outlined is the default
  expect_match(html, "id=\"locals\"", fixed = TRUE)
})

test_that("C11 the figure the server builds follows the area and the local-streets switch", {
  skip_if_not_installed("png")
  bnd <- st_transform(sq_shape(1500), 4326); calls <- with_stubs(NULL, boundary = bnd)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = FALSE, style = "classic", view = "hierarchy", area = "city", locals = FALSE, draw = 1)
    p <- built(); frame <- p[[1]]$layers[[1]]$data
    expect_equal(as.numeric(st_area(frame)), as.numeric(st_area(geo()$shape)))              # the plate is the city shape, not a circle
    expect_match(p$patches$annotation$caption, "not drawn", fixed = TRUE)
    expect_match(p$patches$annotation$caption, "boundary mapped in OpenStreetMap", fixed = TRUE)
    session$setInputs(locals = TRUE)
    expect_false(grepl("not drawn", built()$patches$annotation$caption, fixed = TRUE))
    session$setInputs(area = "circle")                                                     # the area is chosen when the map is drawn, so the drawn map keeps its shape
    expect_equal(as.numeric(st_area(built()[[1]]$layers[[1]]$data)), as.numeric(st_area(geo()$shape)))
  })
})

test_that("C12 circle maps never mention a boundary and always draw their ring", {
  lines <- demo_lines_prepped()
  p <- env$make_map(lines, "T", lon0, lat0, 3000, crs0)
  expect_false(grepl("boundary", p$patches$annotation$caption, fixed = TRUE))
  expect_equal(as.numeric(st_area(p[[1]]$layers[[1]]$data)), pi * 3000^2, tolerance = 0.005)
})

test_that("C13 a sliver-thin boundary still clips (the inset falls back to the boundary itself)", {
  sliver <- st_sfc(st_polygon(list(cbind(cx + c(0, 3000, 3000, 0, 0), cy + c(0, 0, 0.1, 0.1, 0)))), crs = crs0)
  expect_equal(length(st_buffer(sliver, -env$shape_radius(sliver) * 0.012)[!st_is_empty(st_buffer(sliver, -env$shape_radius(sliver) * 0.012))]), 0)   # the inset really is empty here
  l <- st_transform(mk(cbind(cx + c(-100, 3100), cy + c(0.05, 0.05)), "primary", "Thin"), 4326)
  p <- env$prep_lines(list(osm_lines = l), lon0, lat0, as.integer(round(env$shape_radius(sliver))), crs0, sliver)
  expect_false(is.null(p)); expect_equal(sum(p$km) * 1000, 3000, tolerance = 1e-3)
})


# ---- circle with the city limits outlined and everything outside dimmed ------------------------------------
veil_layer <- function(p, want_m2, tol = 0.01) {      # the dimming layer is the one whose polygon has the expected area
  for (ly in p[[1]]$layers) { d <- ly$data
    if (inherits(d, "sf") && nrow(d) == 1 && abs(as.numeric(st_area(d)) - want_m2) / want_m2 < tol) return(ly) }
  NULL
}

test_that("D1 the dimming layer covers exactly the circle minus the city, and the city is outlined", {
  ln <- demo_lines_prepped(); inside <- sq_shape(1500)                    # 3 km square fully inside the 3 km circle
  circ_area <- pi * 3000^2; want <- circ_area - 3000^2
  p <- env$make_map(ln, "Testville", lon0, lat0, 3000L, crs0, inside = inside)
  ly <- veil_layer(p, want); expect_false(is.null(ly))
  expect_equal(ly$aes_params$alpha, 0.74); expect_equal(ly$aes_params$fill, env$get_style("classic")$plate)
  expect_false(is.null(veil_layer(p, 3000^2)))                          # the dashed outline of the city (a polygon with no fill)
  base <- env$make_map(ln, "Testville", lon0, lat0, 3000L, crs0)
  expect_equal(length(p[[1]]$layers) - length(base[[1]]$layers), 3)       # veil, ring redrawn, outline
  expect_null(veil_layer(base, want))
  expect_match(p$patches$annotation$caption, "dimmed", fixed = TRUE); expect_false(grepl("dimmed", base$patches$annotation$caption))
})

has_veil <- function(p) any(vapply(p[[1]]$layers, function(ly) isTRUE(ly$aes_params$alpha == 0.74), logical(1)))

test_that("D2 dimming changes only the drawing: the subtitle and the street data are identical", {
  ln <- demo_lines_prepped(); inside <- sq_shape(1500)
  p <- env$make_map(ln, "Testville", lon0, lat0, 3000L, crs0, inside = inside); q <- env$make_map(ln, "Testville", lon0, lat0, 3000L, crs0)
  expect_true(nzchar(p$patches$annotation$subtitle)); expect_equal(p$patches$annotation$subtitle, q$patches$annotation$subtitle)
  expect_true(has_veil(p)); expect_false(has_veil(q))
  road_data <- function(pl) Filter(function(d) inherits(d, "sf") && nrow(d) > 1, lapply(pl[[1]]$layers, function(ly) ly$data))
  expect_equal(sum(vapply(road_data(p), nrow, 1L)), sum(vapply(road_data(q), nrow, 1L)))   # no street is removed or split
})

test_that("D3 a city that covers the whole circle, or misses it entirely, still draws in every map type", {
  skip_if_not_installed("png")
  ln <- demo_lines_prepped()
  miss <- st_sfc(st_polygon(list(cbind(cx + 20000 + c(0, 500, 500, 0, 0), cy + c(0, 0, 500, 500, 0)))), crs = crs0)
  for (ins in list(sq_shape(9000), miss))
    for (v in names(env$views)) {
      p <- env$make_map(ln, "Testville", lon0, lat0, 3000L, crs0, view = v, inside = ins)
      f <- tempfile(fileext = ".png"); env$draw_figure(p, f, "png", "classic", preview = TRUE); expect_gt(file.size(f), 5000)
    }
  expect_false(has_veil(env$make_map(ln, "Testville", lon0, lat0, 3000L, crs0, inside = sq_shape(9000))))   # covers the circle: nothing to dim
  expect_false(grepl("dimmed", env$make_map(ln, "Testville", lon0, lat0, 3000L, crs0, inside = sq_shape(9000))$patches$annotation$caption))
  expect_false(has_veil(env$make_map(ln, "Testville", lon0, lat0, 3000L, crs0, inside = miss)))   # misses the circle: no outline to draw, so no veil either
})

test_that("D4 the server draws the circle and keeps the boundary only for dimming, with a fallback when there is none", {
  skip_if_not_installed("png")
  bnd <- st_transform(sq_shape(1500), 4326)
  run <- function(boundary, area = "dim") {
    calls <- with_stubs(NULL, boundary = boundary); out <- NULL
    testServer(env$server, {
      session$setInputs(query = "Elkins", find = 1)
      session$setInputs(choice = "1", radius = 3, labels = FALSE, style = "classic", view = "hierarchy", area = area, locals = FALSE, draw = 1)
      g <- geo(); p <- built(); out <<- list(shape = g$shape, inside = g$inside, radius = g$radius_m, layers = length(p[[1]]$layers))
    })
    c(out, list(fetch = calls$fetch))
  }
  d <- run(bnd)
  expect_null(d$shape); expect_false(is.null(d$inside)); expect_equal(d$radius, 3000); expect_null(d$fetch[[1]]$shape)   # circle download, full radius
  expect_equal(as.numeric(st_area(d$inside)), 3000^2, tolerance = 0.01)
  nob <- run(NULL); expect_null(nob$inside); expect_equal(nob$radius, 3000); expect_lt(nob$layers, d$layers)           # no boundary: plain circle, nothing dimmed
  expect_null(run(bnd, "circle")$inside); expect_null(run(bnd, "city")$inside)                                          # the other two modes never dim
  expect_false(is.null(run(bnd, "city")$shape))
})

test_that("D5 the radius slider changes the circle in dimming mode, and the cached download is the circle's", {
  skip_if_not_installed("png")
  bnd <- st_transform(sq_shape(1500), 4326); calls <- with_stubs(NULL, boundary = bnd); r <- NULL
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 2, labels = FALSE, style = "classic", view = "hierarchy", area = "dim", locals = FALSE, draw = 1)
    r <<- geo()$radius_m
  })
  expect_equal(r, 2000)
})


# ---- street downloads with hard time limits and failover --------------------------------------------------------
overpass_xml <- function() paste0('<?xml version="1.0" encoding="UTF-8"?>\n<osm version="0.6" generator="Overpass API">\n<note>x</note><meta osm_base="2026-11-11T00:00:00Z"/>\n',
  '<node id="1" lat="38.9200" lon="-79.8500"/><node id="2" lat="38.9210" lon="-79.8500"/>\n',
  '<way id="10"><nd ref="1"/><nd ref="2"/><tag k="highway" v="residential"/><tag k="name" v="Test Street"/></way>\n</osm>')
ok_resp <- function() httr2::response(200, body = charToRaw(overpass_xml()))
reset_mirrors <- function() env$mirror_state$bad_until <- list()
bb_test <- c(xmin = -79.86, ymin = 38.91, xmax = -79.84, ymax = 38.93)
mirrors <- function() env$overpass_mirrors
# Status pages answer "slots free" and are not counted as download attempts; the server list is pinned so regions do not change it.
mk_status <- function(f) function(req) if (grepl("/status$", req$url)) httr2::response(200, body = charToRaw("Connected as: 1\n2 slots available now.\n")) else f(req)
Sys.setenv(STREET_MAPS_OVERPASS = paste(env$overpass_mirrors, collapse = ","))

test_that("E1 a failing first server hands over to the second, the answer parses, and it is cached", {
  reset_mirrors(); hit <- character()
  mock <- function(req) { hit <<- c(hit, req$url); if (length(hit) == 1) stop("connection reset") else ok_resp() }
  r <- httr2::with_mocked_responses(mk_status(mock), env$fetch_roads(33.3333, 44.4444, 3000L, 32636))
  expect_equal(nrow(r$osm_lines), 1); expect_equal(r$osm_lines$name, "Test Street"); expect_equal(hit, mirrors()[1:2])
  expect_true(file.exists(file.path(env$roads_cache_dir(), "roads_33.3333_44.4444_3000.rds")))
  hit <- character(); r2 <- httr2::with_mocked_responses(mk_status(mock), env$fetch_roads(33.3333, 44.4444, 3000L, 32636))
  expect_equal(length(hit), 0); expect_equal(nrow(r2$osm_lines), 1)                      # second call served from the cache
})

test_that("E2 a slow-answer error hidden inside a normal 200 response, and HTTP 429 and 504, all fail over", {
  cases <- list(list(httr2::response(200, body = charToRaw('<osm><remark> runtime error: Query timed out in "query" at line 5 after 26 seconds. </remark></osm>')), "could not finish"),
                list(httr2::response(429, body = charToRaw("<html>Too Many Requests</html>")), "status 429"),
                list(httr2::response(504, body = charToRaw("<html>Gateway Timeout</html>")), "status 504"))
  for (cs in cases) {
    bad <- cs[[1]]; reset_mirrors(); hit <- character()
    mock <- function(req) { hit <<- c(hit, req$url); if (length(hit) == 1) bad else ok_resp() }
    r <- httr2::with_mocked_responses(mk_status(mock), env$overpass_download(bb_test))
    expect_false(is.null(r$data)); expect_equal(hit, mirrors()[1:2]); expect_false(r$tries[[1]]$ok); expect_true(r$tries[[2]]$ok)
    expect_match(r$tries[[1]]$msg, cs[[2]], fixed = TRUE)                                    # the reason is recorded, not just "failed"
  }
  reset_mirrors()
})

test_that("E3 when every server fails: two passes over all servers, a plain message, and nothing is cached", {
  reset_mirrors(); n <- 0
  expect_error(httr2::with_mocked_responses(mk_status(function(req) { n <<- n + 1; httr2::response(503) }), env$fetch_roads(55.5555, 66.6666, 3000L, 32637)),
               "did not answer in time \\(6 attempts")
  expect_equal(n, 6)
  expect_false(file.exists(file.path(env$roads_cache_dir(), "roads_55.5555_66.6666_3000.rds")))
})

test_that("E4 each attempt carries a hard time limit that follows the area size (independent oracle)", {
  expect_equal(c(env$attempt_cap(1), env$attempt_cap(10), env$attempt_cap(50), env$attempt_cap(100), env$attempt_cap(300)), c(25, 25, 40, 60, 60))
  reset_mirrors(); seen <- list()
  mock <- function(req) { seen[[length(seen) + 1]] <<- list(ms = req$options$timeout_ms, body = utils::URLdecode(req$body$data$data), ua = req$options$useragent); httr2::response(500) }
  invisible(httr2::with_mocked_responses(mk_status(mock), env$overpass_download(bb_test, area_km2 = 50)))
  expect_equal(vapply(seen, function(x) x$ms, 0), c(rep(40000, 3), rep(88000, 3)), tolerance = 1e-3)       # first pass 40 s, second pass 2.2 x
  expect_match(seen[[1]]$body, "[out:xml][timeout:40]", fixed = TRUE)                       # the server is told the same limit
  expect_match(seen[[1]]$body, "highway", fixed = TRUE); expect_match(seen[[1]]$body, "38.91", fixed = TRUE)
  expect_match(seen[[1]]$ua, "street-hierarchy-maps", fixed = TRUE)
})

test_that("E5 a server that just failed goes to the back of the line, and returns to its place after the cooldown", {
  reset_mirrors(); t0 <- as.POSIXct("2026-11-11 12:00:00", tz = "UTC"); m <- mirrors()
  expect_equal(env$order_mirrors(m, t0), m)
  env$mark_mirror(m[1], FALSE, t0); expect_equal(env$order_mirrors(m, t0 + 10), m[c(2, 3, 1)])
  env$mark_mirror(m[2], FALSE, t0); expect_equal(env$order_mirrors(m, t0 + 10), m[c(3, 1, 2)])
  expect_equal(env$order_mirrors(m, t0 + 301), m)                                          # five minutes later, forgiven
  env$mark_mirror(m[1], TRUE, t0 + 20); expect_equal(env$order_mirrors(m, t0 + 30), m[c(1, 3, 2)])   # a success clears the mark at once
  reset_mirrors()
})

test_that("E6 the next download starts with a server that answered, not the one that failed last time", {
  reset_mirrors(); hit <- character()
  mock <- function(req) { hit <<- c(hit, req$url); if (grepl("overpass-api.de", req$url)) stop("timeout") else ok_resp() }
  httr2::with_mocked_responses(mk_status(mock), env$overpass_download(bb_test)); expect_equal(hit, mirrors()[1:2])
  hit <- character(); httr2::with_mocked_responses(mk_status(mock), env$overpass_download(bb_test)); expect_equal(hit[1], mirrors()[2])
  reset_mirrors()
})

test_that("E7 the overall deadline bounds the whole download, and no attempt can outlast it", {
  reset_mirrors(); now <- 0; clock <- function() as.POSIXct("2026-11-11 12:00:00", tz = "UTC") + now
  getter <- function(u, bb, cap) { now <<- now + cap; stop("timed out") }                  # every server uses its whole allowance, then fails
  r <- env$overpass_download(bb_test, area_km2 = 10, deadline_s = 150, clock = clock, getter = getter)
  caps <- vapply(r$tries, function(t) t$cap, 0)
  expect_null(r$data); expect_equal(caps, c(25, 25, 25, 55, 20)); expect_lte(sum(caps), 150)
  reset_mirrors(); now <- 0
  r2 <- env$overpass_download(bb_test, area_km2 = 10, deadline_s = 60, clock = clock, getter = getter)
  expect_lte(sum(vapply(r2$tries, function(t) t$cap, 0)), 60)
  expect_equal(length(env$overpass_download(bb_test, deadline_s = 5, getter = function(...) stop("never"))$tries), 0)   # no time for a useful attempt
  reset_mirrors()
})

test_that("E8 progress callback reports each attempt in order, and fetch_roads passes it through", {
  reset_mirrors(); log <- list(); hit <- 0
  mock <- function(req) { hit <<- hit + 1; if (hit < 3) httr2::response(502) else ok_resp() }
  r <- httr2::with_mocked_responses(mk_status(mock), env$fetch_roads(77.7777, 22.2222, 2000L, 32638, on_try = function(i, n, url, cap) log[[length(log) + 1]] <<- list(i = i, n = n, url = url, cap = cap)))
  expect_equal(vapply(log, function(x) x$i, 0), 1:3); expect_equal(vapply(log, function(x) x$url, ""), mirrors()[1:3]); expect_true(all(vapply(log, function(x) x$n, 0) == 6))
  expect_equal(nrow(r$osm_lines), 1); reset_mirrors()
})

test_that("E9 the app shows which server it is trying, and says when it switches", {
  skip_if_not_installed("png")
  calls <- with_stubs(NULL); texts <- character()
  env$fetch_roads <- function(lon, lat, radius_m, crs_m, shape = NULL, on_try = NULL, detail = "all") {
    on_try(1, 6, "https://overpass-api.de/api/interpreter", 25); on_try(2, 6, "https://overpass.private.coffee/api/interpreter", 25); fixture_roads() }
  testthat::local_mocked_bindings(setProgress = function(value = NULL, message = NULL, detail = NULL, session = NULL) { texts <<- c(texts, detail); invisible() }, .package = "shiny")
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = FALSE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, draw = 1)
    invisible(geo())
  })
  expect_true(any(grepl("^Downloading from overpass-api.de \\(up to 25 s\\)", texts)))
  expect_true(any(grepl("^Trying another server: overpass.private.coffee \\(up to 25 s\\)", texts)))
})

# ---- download counts ---------------------------------------------------------------------------------------------
test_that("G1 log_download writes one header and one line per download, and refuses anything unexpected", {
  f <- tempfile(fileext = ".csv"); t1 <- as.POSIXct("2026-11-11 08:30:15", tz = "UTC")
  expect_true(env$log_download("pdf", "speed", "dusk", when = t1, file = f)); expect_true(env$log_download("png", "hierarchy", "classic", when = t1 + 60, file = f))
  expect_equal(readLines(f), c("time,format,view,style", "2026-11-11T08:30:15Z,pdf,speed,dusk", "2026-11-11T08:31:15Z,png,hierarchy,classic"))
  before <- readLines(f)
  for (bad in list(list("exe", "speed", "dusk"), list("pdf", "x,y", "dusk"), list("pdf", "speed", "pink"), list(c("png", "pdf"), "speed", "dusk"), list(NULL, "speed", "dusk")))
    expect_false(do.call(env$log_download, c(bad, list(file = f))))
  expect_equal(readLines(f), before)                                                          # nothing was appended
  expect_false(env$log_download("png", "speed", "dusk", file = file.path(tempdir(), "no_such_dir", "x.csv")))   # an unwritable log never breaks a download
})

test_that("G2 read_counts tolerates a missing file, damaged lines, and foreign files", {
  expect_equal(nrow(env$read_counts(tempfile())), 0)
  f <- tempfile(); writeLines(c("time,format,view,style", "2026-11-11T08:30:15Z,png,speed,dusk", "garbage line", "2026-13-45T99:99:99Z,png,speed,dusk",
                                "2026-11-11T09:00:00Z,exe,speed,dusk", "2026-11-11T10:00:00Z,pdf,density,mono"), f)
  d <- env$read_counts(f); expect_equal(d$format, c("png", "pdf")); expect_equal(nrow(d), 2)
  g <- tempfile(); writeLines(c("a,b", "1,2"), g); expect_equal(nrow(env$read_counts(g)), 0)
  h <- tempfile(); writeLines("", h); expect_equal(nrow(env$read_counts(h)), 0)
})

test_that("G3 summarize_counts: rolling windows and per-format totals against hand counts", {
  now <- as.POSIXct("2026-11-11 12:00:00", tz = "UTC")
  d <- data.frame(time = now - c(1800, 3600 * 24 + 1, 86400 * 2, 86400 * 8, 86400 * 40), format = c("png", "png", "pdf", "svg", "png"), view = "speed", style = "dusk", stringsAsFactors = FALSE)
  s <- env$summarize_counts(d, now)
  expect_equal(c(s$total, s$day, s$week), c(5, 1, 3)); expect_equal(s$first, now - 86400 * 40)
  tb <- s$table; expect_equal(tb$format, names(env$export_formats))
  expect_equal(tb$all_time[tb$format == "PNG (300 dpi)"], 3); expect_equal(tb$last7[tb$format == "PNG (300 dpi)"], 2)
  expect_equal(tb$all_time[tb$format == "SVG (vector)"], 1); expect_equal(tb$last7[tb$format == "SVG (vector)"], 0); expect_equal(tb$all_time[tb$format == "TIFF (300 dpi)"], 0)
  e <- env$summarize_counts(env$read_counts(tempfile()), now); expect_equal(c(e$total, e$day, e$week), c(0, 0, 0))
})

test_that("G4 the log location honors STREET_MAPS_DATA and falls back to the temp folder when it cannot be written", {
  old <- Sys.getenv("STREET_MAPS_DATA"); withr::defer(Sys.setenv(STREET_MAPS_DATA = old))
  d <- file.path(tempdir(), "custom_data"); Sys.setenv(STREET_MAPS_DATA = d); expect_equal(env$data_dir(), d); expect_true(dir.exists(d))
  blocker <- tempfile(); writeLines("x", blocker); Sys.setenv(STREET_MAPS_DATA = file.path(blocker, "sub"))
  expect_equal(env$data_dir(), tempdir())
})

test_that("G5 a download is logged once per file, the count shown follows it, and a repeat download is served from the cache", {
  skip_if_not_installed("png")
  calls <- with_stubs(NULL); unlink(env$counts_file()); unlink(list.files(env$export_dir(), full.names = TRUE))
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = FALSE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, draw = 1)
    session$flushReact(); expect_equal(counts()$total, 0)                                           # the panel is already live before the first download
    session$setInputs(format = "png"); f1 <- output$download; m1 <- file.mtime(list.files(env$export_dir(), full.names = TRUE))
    session$flushReact(); expect_equal(counts()$total, 1)                                           # and updates at once, without waiting for the timer
    expect_equal(env$read_counts()$format, "png"); expect_equal(dim(png::readPNG(f1))[1:2], c(3000, 2400))
    Sys.sleep(1.1); f2 <- output$download
    expect_equal(nrow(env$read_counts()), 2)                                                      # a repeat is still a download
    expect_equal(file.mtime(list.files(env$export_dir(), full.names = TRUE)), m1)                 # but the file was not drawn again
    expect_equal(unname(tools::md5sum(f1)), unname(tools::md5sum(f2)))
    session$setInputs(format = "svg"); invisible(output$download)
    session$setInputs(style = "dusk"); session$setInputs(format = "png"); f3 <- output$download     # another style is a new picture
    expect_false(unname(tools::md5sum(f1)) == unname(tools::md5sum(f3)))
    d <- env$read_counts(); expect_equal(as.vector(table(d$format)[c("png", "svg")]), c(3, 1)); expect_equal(d$style, c("classic", "classic", "classic", "dusk"))
    session$flushReact(); expect_equal(counts()$total, 4)
  })
})

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
  expect_match(env$app_css, ".is-expanded .brand > :not(.expand-btn) { display: none; }", fixed = TRUE)   # the header shrinks to the exit button
})

test_that("G6b expanding folds the sidebar away and leaving brings it back", {
  calls <- list(); testthat::local_mocked_bindings(toggle_sidebar = function(id, open = NULL, ...) { calls[[length(calls) + 1]] <<- list(id = id, open = open); invisible() }, .package = "bslib")
  testServer(env$server, {
    session$setInputs(expanded = TRUE); session$setInputs(expanded = FALSE); session$setInputs(expanded = TRUE)
  })
  expect_equal(vapply(calls, function(x) x$id, ""), rep("sb", 3)); expect_equal(vapply(calls, function(x) x$open, TRUE), c(FALSE, TRUE, FALSE))
})

test_that("G7 the counts panel text: empty state, numbers with separators, and the honest limits", {
  skip_if_not_installed("png")
  unlink(env$counts_file()); calls <- with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "x", find = 1)
    expect_match(as.character(output$counts$html), "No maps have been downloaded yet")
    for (i in 1:1203) env$log_download("pdf", "speed", "dusk")
    counts_tick(counts_tick() + 1); session$flushReact()
    h <- as.character(output$counts$html)
    expect_match(h, "1,203"); expect_match(h, "cannot tell whether a file was then saved", fixed = TRUE); expect_match(h, "Nothing about the place or the person is recorded", fixed = TRUE)
  })
})

test_that("G8 export cache keys differ for every input that changes the picture, and pruning keeps the newest files", {
  k <- function(...) env$export_key("Elkins", 1, 2, 3000, NULL, NULL, "hierarchy", "classic", FALSE, TRUE, "png", ...)
  expect_equal(k("v1"), k("v1")); expect_false(k("v1") == k("v2"))
  expect_false(env$export_key("A", 1) == env$export_key("A", 2)); expect_false(env$export_key("A", NULL) == env$export_key("A", 1))
  unlink(list.files(env$export_dir(), full.names = TRUE))
  for (i in 1:15) { f <- file.path(env$export_dir(), sprintf("f%02d.png", i)); writeLines("x", f); Sys.setFileTime(f, Sys.time() + i) }
  expect_equal(env$prune_exports(12), 3); kept <- list.files(env$export_dir())
  expect_equal(length(kept), 12); expect_false(any(c("f01.png", "f02.png", "f03.png") %in% kept)); expect_true("f15.png" %in% kept)
})

# ---- popup launcher ----------------------------------------------------------------------------------------------
launcher_path <- Sys.getenv("LAUNCH_FILE", file.path(dirname(normalizePath(app_file)), "launch.R"))
lenv <- new.env(parent = globalenv()); suppressMessages(sys.source(launcher_path, envir = lenv))

test_that("L1 find_app_browser: an override wins, a missing override yields NA rather than a guess, and Windows locations are searched", {
  exe <- tempfile(fileext = ".sh"); writeLines("#!/bin/sh", exe)
  expect_equal(lenv$find_app_browser(override = exe), exe); expect_true(is.na(lenv$find_app_browser(override = file.path(tempdir(), "nope"))))
  root <- file.path(tempdir(), "pf"); dir.create(file.path(root, "Microsoft/Edge/Application"), recursive = TRUE); writeLines("x", file.path(root, "Microsoft/Edge/Application/msedge.exe"))
  old <- Sys.getenv(c("PROGRAMFILES", "PROGRAMFILES(X86)", "LOCALAPPDATA"), unset = NA); withr::defer(for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, setNames(list(old[[n]]), n)))
  Sys.setenv(PROGRAMFILES = root); Sys.setenv(`PROGRAMFILES(X86)` = file.path(tempdir(), "none")); Sys.setenv(LOCALAPPDATA = file.path(tempdir(), "none"))
  expect_equal(lenv$find_app_browser(os = "Windows", override = ""), file.path(root, "Microsoft/Edge/Application/msedge.exe"))
})

test_that("L2 popup_args asks for an app window of the requested size at the right address", {
  a <- lenv$popup_args("http://127.0.0.1:4321", 1000, 700)
  expect_true("--app=http://127.0.0.1:4321" %in% a); expect_true("--window-size=1000,700" %in% a)
})

test_that("L3 the launcher opens a popup when it can, and otherwise falls back to the default browser with a message", {
  got <- NULL; opened <- character(); old <- options(browser = function(url) opened <<- c(opened, url)); withr::defer(options(old))
  go <- lenv$make_launcher("popup", find = function() "/x/chrome", spawn = function(exe, args) got <<- list(exe = exe, args = args))
  go("http://127.0.0.1:5000"); expect_equal(got$exe, "/x/chrome"); expect_true("--app=http://127.0.0.1:5000" %in% got$args); expect_length(opened, 0)
  expect_message(lenv$make_launcher("popup", find = function() NA_character_)("http://127.0.0.1:5001"), "default browser"); expect_equal(opened, "http://127.0.0.1:5001")
  opened <- character()
  expect_message(lenv$make_launcher("popup", find = function() "/x/chrome", spawn = function(...) stop("cannot start"))("http://127.0.0.1:5002"), "default browser"); expect_equal(opened, "http://127.0.0.1:5002")
  opened <- character(); never <- function(...) stop("popup must not be used in browser mode")
  lenv$make_launcher("browser", find = never, spawn = never)("http://127.0.0.1:5003"); expect_equal(opened, "http://127.0.0.1:5003")
  expect_error(lenv$make_launcher("window"))
})


test_that("E10 the server list can be replaced with STREET_MAPS_OVERPASS, ignoring entries that are not web addresses", {
  old <- Sys.getenv("STREET_MAPS_OVERPASS", unset = NA); withr::defer(if (is.na(old)) Sys.unsetenv("STREET_MAPS_OVERPASS") else Sys.setenv(STREET_MAPS_OVERPASS = old))
  Sys.setenv(STREET_MAPS_OVERPASS = " https://a.example/api/interpreter , ftp://bad, notaurl,https://b.example/api/interpreter ")
  e2 <- new.env(parent = globalenv()); suppressMessages(sys.source(app_file, envir = e2))
  expect_equal(e2$overpass_mirrors, c("https://a.example/api/interpreter", "https://b.example/api/interpreter"))
  Sys.setenv(STREET_MAPS_OVERPASS = "garbage"); e3 <- new.env(parent = globalenv()); suppressMessages(sys.source(app_file, envir = e3))
  expect_equal(e3$overpass_mirrors, mirrors())                                              # nothing usable: the built-in list stays
})


# ---- explainable street downloads ----------------------------------------------------------------------------
with_env <- function(val, code) { old <- Sys.getenv("STREET_MAPS_OVERPASS", unset = NA); Sys.setenv(STREET_MAPS_OVERPASS = val)
  on.exit(if (is.na(old)) Sys.unsetenv("STREET_MAPS_OVERPASS") else Sys.setenv(STREET_MAPS_OVERPASS = old)); force(code) }
bbox_at <- function(lon, lat, d = 0.05) c(xmin = lon - d, ymin = lat - d, xmax = lon + d, ymax = lat + d)

test_that("N1 regional servers go first for North America and Europe, the main site's download only for small areas, and a custom list wins", {
  with_env("", {
    na <- env$overpass_mirrors_for(bbox_at(-79.85, 38.92)); expect_equal(na[1], "https://ovp.dresel.systems/api-na/interpreter")
    eu <- env$overpass_mirrors_for(bbox_at(2.35, 48.85)); expect_equal(eu[1], "https://ovp.dresel.systems/api/interpreter")
    asia <- env$overpass_mirrors_for(bbox_at(139.7, 35.7)); expect_equal(asia[1], env$overpass_mirrors[1]); expect_false(any(grepl("dresel", asia)))
    expect_equal(tail(na, 1), env$osm_api_url)                                              # small area: last resort present
    big <- env$overpass_mirrors_for(bbox_at(-79.85, 38.92, d = 0.5)); expect_false(env$osm_api_url %in% big)   # large area: never
    expect_false(any(grepl("kumi", env$overpass_mirrors))); expect_equal(anyDuplicated(na), 0)
  })
  with_env("https://a.example/api/interpreter, junk, http://b.example/x", expect_equal(env$overpass_mirrors_for(bbox_at(-79.85, 38.92)), c("https://a.example/api/interpreter", "http://b.example/x")))
})

test_that("N2 the status check skips busy or dead servers fast and tolerates unreadable pages", {
  url <- "https://x.example/api/interpreter"; resp <- function(txt, st = 200) function(req) httr2::response(st, body = charToRaw(txt))
  ok <- function(txt, st = 200) httr2::with_mocked_responses(resp(txt, st), env$overpass_ready(url))
  expect_true(ok("Connected as: 1\n2 slots available now.\n"))
  expect_true(ok("<html>whatever</html>")); expect_true(ok("nope", 404))                      # no usable status page: just try the server
  expect_error(ok("Connected as: 1\n0 slots available now.\nSlot available after: 2026-11-11T12:00:00Z, in 40 seconds.\n"), "no free query slot for 40 s")
  expect_error(httr2::with_mocked_responses(function(req) stop("Timeout was reached"), env$overpass_ready(url)), "status check failed: Timeout")
  t0 <- Sys.time(); expect_true(ok("0 slots available now.\nSlot available after: 2026-11-11T12:00:00Z, in 1 seconds.\n")); expect_gte(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1.5)
})

test_that("N3 failures are classified, and the message names each server and its reason", {
  ex <- function(m) env$explain_failure(m)[1]
  expect_equal(ex("Could not resolve host: x"), "unreachable"); expect_equal(ex("Timeout was reached"), "slow"); expect_equal(ex("server answered with status 429"), "busy")
  expect_equal(ex("server answered with status 503"), "busy"); expect_equal(ex("server could not finish the query"), "busy"); expect_equal(ex("no free query slot for 40 s"), "busy")
  expect_equal(ex("status check failed: Timeout was reached"), "slow"); expect_equal(ex("Failed to connect to x"), "unreachable"); expect_equal(ex("server answered with status 403"), "rejected")
  tr <- function(u, m) list(url = u, ok = FALSE, secs = 10, cap = 25, msg = m)
  m1 <- env$download_failure_message(list(tr("https://a.example/i", "server answered with status 429"), tr("https://b.example/i", "Timeout was reached"), tr("https://a.example/i", "x")))
  expect_match(m1, "a.example: it is rate-limiting requests; b.example: it did not answer in time", fixed = TRUE); expect_match(m1, "3 attempts over 30 seconds", fixed = TRUE)
  expect_equal(lengths(regmatches(m1, gregexpr("a.example", m1, fixed = TRUE))), 1)           # one line per server
  m2 <- env$download_failure_message(list(tr("https://a.example/i", "Could not resolve host: a.example"), tr("https://b.example/i", "Failed to connect")))
  expect_match(m2, "^Could not reach any OpenStreetMap server"); expect_match(m2, "internet connection, VPN, or firewall")
  expect_match(env$download_failure_message(list(tr(env$osm_api_url, "server answered with status 400 too many nodes (limit is 50000)"))), "smaller radius")
  expect_match(env$download_failure_message(list()), "not enough time")
})

test_that("N4 the OpenStreetMap map-download fallback keeps only street classes we draw", {
  xml <- paste0('<?xml version="1.0"?>\n<osm version="0.6">\n<node id="1" lat="38.9200" lon="-79.8500"/><node id="2" lat="38.9210" lon="-79.8500"/><node id="3" lat="38.9220" lon="-79.8500"/>\n',
    '<way id="10"><nd ref="1"/><nd ref="2"/><tag k="highway" v="residential"/><tag k="name" v="Keep Street"/></way>\n',
    '<way id="11"><nd ref="2"/><nd ref="3"/><tag k="highway" v="footway"/><tag k="name" v="Path"/></way>\n',
    '<way id="12"><nd ref="1"/><nd ref="3"/><tag k="waterway" v="stream"/></way>\n</osm>')
  mock <- function(req) { expect_match(req$url, "bbox=-79.86", fixed = TRUE); httr2::response(200, body = charToRaw(xml)) }
  r <- httr2::with_mocked_responses(mock, env$overpass_get(env$osm_api_url, bb_test, 30))
  expect_equal(r$osm_lines$name, "Keep Street")
  err <- tryCatch(httr2::with_mocked_responses(function(req) httr2::response(400, body = charToRaw("You requested too many nodes (limit is 50000)")), env$overpass_get(env$osm_api_url, bb_test, 30)), error = function(e) conditionMessage(e))
  expect_match(err, "status 400"); expect_equal(env$explain_failure(err)[1], "dense")
  calls <- character(); reset_mirrors()
  g <- function(u, bb, cap) { calls <<- c(calls, u); stop("timed out") }
  r2 <- env$overpass_download(bb_test, mirrors = c("https://a.example/i", env$osm_api_url), getter = g, deadline_s = 300)
  expect_equal(calls, c("https://a.example/i", env$osm_api_url, "https://a.example/i"))      # the main-site download runs once, not twice
  reset_mirrors()
})

test_that("N5 downloaded streets are kept on disk for 30 days, then fetched again", {
  with_env(paste(env$overpass_mirrors, collapse = ","), {
    reset_mirrors(); n <- 0; mock <- mk_status(function(req) { n <<- n + 1; ok_resp() })
    f <- file.path(env$roads_cache_dir(), "roads_9.1111_9.2222_1500.rds"); unlink(f)
    httr2::with_mocked_responses(mock, env$fetch_roads(9.1111, 9.2222, 1500L, 32632)); expect_true(file.exists(f)); expect_equal(n, 1)
    httr2::with_mocked_responses(mock, env$fetch_roads(9.1111, 9.2222, 1500L, 32632)); expect_equal(n, 1)
    Sys.setFileTime(f, Sys.time() - 31 * 86400); httr2::with_mocked_responses(mock, env$fetch_roads(9.1111, 9.2222, 1500L, 32632)); expect_equal(n, 2)
    writeLines("not an rds", f); Sys.setFileTime(f, Sys.time()); httr2::with_mocked_responses(mock, env$fetch_roads(9.1111, 9.2222, 1500L, 32632)); expect_equal(n, 3)   # unreadable file is replaced
    expect_true(startsWith(f, file.path(env$data_dir(), "roads_cache")))
  })
})


# ---- direction and density redesign ----------------------------------------------------------------------------------
test_that("P1 patchwork is attached by the app itself, so the first map drawn can be a direction map in a fresh session", {
  src <- readLines(Sys.getenv("APP_FILE", "app.R"), warn = FALSE)
  expect_true(any(grepl("^library\\(patchwork\\)", src)))
})

test_that("P2 direction map: streets that never touch the city are one neutral color, streets in the city keep their heading colors", {
  lines <- demo_lines_prepped(); shape <- sq_shape(800)
  p <- env$make_map(lines, "Test", lon0, lat0, 3000, crs0, view = "direction", inside = shape)
  map_plot <- p[[1]]
  cols <- unlist(lapply(map_plot$layers, function(l) l$aes_params$colour %||% l$aes_params$color))
  pal <- env$styles$classic
  touches <- lengths(st_intersects(lines, st_make_valid(shape))) > 0
  skip_if(all(touches) || !any(touches), "fixture needs streets both inside and outside the square")
  expect_true(pal$cols[5] %in% cols)                                                   # the neutral layer exists
  neutral_layer <- Filter(function(l) identical(l$aes_params$colour, pal$cols[5]), map_plot$layers)[[1]]
  expect_equal(nrow(neutral_layer$data), sum(!touches))                                 # exactly the streets outside the city
  expect_equal(sum(vapply(Filter(function(l) !is.null(l$aes_params$colour) && l$aes_params$colour %in% pal$dir, map_plot$layers), function(l) nrow(l$data), 0)), sum(touches))
  p0 <- env$make_map(lines, "Test", lon0, lat0, 3000, crs0, view = "direction")          # no city outline: nothing is neutral
  expect_false(any(vapply(p0[[1]]$layers, function(l) identical(l$aes_params$colour, pal$cols[5]), TRUE)))
})

test_that("P3 the compass labels sit outside the longest bar, and tiny shares read as less than one percent", {
  lines <- demo_lines_prepped()
  p <- env$make_map(lines, "Test", lon0, lat0, 3000, crs0, view = "direction")
  rose <- p[[2]][[1]]; bars <- p[[2]][[2]]
  ann <- Filter(function(l) inherits(l$geom, "GeomText"), rose$layers)[[1]]
  top <- max(rose$data$h)
  expect_equal(nrow(ann$data), 4); expect_true(all(ann$data$y >= 1.2 * top))                                       # label radius clears the ring at 1.0
  txt <- ggplot2::layer_data(bars, 2)$label
  expect_true(all(grepl("^(<1|[0-9]+)%$", txt)))
  expect_true(any(txt == "<1%") == any(round(bars$data$pct) < 1 | bars$data$pct < 0.5))
})

test_that("P4 density legend has room: title above the keys and a larger gap before the caption", {
  p <- env$make_map(demo_lines_prepped(), "Test", lon0, lat0, 3000, crs0, view = "density")
  g <- p[[1]]$guides$guides$fill %||% NULL
  th <- p[[1]]$theme
  expect_gte(as.numeric(th$legend.key.width), 1.5)
  cap_margin <- p$patches$annotation$theme$plot.caption$margin
  expect_gte(as.numeric(cap_margin[1]), 20)
  hier <- env$make_map(demo_lines_prepped(), "Test", lon0, lat0, 3000, crs0, view = "hierarchy")
  expect_lt(as.numeric(hier$patches$annotation$theme$plot.caption$margin[1]), 20)                          # other maps keep their tighter spacing
})


# ---- purposeful palettes, a radius that fits the place, address centering, adaptive detail -------------------------
test_that("R1 the radius slider fits the place: top about 1.5 times the enclosing radius, a step that suits the size, a start that covers the place", {
  chk <- function(enc, min, max, step, value) { p <- env$radius_plan(enc); expect_equal(unlist(p[c("min", "max", "step", "value")]), c(min = min, max = max, step = step, value = value), info = enc) }
  chk(3500, 0.5, 5.5, 0.5, 3.5)           # r = 3.5 km: top = ceil(5.25 / 0.5) * 0.5
  chk(3200, 0.5, 5, 0.5, 3.5)             # the start rounds up so the circle covers the whole place
  chk(9000, 1, 14, 1, 6)                  # 9 km: start at the largest radius that still downloads every street
  chk(25000, 5, 40, 5, 5)
  chk(120000, 15, 50, 5, 15)              # never above the absolute limit
  for (bad in list(NA_real_, 100, 499, Inf)) chk(bad, 1, 5, 0.5, 3)    # no usable boundary: the general range
  for (e in c(800, 2500, 3500, 6000, 9000, 14000, 25000, 40000, 90000)) {
    p <- env$radius_plan(e); expect_lt(p$min, p$max); expect_gte(p$value, p$min); expect_lte(p$value, p$max)
    expect_equal((p$max - p$min) %% p$step, 0, tolerance = 1e-9); expect_gte(p$max, min(50, e / 1000 * 1.5) - 1e-9)
  }
})

test_that("R2 the enclosing radius is the farthest boundary point from the center, or the bounding box corner when there is no boundary", {
  b <- st_transform(sq_shape(3000), 4326)                                                   # square centered on the map center, half-side 3 km
  expect_equal(env$enclosing_radius_m(b, lon0, lat0), 3000 * sqrt(2), tolerance = 0.002)
  off <- env$enclosing_radius_m(b, lon0 + 0.02, lat0)                                       # a center off to one side is farther from the far corners
  expect_gt(off, env$enclosing_radius_m(b, lon0, lat0))
  m_lat <- 110574; m_lon <- 111320 * cos(lat0 * pi / 180)
  bb <- c(lat0 - 0.02, lat0 + 0.03, lon0 - 0.04, lon0 + 0.01)                               # south, north, west, east
  expect_equal(env$enclosing_radius_m(NULL, lon0, lat0, bb), sqrt((0.04 * m_lon)^2 + (0.03 * m_lat)^2), tolerance = 0.01)
  expect_true(is.na(env$enclosing_radius_m(NULL, lon0, lat0, NULL))); expect_true(is.na(env$enclosing_radius_m(NULL, lon0, lat0, c(NA, NA, NA, NA))))
})

test_that("R3 the level of detail follows the radius, and the all-streets switch works only up to its cap", {
  d <- env$detail_for
  expect_equal(c(d(3000), d(6000), d(6001), d(25000), d(25001), d(50000)), c("all", "all", "main", "main", "major", "major"))
  expect_equal(c(d(9000, TRUE), d(12000, TRUE), d(12001, TRUE), d(30000, TRUE), d(3000, TRUE)), c("all", "all", "main", "major", "all"))
  expect_equal(d(9000, NULL), "main"); expect_equal(d(9000, NA), "main")
  hw <- env$hw
  expect_equal(env$detail_classes("all"), hw)
  main <- env$detail_classes("main"); major <- env$detail_classes("major")
  expect_true(all(c("tertiary", "tertiary_link", "secondary", "primary", "trunk", "motorway") %in% main)); expect_false(any(c("residential", "unclassified") %in% main))
  expect_true(all(c("secondary", "secondary_link", "primary", "trunk", "motorway") %in% major)); expect_false(any(c("tertiary", "tertiary_link", "residential", "unclassified") %in% major))
  expect_equal(env$detail_classes("nonsense"), hw)
})

test_that("R4 a thinner download asks only for the classes it needs and is cached apart from the full one", {
  with_env(paste(env$overpass_mirrors, collapse = ","), {
    for (dt in c("main", "major")) {
      reset_mirrors(); body <- NULL
      mock <- mk_status(function(req) { body <<- utils::URLdecode(req$body$data$data); ok_resp() })
      f <- file.path(env$roads_cache_dir(), sprintf("roads_8.1234_9.4321_9000_%s.rds", dt)); unlink(f)
      httr2::with_mocked_responses(mock, env$fetch_roads(8.1234, 9.4321, 9000L, 32632, detail = dt))
      expect_true(file.exists(f)); expect_false(file.exists(file.path(env$roads_cache_dir(), "roads_8.1234_9.4321_9000.rds")))
      expect_match(body, "secondary", fixed = TRUE); expect_false(grepl("residential", body, fixed = TRUE)); expect_false(grepl("unclassified", body, fixed = TRUE))
      expect_equal(grepl("tertiary", body, fixed = TRUE), dt == "main")
    }
  })
  with_env(paste(env$overpass_mirrors, collapse = ","), {
    reset_mirrors(); body <- NULL
    httr2::with_mocked_responses(mk_status(function(req) { body <<- utils::URLdecode(req$body$data$data); ok_resp() }), env$fetch_roads(8.2, 9.5, 2000L, 32632))
    expect_match(body, "residential", fixed = TRUE)                                          # the default is every street
  })
})

test_that("R5 a thinned map says so in the subtitle, caption, and tiles; a full one is unchanged", {
  lines <- demo_lines_prepped(); full <- env$view_data("hierarchy", lines, 3000, ctr_m()); same <- env$view_data("hierarchy", lines, 3000, ctr_m(), NULL, "all")
  expect_identical(full, same)
  th <- env$view_data("hierarchy", lines, 3000, ctr_m(), NULL, "main")
  expect_match(th$subtitle, "not included at this size"); expect_match(th$caption, "not included at this size")
  expect_equal(th$tiles[[3]]$value, "Not shown"); expect_false(grepl("% of it is local", th$subtitle, fixed = TRUE))
  mj <- env$view_data("direction", lines, 3000, ctr_m(), NULL, "major"); expect_match(mj$subtitle, "Only highways, arterials, and major collectors"); expect_match(mj$caption, "only highways")
  for (v in c("speed", "density")) expect_match(env$view_data(v, lines, 3000, ctr_m(), NULL, "main")$subtitle, "Local streets", fixed = TRUE)
})

test_that("R6 geocode_address looks near the place, takes the first match, and returns NULL for none", {
  seen <- NULL
  r <- httr2::with_mocked_responses(function(req) { seen <<- req$url; httr2::response_json(body = list(list(display_name = "401 Davis Avenue, Elkins, West Virginia", lat = "38.9231", lon = "-79.8465"))) },
                                    env$geocode_address("401 Davis Ave, Elkins", c(38.8, 39.0, -79.9, -79.7)))
  expect_match(seen, "viewbox=-79.90000%2C39.00000%2C-79.70000%2C38.80000", fixed = TRUE); expect_match(seen, "limit=1", fixed = TRUE); expect_match(seen, "bounded=0", fixed = TRUE)
  expect_equal(c(r$lat, r$lon), c(38.9231, -79.8465)); expect_match(r$label, "^401 Davis")
  r2 <- httr2::with_mocked_responses(function(req) { seen <<- req$url; httr2::response_json(body = list()) }, env$geocode_address("nowhere"))
  expect_null(r2); expect_false(grepl("viewbox", seen, fixed = TRUE))
})

test_that("R6b geocode_address can return several candidates, and address_query adds the place name only when needed", {
  seen <- NULL
  r <- httr2::with_mocked_responses(function(req) { seen <<- req$url; httr2::response_json(body = list(list(display_name = "A", lat = "1", lon = "2"), list(display_name = "B", lat = "3", lon = "4"))) },
                                    env$geocode_address("x", NULL, limit = 5))
  expect_match(seen, "limit=5", fixed = TRUE); expect_equal(r$label, c("A", "B")); expect_equal(r$lat, c(1, 3)); expect_equal(r$lon, c(2, 4))
  expect_equal(env$address_query("401 Davis Ave", "Elkins, West Virginia"), "401 Davis Ave, Elkins, West Virginia")
  expect_equal(env$address_query("401 Davis Ave, Elkins", "Elkins, West Virginia"), "401 Davis Ave, Elkins")
})

test_that("L1 Locate shows what it found: one match, several matches, none, a failure, or a missing address", {
  hits2 <- tibble(label = c("401 Davis Avenue, Elkins, Randolph County", "401 Davis Street, Elkins"), lat = lat0 + c(0.01, 0.02), lon = lon0 + c(0.01, 0.02))
  html_for <- function(stub, addr, ...) {
    with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326), addr = stub); out <- NULL
    testServer(env$server, {
      session$setInputs(query = "Elkins", find = 1, choice = "1", addr = addr)
      session$setInputs(find_addr = 1); out <<- as.character(output$addr_status$html)
    })
    out }
  expect_match(html_for(hits2[1, ], "401 Davis Ave"), "Found: 401 Davis Avenue", fixed = TRUE)
  h <- html_for(hits2, "401 Davis"); expect_match(h, "Which address?", fixed = TRUE); expect_match(h, "401 Davis Street", fixed = TRUE)
  expect_match(html_for("none", "zzz"), "No match near", fixed = TRUE)
  expect_match(html_for(simpleError("timeout"), "zzz"), "did not answer", fixed = TRUE)
  expect_match(html_for(hits2[1, ], "  "), "Type an address first", fixed = TRUE)
  with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326), addr = "none"); stale <- NULL      # editing the text after a failed Locate retires the old message
  testServer(env$server, { session$setInputs(query = "Elkins", find = 1, choice = "1", addr = "zzz"); session$setInputs(find_addr = 1); session$setInputs(addr = "zzz street"); stale <<- as.character(output$addr_status$html) })
  expect_match(stale, "address changed", fixed = TRUE); expect_false(grepl("No match", stale, fixed = TRUE))
})

test_that("L2 Draw map uses the address that was located, including the candidate the user picked", {
  hits2 <- tibble(label = c("401 Davis Avenue, Elkins", "401 Davis Street, Elkins"), lat = lat0 + c(0.01, 0.02), lon = lon0 + c(0.01, 0.02)); out <- NULL
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326), addr = hits2)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", addr = "401 Davis", use_addr = TRUE)
    session$setInputs(find_addr = 1); invisible(output$addr_status$html); session$setInputs(addr_choice = "2")
    session$setInputs(radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, draw = 1)
    g <- geo(); out <<- c(g$lon, g$lat)
  })
  expect_equal(out, c(lon0 + 0.02, lat0 + 0.02)); expect_length(calls$addr, 1)              # the Locate lookup was reused: no second request
})

test_that("L2b a tampered or out-of-range candidate choice falls back to the first match", {
  hits2 <- tibble(label = c("401 Davis Avenue, Elkins", "401 Davis Street, Elkins"), lat = lat0 + c(0.01, 0.02), lon = lon0 + c(0.01, 0.02)); out <- NULL
  with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326), addr = hits2)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", addr = "401 Davis", use_addr = TRUE, find_addr = 1)
    invisible(output$addr_status$html); session$setInputs(addr_choice = "7")
    session$setInputs(radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, draw = 1)
    out <<- unname(c(geo()$lon, geo()$lat))
  })
  expect_equal(out, c(lon0 + 0.01, lat0 + 0.01))
})

test_that("L3 the Locate button and Enter key are wired up in the page", {
  ui <- as.character(env$ui(NULL)); expect_match(ui, 'id="find_addr"', fixed = TRUE); expect_match(ui, "Locate", fixed = TRUE)
  src <- paste(readLines(Sys.getenv("APP_FILE"), warn = FALSE), collapse = "\n"); expect_match(src, "keyup', '#addr'", fixed = TRUE)
})

test_that("R7 an address becomes the map center: the streets are fetched around it, it is marked and named, and the city outline still applies", {
  hit <- tibble(label = "401 Davis Avenue, Elkins, Randolph County, West Virginia", lat = lat0 + 0.01, lon = lon0 + 0.01)
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326), addr = hit)
  out <- NULL
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "dim", locals = FALSE,
                      use_addr = TRUE, addr = "401 Davis Ave", draw = 1)
    g <- geo(); out <<- list(lon = g$lon, lat = g$lat, txt = g$center_txt, inside = g$inside, vd = vdata())
  })
  expect_equal(c(out$lon, out$lat), c(lon0 + 0.01, lat0 + 0.01)); expect_equal(c(calls$fetch[[1]]$lon, calls$fetch[[1]]$lat), c(lon0 + 0.01, lat0 + 0.01))
  expect_equal(out$txt, "401 Davis Avenue Elkins"); expect_false(is.null(out$inside))
  expect_match(calls$addr[[1]]$q, "401 Davis Ave, Elkins", fixed = TRUE); expect_match(out$vd$subtitle, "of 401 Davis Avenue Elkins", fixed = TRUE)
  p <- env$make_map(demo_lines_prepped(), "Elkins", lon0, lat0, 3000, crs0, center_txt = "401 Davis Avenue Elkins")
  expect_match(as.character(p$patches$annotation$caption), "the small circle marks 401 Davis Avenue Elkins", fixed = TRUE)
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomSf") && is.null(l$mapping$colour) && identical(l$aes_params$shape, 21), TRUE)))
  p0 <- env$make_map(demo_lines_prepped(), "Elkins", lon0, lat0, 3000, crs0)
  expect_false(any(vapply(p0$layers, function(l) identical(l$aes_params$shape, 21), TRUE))); expect_false(grepl("small circle", as.character(p0$patches$annotation$caption), fixed = TRUE))
})

test_that("R8 the address is ignored with 'City limits only', when the switch is off, or when blank; failures give plain messages", {
  hit <- tibble(label = "Somewhere, Elkins", lat = lat0 + 0.01, lon = lon0)
  run <- function(stub, ...) {
    calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326), addr = stub); res <- NULL
    extra <- list(...)
    testServer(env$server, {
      session$setInputs(query = "Elkins", find = 1)
      do.call(session$setInputs, c(list(choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", locals = FALSE, draw = 1), extra))
      res <<- tryCatch(geo(), error = function(e) e)
    })
    list(calls = calls, res = res)
  }
  r <- run(hit, area = "city", use_addr = TRUE, addr = "x"); expect_length(r$calls$addr, 0); expect_equal(unname(r$res$lon), lon0, tolerance = 0.05)
  r <- run(hit, area = "dim", use_addr = FALSE, addr = "x"); expect_length(r$calls$addr, 0); expect_equal(unname(r$res$lon), lon0)
  r <- run(hit, area = "dim", use_addr = TRUE, addr = "   "); expect_length(r$calls$addr, 0); expect_equal(unname(r$res$lon), lon0)
  r <- run("none", area = "circle", use_addr = TRUE, addr = "zzz"); expect_s3_class(r$res, "shiny.silent.error"); expect_match(conditionMessage(r$res), "could not find that address near")
  r <- run(simpleError("timeout"), area = "circle", use_addr = TRUE, addr = "zzz"); expect_s3_class(r$res, "shiny.silent.error"); expect_match(conditionMessage(r$res), "did not answer")
  r <- run(hit, area = "circle", use_addr = TRUE, addr = strrep("a", 300)); expect_s3_class(r$res, "shiny.silent.error"); expect_length(r$calls$addr, 0)
  expect_length(r$calls$fetch, 0)
})

test_that("R9 choosing a place resizes the slider to that place, and a big area is downloaded thinned", {
  big <- st_transform(sq_shape(9000), 4326); upd <- list()
  testthat::local_mocked_bindings(updateSliderInput = function(session, inputId, label = NULL, value = NULL, min = NULL, max = NULL, step = NULL, ...) { upd[[length(upd) + 1]] <<- list(id = inputId, value = value, min = min, max = max, step = step); invisible() }, .package = "shiny")
  calls <- with_stubs(NULL, boundary = big)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1"); session$flushReact()
    session$setInputs(choice = "2"); session$flushReact()
    session$setInputs(choice = "1", radius = 10, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, all_streets = FALSE, draw = 1)
    invisible(geo())
  })
  first <- Filter(function(u) u$id == "radius", upd)[[1]]
  expect_equal(c(first$min, first$max, first$step, first$value), c(2.5, 20, 2.5, 5))        # the square's far corner is 12.7 km out: top = 1.5 x that, rounded up to the step
  fallback <- Filter(function(u) u$id == "radius" && u$max == 5, upd)
  expect_length(fallback, 1); expect_equal(c(fallback[[1]]$min, fallback[[1]]$step, fallback[[1]]$value), c(1, 0.5, 3))   # "Other, Place" has no boundary: the general range
  expect_equal(calls$fetch[[1]]$detail, "main"); expect_equal(calls$fetch[[1]]$radius_m, 10000)
  calls <- with_stubs(NULL)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1)
    session$setInputs(choice = "1", radius = 10, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, all_streets = TRUE, draw = 1)
    invisible(geo())
  })
  expect_equal(calls$fetch[[1]]$detail, "all")                                               # the switch includes every street up to its cap
})

test_that("R10 the new palettes: direction axes and density steps pass the checks they were designed against", {
  hexL <- function(h) { v <- grDevices::convertColor(t(grDevices::col2rgb(h) / 255), "sRGB", "Lab")[, 1]; v }
  for (st in names(env$styles)) {
    pal <- env$styles[[st]]; expect_length(pal$dir, 8); expect_length(pal$dens, 6); expect_equal(anyDuplicated(pal$dens), 0)
  }
  # Density steps are one hue and monotone in lightness (light to dark on paper, dark to bright on dusk); mono is gray.
  expect_true(all(diff(hexL(env$styles$classic$dens)) < 0)); expect_true(all(diff(hexL(env$styles$dusk$dens)) > 0)); expect_true(all(diff(hexL(env$styles$mono$dens)) < 0))
  # The four axis colors (bins 1, 3, 5, 7) are pairwise far apart, and the ring of eight turns in gentle steps.
  oklab <- function(h) { c3 <- as.numeric(grDevices::col2rgb(h)) / 255; lin <- ifelse(c3 <= 0.04045, c3 / 12.92, ((c3 + 0.055) / 1.055)^2.4)
    lms <- c(0.4122214708 * lin[1] + 0.5363325363 * lin[2] + 0.0514459929 * lin[3], 0.2119034982 * lin[1] + 0.6806995451 * lin[2] + 0.1073969566 * lin[3],
             0.0883024619 * lin[1] + 0.2817188376 * lin[2] + 0.6299787005 * lin[3])^(1 / 3)
    100 * c(0.2104542553 * lms[1] + 0.7936177850 * lms[2] - 0.0040720468 * lms[3], 1.9779984951 * lms[1] - 2.4285922050 * lms[2] + 0.4505937099 * lms[3],
            0.0259040371 * lms[1] + 0.7827717662 * lms[2] - 0.8086757660 * lms[3]) }
  lab <- oklab                                                                              # distances in OKLab x 100, the unit the color checks use
  de <- function(a, b) sqrt(sum((lab(a) - lab(b))^2))
  for (st in c("classic", "dusk")) { d <- env$styles[[st]]$dir; ax <- d[c(1, 3, 5, 7)]
    for (i in 1:3) for (j in (i + 1):4) expect_gt(de(ax[i], ax[j]), 15, label = paste(st, i, j))
    # The ring turns smoothly: every neighboring pair (including the last back to the first) is a visible but gentle step.
    for (k in 1:8) { step <- de(d[k], d[if (k == 8) 1 else k + 1]); expect_gt(step, 5, label = paste(st, "step", k)); expect_lt(step, 25, label = paste(st, "step", k)) } }
})


# ---- miles toggle ----------------------------------------------------------------------------------------------------
test_that("U1 unit helpers: kilometers by default, miles on request, and the setting never leaks out of a call", {
  expect_equal(env$fmt_km(16.09344), "16 km"); expect_equal(env$with_units("mi", env$fmt_km(16.09344)), "10 mi")
  expect_equal(env$with_units("mi", env$radius_txt(3000)), 1.9); expect_equal(env$radius_txt(3000), 3); expect_equal(env$with_units("mi", env$radius_txt(1609.344 * 2)), 2)
  expect_equal(env$as_unit(NULL), "km"); expect_equal(env$as_unit("furlongs"), "km")
  env$with_units("mi", NULL); expect_equal(env$ulab(), "km")
  try(env$with_units("mi", stop("boom")), silent = TRUE); expect_equal(env$ulab(), "km")
})

test_that("U2 every map type speaks miles when asked: subtitle, tiles, legend, and caption carry no leftover kilometers", {
  lines <- demo_lines_prepped(); tot_km <- sum(lines$km)
  for (v in c("hierarchy", "speed", "direction", "density")) {
    vd <- env$view_data(v, lines, 3000, ctr_m(), NULL, "all", NULL, "mi")
    txt <- paste(vd$subtitle, vd$caption, vd$scope, paste(vapply(vd$tiles, function(t) paste(t$value, t$note), ""), collapse = " "))
    expect_false(grepl("\\bkm\\b|kilometer", txt), label = paste(v, "has no km"))
    expect_match(vd$scope, "within 1.9 mi of the center of", fixed = TRUE)
  }
  vd <- env$view_data("hierarchy", lines, 3000, ctr_m(), NULL, "all", NULL, "mi")
  expect_match(vd$subtitle, paste0("^", round(tot_km / 1.609344), " mi of street"))
  expect_equal(vd$tiles[[1]]$value, paste0(format(round(tot_km / 1.609344), big.mark = ","), " mi"))
  km <- env$view_data("hierarchy", lines, 3000, ctr_m()); expect_match(km$subtitle, paste0("^", round(tot_km), " km of street"))   # default unchanged
})

test_that("U3 density classes in miles are their own round numbers and agree with the kilometer map hexagon by hexagon", {
  lines <- demo_lines_prepped()
  k <- env$view_data("density", lines, 3000, ctr_m()); m <- env$view_data("density", lines, 3000, ctr_m(), NULL, "all", NULL, "mi")
  expect_equal(m$levels, c("under 3", "3–8", "8–16", "16–24", "24–32", "32 or more")); expect_equal(k$levels, env$dens_labels)
  expect_equal(m$cells$dens, k$cells$dens * 1.609344, tolerance = 1e-9)
  expect_equal(as.character(m$cells$cls), as.character(cut(m$cells$dens, c(0, 3, 8, 16, 24, 32, Inf), labels = m$levels, right = FALSE, include.lowest = TRUE)))
  expect_match(m$tiles[[2]]$value, " mi/mi²$"); expect_match(k$tiles[[2]]$value, " km/km²$")
})

test_that("U4 the drawn figures use the unit: legend labels and density legend title", {
  lines <- demo_lines_prepped()
  p <- env$make_map(lines, "Test", lon0, lat0, 3000, crs0, view = "hierarchy", units = "mi")
  expect_true(any(grepl(" mi$", unlist(lapply(p[[1]]$scales$scales, function(s) as.character(s$labels)))))); expect_equal(env$ulab(), "km")
  d <- env$make_map(lines, "Test", lon0, lat0, 3000, crs0, view = "density", units = "mi")
  expect_equal(d[[1]]$scales$get_scales("fill")$name, "mi of street per mi²")
})

test_that("U5 the radius slider is converted to miles with round steps, and switching units keeps the same distance", {
  pl <- env$radius_plan_units(list(min = 1, max = 5, step = 0.5, value = 3), "mi")
  expect_equal(unlist(pl), c(min = 0.75, max = 3, step = 0.25, value = 1.75)); expect_identical(env$radius_plan_units(list(min = 1, max = 5, step = 0.5, value = 3), "km")$max, 5)
  big <- env$radius_plan_units(list(min = 5, max = 50, step = 5, value = 10), "mi"); expect_equal(big$step, 2.5); expect_lte(big$max * 1.609344, 50); expect_gte(big$min * 1.609344, 5)
  upd <- list()
  testthat::local_mocked_bindings(updateSliderInput = function(session, inputId, label = NULL, value = NULL, min = NULL, max = NULL, step = NULL, ...) { upd[[length(upd) + 1]] <<- list(id = inputId, value = value, min = min, max = max, step = step); invisible() }, .package = "shiny")
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); rad <- NULL
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, units = "km"); session$setInputs(choice = "1"); session$flushReact()
    session$setInputs(radius = 4); session$setInputs(units = "mi"); session$flushReact()
    session$setInputs(radius = 1.5, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, draw = 1); rad <<- geo()$radius_m
  })
  mi <- Filter(function(u) u$id == "radius" && !is.null(u$step) && u$step %in% c(0.25, 0.5, 1, 2.5) && u$max < 40, upd); expect_gte(length(mi), 1)
  expect_equal(tail(mi, 1)[[1]]$value, round(4 / 1.609344 / 0.25) * 0.25)                    # 4 km is 2.5 mi, kept across the switch
  expect_equal(rad, round(1.5 * 1609.344))                                                    # a slider value in miles is converted for the download
})


# ---- network tiles ---------------------------------------------------------------------------------------------------
mk_lines <- function(...) { l <- list(...); st_sf(id = seq_along(l), geometry = st_sfc(lapply(l, function(m) st_linestring(matrix(m, ncol = 2, byrow = TRUE))), crs = 32617)) }
net_of <- function(ln, r = 1000) { cl <- st_buffer(st_sfc(st_point(c(0, 0)), crs = 32617), r); env$network_stats(ln, env$segments_of(ln), cl, as.numeric(st_area(cl)) / 1e6, r * 0.03) }

test_that("N6 intersections and dead ends are counted from geometry: a crossing grid, a T junction, and streets cut off by the map edge", {
  grid <- mk_lines(c(-200, 0, 200, 0), c(-200, 100, 200, 100), c(0, -200, 0, 200), c(100, -200, 100, 200))
  n <- net_of(grid); expect_equal(n$n_int, 4); expect_equal(n$n_dead, 8); expect_equal(n$dead_share, 8 / 12); expect_equal(n$int_per_km2, 4 / pi, tolerance = 1e-3)         # the circle is a many-sided polygon, so its area is a hair under pi
  tj <- mk_lines(c(-200, 0, 200, 0), c(0, 0, 0, 300)); n2 <- net_of(tj); expect_equal(n2$n_int, 1); expect_equal(n2$n_dead, 3)         # a street ending on another one is a junction
  edge <- mk_lines(c(-1000, 0, 1000, 0)); n3 <- net_of(edge); expect_equal(n3$n_int, 0); expect_equal(n3$n_dead, 0); expect_true(is.na(n3$dead_share))   # cut by the ring: not dead ends
  near <- mk_lines(c(-980, 0, 980, 0)); n4 <- net_of(near); expect_equal(n4$n_dead, 0)                                                   # ends 20 m inside a 1 km ring (edge band is 30 m) are the map's cut, not dead ends
  inner <- mk_lines(c(-900, 0, 900, 0)); expect_equal(net_of(inner)$n_dead, 2)                                                          # ends well inside the ring are real dead ends
  loop <- mk_lines(c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0)); expect_equal(net_of(loop)$n_dead, 0)                                       # a closed loop has no ends
})

test_that("N9 a bridge or tunnel crossing a street is not a junction, but a bridge that meets the street at its end still is", {
  flat <- mk_lines(c(-200, 0, 200, 0)); cross <- mk_lines(c(-200, 0, 200, 0), c(0, -200, 0, 200))
  expect_equal(net_of(cross)$n_int, 1)                                                                     # untagged: a crossing is a junction
  for (tag in c("bridge", "tunnel")) {
    x <- cross; x[[tag]] <- c(NA, "yes"); n <- net_of(x); expect_equal(n$n_int, 0, label = tag); expect_equal(n$n_dead, 4, label = tag); expect_equal(n$n_grade, 1)
  }
  x <- cross; x$bridge <- c(NA, "no"); expect_equal(net_of(x)$n_int, 1)                                    # "no" is not a bridge
  x <- mk_lines(c(-200, 0, 0, 0), c(0, 0, 200, 0), c(0, 0, 0, 200)); x$bridge <- c(NA, NA, "yes"); expect_equal(net_of(x)$n_int, 1)   # a bridge that starts at a street is joined to it
  x <- cross; x$bridge <- c("viaduct", NA); expect_equal(net_of(x)$n_int, 0)
  expect_equal(env$is_grade_separated(x), c(TRUE, FALSE)); expect_equal(env$is_grade_separated(flat), FALSE)
})

test_that("N10 the one-way share counts length tagged one-way in either direction, and says n/a when nothing is known", {
  l <- mk_lines(c(0, 0, 100, 0), c(0, 10, 300, 10), c(0, 20, 100, 20), c(0, 30, 100, 30)); l$km <- c(0.1, 0.3, 0.1, 0.1); l$oneway <- c("yes", "-1", "no", NA)
  expect_equal(env$oneway_share(l), 0.4 / 0.6); l$oneway <- NULL; expect_true(is.na(env$oneway_share(l)))
  l$oneway <- c("reverse", "1", "true", "alternating"); expect_equal(env$oneway_share(l), 0.5 / 0.6)
  l$oneway <- c("yes", NA, NA, NA); n <- list(int_per_km2 = 1, n_int = 1, n_dead = 1, dead_share = .5, order = .5, oneway_share = env$oneway_share(l), n_grade = 2)
  tl <- env$net_tiles(n); expect_equal(tl[[3]]$value, "17%"); expect_match(tl[[3]]$note, "2 bridge or tunnel pieces"); expect_match(env$net_tiles(modifyList(n, list(n_grade = 0)))[[3]]$note, "as tagged$")
  pl <- env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0); expect_true(all(c("bridge", "tunnel", "oneway") %in% names(pl)))       # the tags survive the preparation step, empty when the data has none
})

test_that("N7 orientation order is 1 for a perfect grid, near 0 for streets in every direction, and follows its formula in between", {
  sg <- function(deg, len = 1) tibble(piece = seq_along(deg), len = rep_len(len, length(deg)), mx = 0, my = 0, deg = deg)
  expect_equal(env$orientation_order(sg(c(0, 90, 0, 90))), 1); expect_equal(env$orientation_order(sg(c(2, 88, -3 %% 180, 93))), 1)      # within the same ten-degree bins
  expect_lt(env$orientation_order(sg(seq(0, 175, by = 10) + 0.1)), 0.001)
  expect_equal(env$orientation_order(sg(rep(0, 5))), 1 - (log(2) / (log(18) - log(2)))^2)
  expect_equal(env$orientation_order(sg(c(0, 90), c(3, 1))[1:2, ]), 1 - ((-(0.75 * log(0.75) + 0.25 * log(0.25)) - log(2)) / (log(18) - log(2)))^2)
  expect_true(is.na(env$orientation_order(sg(numeric(0)))))
})

test_that("N8 the network tiles read well, switch to miles, and are skipped when there is nothing to show or the area is too large", {
  grid <- mk_lines(c(-200, 0, 200, 0), c(-200, 100, 200, 100), c(0, -200, 0, 200), c(100, -200, 100, 200)); n <- net_of(grid)
  t <- env$net_tiles(n); expect_length(t, 4); expect_equal(t[[1]]$value, "1"); expect_match(t[[1]]$label, "square km"); expect_equal(t[[2]]$value, "67%"); expect_equal(t[[3]]$value, "n/a"); expect_match(t[[4]]$value, "^[01]\\.[0-9]{2}$")
  tm <- env$with_units("mi", env$net_tiles(n)); expect_equal(tm[[1]]$value, "3"); expect_match(tm[[1]]$label, "square mile")             # 1.27 per km2 is 3.30 per mi2
  expect_null(env$net_tiles(NULL)); expect_null(env$view_data("hierarchy", demo_lines_prepped(), 3000, ctr_m())$net_tiles)
  expect_length(env$view_data("hierarchy", demo_lines_prepped(), 3000, ctr_m(), NULL, "all", NULL, "km", n)$net_tiles, 4)
  old <- env$max_net_lines; env$max_net_lines <- 2; expect_null(net_of(grid)); env$max_net_lines <- old
})

test_that("N9 the server computes the network measures once per drawn map and shows them in the stats column", {
  with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); out <- NULL; html <- NULL
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, draw = 1)
    out <<- geo()$net; html <<- as.character(output$stats$html)
  })
  expect_false(is.null(out)); expect_gt(out$n_int, 0); expect_true(out$order >= 0 && out$order <= 1)
  expect_match(html, "Street network", fixed = TRUE); expect_match(html, "Intersections per square km", fixed = TRUE); expect_match(html, "Dead ends", fixed = TRUE); expect_match(html, "Orderliness", fixed = TRUE)
})


# ---- share links and reset -------------------------------------------------------------------------------------------
test_that("SH1 a share link carries the settings and reads back the same; bad values are dropped", {
  inp <- list(query = "  Saint Paul & Minneapolis, MN ", radius = 2.5, units = "mi", view = "direction", style = "dusk", area = "circle", locals = FALSE, labels = TRUE, all_streets = TRUE, use_addr = TRUE, addr = "401 Davis Ave")
  pr <- env$share_params(inp, 2L); url <- env$share_url(pr, "https://example.org/app")
  expect_match(url, "^https://example.org/app\\?q=Saint%20Paul%20%26%20Minneapolis%2C%20MN&c=2&r=2.5&u=mi&v=direction&s=dusk&a=circle&l=0&lb=1&as=1&ua=1&ad=401%20Davis%20Ave&go=1$")
  back <- env$parse_share(sub("^[^?]*", "", url))
  expect_equal(back$q, "Saint Paul & Minneapolis, MN"); expect_equal(back$c, 2L); expect_equal(back$r, 2.5); expect_equal(back$u, "mi"); expect_equal(back$v, "direction"); expect_equal(back$s, "dusk")
  expect_equal(back$a, "circle"); expect_false(back$l); expect_true(back$lb); expect_true(back$as); expect_true(back$ua); expect_equal(back$ad, "401 Davis Ave"); expect_true(back$go)
  expect_null(env$share_params(list(query = "   "), 1L)); expect_equal(env$share_url(NULL), "")
  bad <- env$parse_share("?q=Elkins&v=bogus&s=neon&a=x&u=furlongs&r=-4&c=abc&l=yes&as=2&ad=&go=0")
  expect_equal(names(bad), c("q", "go")); expect_false(bad$go)
  expect_null(env$parse_share("?v=direction")); expect_null(env$parse_share("?q=")); expect_null(env$parse_share(strrep("?q=a", 1) |> paste0(strrep("a", 300))))
  expect_equal(env$parse_share("?q=Elkins&r=9999")$q, "Elkins"); expect_null(env$parse_share("?q=Elkins&r=9999")$r)
  expect_equal(env$parse_share("?q=Elkins&r=500")$r, 500); expect_null(env$parse_share("?q=Elkins&r=501")$r); expect_null(env$parse_share("?q=Elkins&r=0.05")$r); expect_equal(env$parse_share("?q=Elkins&r=0.1")$r, 0.1)   # the radius limits are inclusive
  expect_null(env$parse_share("?q=Elkins&u=yd")$u); expect_equal(env$parse_share("?q=Elkins&u=km")$u, "km"); expect_equal(env$parse_share("?q=Elkins&u=mi")$u, "mi")   # only the two units we draw in
  expect_equal(env$parse_share("?q=%3Cscript%3Ealert(1)%3C%2Fscript%3E")$q, "<script>alert(1)</script>")                  # kept as plain text; it only ever goes into a text box
})

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

test_that("SH2 opening a link fills the controls, finds the saved match, uses the saved radius for the first draw, then hands the slider back", {
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); ups <- list()
  rec <- function(nm) function(session, inputId, ...) { a <- list(...); ups[[length(ups) + 1]] <<- c(list(fn = nm, id = inputId), a); invisible() }
  testthat::local_mocked_bindings(updateRadioButtons = rec("radio"), updateTextInput = rec("text"), updateSelectInput = rec("select"), updateSliderInput = rec("slider"), .package = "shiny")
  testthat::local_mocked_bindings(update_switch = function(id, value = NULL, ...) { ups[[length(ups) + 1]] <<- list(fn = "switch", id = id, value = value); invisible() }, .package = "bslib")
  env$parse_share <- function(qs) list(q = "Elkins", c = 1L, r = 1.5, u = "mi", v = "direction", s = "dusk", a = "circle", l = FALSE, lb = TRUE, as = FALSE, ua = FALSE, go = TRUE)
  withr::defer(env$parse_share <- orig_parse)
  got <- list()
  testServer(env$server, {
    session$flushReact()
    expect_length(calls$geo, 1); expect_equal(calls$geo, "Elkins")                                    # the place was looked up without anyone pressing Find
    session$setInputs(units = "mi", view = "direction", style = "dusk", area = "circle", locals = FALSE, labels = TRUE, all_streets = FALSE, radius = 3)   # what the browser reports back (the text box has not reported yet)
    session$flushReact()
    session$setInputs(choice = "1"); session$flushReact()
    g <- geo(); got$radius <<- g$radius_m; got$n <<- length(calls$fetch)
    session$setInputs(radius = 1, draw = 1); session$flushReact(); g2 <- geo(); got$radius2 <<- g2$radius_m
  })
  expect_true(any(vapply(ups, function(u) u$fn == "radio" && u$id == "view" && identical(u$selected, "direction"), TRUE)))
  expect_true(any(vapply(ups, function(u) u$fn == "radio" && u$id == "units" && identical(u$selected, "mi"), TRUE)))
  expect_true(any(vapply(ups, function(u) u$fn == "switch" && u$id == "locals" && identical(u$value, FALSE), TRUE)))
  expect_equal(got$radius, round(1.5 * 1609.344)); expect_equal(got$n, 1)                              # drawn once, at the saved 1.5 miles, though the slider still said 3
  expect_equal(got$radius2, round(1 * 1609.344))                                                       # a later click on Draw map follows the slider
})

test_that("SH2b a link to the second match asks for that match first, and does not draw until the browser reports it", {
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); ups <- list()
  testthat::local_mocked_bindings(updateRadioButtons = function(...) invisible(), updateTextInput = function(...) invisible(), updateSliderInput = function(...) invisible(),
                                  updateSelectInput = function(session, inputId, ..., selected = NULL) { ups[[length(ups) + 1]] <<- list(id = inputId, selected = selected); invisible() }, .package = "shiny")
  testthat::local_mocked_bindings(update_switch = function(...) invisible(), .package = "bslib")
  env$parse_share <- function(qs) list(q = "Elkins", c = 2L, r = 1.5, u = "km", go = TRUE); withr::defer(env$parse_share <- orig_parse)
  testServer(env$server, { session$setInputs(units = "km", radius = 3); session$flushReact(); expect_length(calls$fetch, 0) })
  expect_equal(length(ups), 1); expect_equal(ups[[1]]$id, "choice"); expect_equal(ups[[1]]$selected, 2L)
})

test_that("SH3 an invalid or missing link does nothing; and a link without go fills the form but does not draw", {
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326))
  env$parse_share <- function(qs) NULL; withr::defer(env$parse_share <- orig_parse)
  testServer(env$server, { session$flushReact(); expect_length(calls$geo, 0) })
  env$parse_share <- function(qs) list(q = "Elkins", c = 1L, r = 2)
  testthat::local_mocked_bindings(updateRadioButtons = function(...) invisible(), updateTextInput = function(...) invisible(), updateSelectInput = function(...) invisible(), updateSliderInput = function(...) invisible(), .package = "shiny")
  testServer(env$server, { session$setInputs(query = "Elkins", units = "km", radius = 3); session$flushReact(); expect_equal(calls$geo, "Elkins"); expect_length(calls$fetch, 0) })
})

test_that("SH4 the share box shows a link that matches the current settings, and Reset restores the starting options", {
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); ups <- list(); html <- NULL
  rec <- function(nm) function(session, inputId, ...) { a <- list(...); ups[[length(ups) + 1]] <<- c(list(fn = nm, id = inputId), a); invisible() }
  testthat::local_mocked_bindings(updateRadioButtons = rec("radio"), updateTextInput = rec("text"), updateSelectInput = rec("select"), updateSliderInput = rec("slider"), .package = "shiny")
  testthat::local_mocked_bindings(update_switch = function(id, value = NULL, ...) { ups[[length(ups) + 1]] <<- list(fn = "switch", id = id, value = value); invisible() }, .package = "bslib")
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", radius = 4, units = "km", view = "speed", style = "mono", area = "dim", locals = TRUE, labels = FALSE, all_streets = FALSE, use_addr = FALSE)
    html <<- as.character(output$share_box$html)
    n0 <- length(ups); session$setInputs(reset = 1); session$flushReact(); after <<- ups[(n0 + 1):length(ups)]
  })
  expect_match(html, "q=Elkins&amp;c=1&amp;r=4&amp;u=km&amp;v=speed&amp;s=mono&amp;a=dim&amp;l=1&amp;lb=0&amp;as=0&amp;ua=0&amp;go=1", fixed = TRUE)
  expect_match(html, 'aria-label="Link to this map"', fixed = TRUE); expect_match(html, "streetMapsCopyLink", fixed = TRUE)
  want <- list(view = "hierarchy", area = "dim", style = "classic", units = "km")
  for (k in names(want)) expect_true(any(vapply(after, function(u) u$fn == "radio" && u$id == k && identical(u$selected, want[[k]]), TRUE)), info = k)
  expect_true(any(vapply(after, function(u) u$fn == "switch" && u$id == "labels" && isTRUE(u$value), TRUE)))
  for (k in c("locals", "all_streets", "use_addr", "inspect")) expect_true(any(vapply(after, function(u) u$fn == "switch" && u$id == k && identical(u$value, FALSE), TRUE)), info = k)
  v0 <- Filter(function(u) u$fn == "slider", ups)[[1]]$value                                                                         # the value the place gave the slider when it was found
  expect_true(any(vapply(after, function(u) u$fn == "slider" && u$id == "radius" && identical(u$value, v0), TRUE)))                 # reset returns to it, in kilometers
  expect_true(any(vapply(after, function(u) u$fn == "text" && u$id == "addr" && identical(u$value, ""), TRUE)))
})

test_that("SH5 the page has the Reset and Copy-link controls", {
  ui <- as.character(env$ui(NULL)); expect_match(ui, 'id="reset"', fixed = TRUE); expect_match(ui, "Reset options", fixed = TRUE); expect_match(ui, 'id="share_box"', fixed = TRUE)
  src <- paste(readLines(Sys.getenv("APP_FILE"), warn = FALSE), collapse = "\n"); expect_match(src, "function streetMapsCopyLink", fixed = TRUE)
})


# ---- comparison ------------------------------------------------------------------------------------------------------
cmp_g <- function() list(lines = demo_lines_prepped(), place = "Elkins, West Virginia", lon = lon0, lat = lat0, radius_m = 3000L, crs_m = crs0, shape = NULL, center_txt = NULL)
# Call right after with_stubs(): the second place is found next to the first, so the fixture streets fall inside its circle.
cmp_geo <- function(calls, label = "Morgantown, Monongalia County, West Virginia, United States") env$geocode <- function(q) { calls$geo <- c(calls$geo, q); tibble(label = label, lat = lat0 + 0.002, lon = lon0 + 0.002) }

test_that("C9 hexagon widths read in feet when the map is in miles", {
  expect_equal(env$with_units("mi", env$short_len(472)), "1,550 ft"); expect_equal(env$short_len(472), "472 m")
  vd <- env$view_data("density", demo_lines_prepped(), 3000, ctr_m(), NULL, "all", NULL, "mi"); expect_match(vd$subtitle, "about 1,550 ft wide"); expect_false(grepl(" m ", paste(vd$subtitle, vd$caption)))
})

test_that("CP1 compare_build draws two places at the same radius, type, and unit, and gives matching rows for each", {
  calls <- with_stubs(NULL); cmp_geo(calls); r <- env$compare_build(cmp_g(), "Morgantown", "direction", "dusk", "mi")
  expect_null(r$error); expect_equal(calls$geo, "Morgantown"); expect_length(calls$fetch, 1)                       # the first place reuses its drawn streets: only the second is downloaded
  expect_equal(calls$fetch[[1]]$radius_m, 3000L); expect_equal(calls$fetch[[1]]$lon, lon0 + 0.002)
  expect_equal(r$a$place, "Elkins, West Virginia"); expect_match(r$b$place, "^Morgantown"); expect_equal(r$unit, "mi"); expect_equal(r$radius_txt, 1.9)
  expect_s3_class(r$fig, "patchwork"); expect_equal(length(r$a$rows), length(r$b$rows)); expect_equal(vapply(r$a$rows, `[[`, "", "label"), vapply(r$b$rows, `[[`, "", "label"))
  expect_match(r$a$rows[[1]]$value, " mi$"); expect_match(r$a$rows[[5]]$label, "square mile"); expect_match(r$a$rows[[7]]$label, "One-way"); expect_match(r$a$rows[[7]]$value, "%$|n/a"); expect_match(r$a$rows[[8]]$value, "^[01]\\.[0-9]{2}$")
  f <- tempfile(fileext = ".png"); env$draw_figure(r$fig, f, "png", "dusk", preview = TRUE, size = env$compare_size); expect_true(file.size(f) > 20000)
  dm <- dim(png::readPNG(f)); expect_equal(dm[1:2], c(2500, 4000))                                                    # 16 x 10 inches at 250 dpi
})

test_that("CP2 a city-limits map is compared as a plain circle: its streets are downloaded again without the boundary", {
  calls <- with_stubs(NULL); cmp_geo(calls); g <- cmp_g(); g$shape <- sq_shape(1500)
  r <- env$compare_build(g, "Morgantown", "hierarchy", "classic", "km"); expect_null(r$error); expect_length(calls$fetch, 2); expect_null(calls$fetch[[1]]$shape)
})

test_that("CP3 comparison problems are reported in plain words and never crash", {
  calls <- with_stubs(NULL); g <- cmp_g()
  expect_match(env$compare_build(g, "   ")$error, "Type the second place"); expect_match(env$compare_build(g, strrep("a", 300))$error, "under")
  env$geocode <- function(q) stop("timeout"); expect_match(env$compare_build(g, "x")$error, "did not answer")
  env$geocode <- function(q) tibble(label = character(), lat = numeric(), lon = numeric()); expect_match(env$compare_build(g, "x")$error, "No place found")
  env$geocode <- function(q) tibble(label = "Pole", lat = 89, lon = 0); expect_match(env$compare_build(g, "x")$error, "poles")
  env$geocode <- function(q) tibble(label = "Far, Place", lat = 10, lon = 10); env$fetch_roads <- function(...) fixture_roads()
  expect_match(env$compare_build(g, "x")$error, "no mapped streets for the second place")                              # fixture streets lie nowhere near that place
  env$geocode <- function(q) tibble(label = "B, Place", lat = lat0, lon = lon0); env$fetch_roads <- function(...) stop("down")
  expect_match(env$compare_build(g, "x")$error, "did not answer for the second place")
})

test_that("CP4 the Compare button needs a drawn map, shows a window with both places and the table, and the download hands back the picture", {
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); shown <- NULL; notes <- character(); dl <- NULL
  base_geo <- env$geocode; env$geocode <- function(q) if (q == "Morgantown") tibble(label = "Morgantown, Monongalia County, West Virginia", lat = lat0 + 0.002, lon = lon0 + 0.002) else base_geo(q)
  testthat::local_mocked_bindings(showModal = function(ui, ...) { shown <<- as.character(ui); invisible() }, showNotification = function(ui, ...) { notes <<- c(notes, as.character(ui)); invisible() }, .package = "shiny")
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", cmp_query = "Morgantown", cmp_go = 1); session$flushReact()
    expect_match(notes[1], "Draw a map first"); expect_null(shown)
    session$setInputs(radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, units = "km", draw = 1, cmp_go = 2); session$flushReact()
    dlp <- output$cmp_dl; dl <<- c(file.exists(dlp), file.exists(dlp) && file.size(dlp) > 20000, grepl("^compare_elkins.*_morgantown.*\\.png$", basename(dlp)))
  })
  expect_match(shown, "Elkins, West Virginia and Morgantown", fixed = TRUE); expect_match(shown, "data:image/png;base64,", fixed = TRUE); expect_match(shown, "Intersections per square km", fixed = TRUE)
  expect_match(shown, "Download picture", fixed = TRUE); expect_match(shown, "Two maps side by side", fixed = TRUE); expect_true(all(dl))                                                                      # the file exists, is a real picture, and is named for both places
})


test_that("CP5 up to four places make one sheet: each extra place is looked up and downloaded, the table has a column for each, and a fifth is refused", {
  calls <- with_stubs(NULL); env$geocode <- function(q) { calls$geo <- c(calls$geo, q); tibble(label = paste0(q, ", Somewhere"), lat = lat0 + 0.002, lon = lon0 + 0.002) }
  g <- cmp_g(); r <- env$compare_build(g, "Morgantown; Fairmont;\nGrafton", "hierarchy", "classic", "km")
  expect_null(r$error); expect_equal(calls$geo, c("Morgantown", "Fairmont", "Grafton")); expect_length(calls$fetch, 3); expect_length(r$places, 4); expect_equal(r$size, c(16, 20))
  expect_equal(vapply(r$places, function(p) length(p$rows), 0L), rep(8L, 4)); expect_length(r$labels, 3); expect_equal(env$compare_size_for(2), c(16, 10)); expect_equal(env$compare_size_for(4), c(16, 20))
  f <- tempfile(fileext = ".png"); env$draw_figure(r$fig, f, "png", "classic", preview = TRUE, size = r$size); expect_equal(dim(png::readPNG(f))[1:2], c(5000, 4000))        # 16 x 20 inches at 250 dpi
  r3 <- env$compare_build(g, "A; B", "hierarchy", "classic", "km"); expect_null(r3$error); expect_length(r3$places, 3); expect_equal(r3$size, c(16, 20))        # three places leave one cell empty
  f3 <- tempfile(fileext = ".pdf"); env$draw_figure(r3$fig, f3, "pdf", "classic", size = r3$size); expect_true(file.size(f3) > 5000); expect_equal(rawToChar(readBin(f3, "raw", 4)), "%PDF")
  expect_match(env$compare_build(g, "A; B; C; D")$error, "up to 3 other places"); expect_match(env$compare_build(g, "A; ;  ; A", "hierarchy", "classic", "km")$error %||% "ok", "ok")           # blanks and repeats are dropped
  env$geocode <- function(q) if (q == "Nowhere") tibble(label = character(), lat = numeric(), lon = numeric()) else tibble(label = q, lat = lat0, lon = lon0)
  expect_match(env$compare_build(g, "Here; Nowhere")$error, "No place found for \"Nowhere\"")
  env$fetch_roads <- function(...) stop("down"); expect_match(env$compare_build(g, "Here; There")$error, "did not answer for \"Here\"")
})

test_that("CP6 the preset picker adds one plan at a time, skips repeats, stops at three, and every preset is a searchable text", {
  a <- env$compare_add("", "Savannah, Georgia, United States"); expect_equal(a$text, "Savannah, Georgia, United States"); expect_false(a$full)
  b <- env$compare_add(a$text, "Eixample, Barcelona, Spain"); expect_equal(b$text, "Savannah, Georgia, United States; Eixample, Barcelona, Spain")
  expect_equal(env$compare_add(b$text, "Eixample, Barcelona, Spain")$text, b$text)                                  # no repeat
  c3 <- env$compare_add(b$text, "Venice, Italy"); expect_equal(length(env$compare_split(c3$text)), 3); d <- env$compare_add(c3$text, "Kyoto, Japan"); expect_true(d$full); expect_equal(d$text, c3$text)
  expect_equal(env$compare_add("x", "")$text, "x")
  vals <- unlist(env$compare_presets[-1]); expect_gte(length(vals), 8); expect_true(all(nchar(vals) > 5 & nchar(vals) < env$max_query_chars)); expect_false(any(grepl(";", vals))); expect_equal(anyDuplicated(vals), 0L)
  expect_true(any(grepl("Manhattan", vals))); expect_true(any(grepl("Eixample", vals))); expect_true(any(grepl("Savannah", vals)))
  html <- as.character(env$ui(NULL)); expect_match(html, 'id="cmp_preset"', fixed = TRUE); expect_match(html, "Savannah, Georgia", fixed = TRUE)
})

test_that("CP7 choosing a preset fills the box, builds a sheet of three, and the PDF download is a real PDF named for every place", {
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); shown <- NULL; notes <- character(); out <- list()
  env$geocode <- (function(old) { force(old); function(q) if (grepl("Elkins", q)) old(q) else tibble(label = paste0(q, ", X"), lat = lat0 + 0.002, lon = lon0 + 0.002) })(env$geocode)
  testthat::local_mocked_bindings(showModal = function(ui, ...) { shown <<- as.character(ui); invisible() }, showNotification = function(ui, ...) { notes <<- c(notes, as.character(ui)); invisible() },
                                  updateTextInput = function(session, inputId, value = NULL, ...) { out$upd <<- c(out$upd, value); invisible() }, .package = "shiny")
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, units = "km", draw = 1)
    session$setInputs(cmp_query = "", cmp_preset = "Savannah, Georgia, United States"); session$flushReact()
    session$setInputs(cmp_query = "Savannah, Georgia, United States; Venice, Italy", cmp_go = 1); session$flushReact()
    p <- output$cmp_pdf; out$pdf <<- basename(p); out$pdf_ok <<- file.exists(p) && file.size(p) > 5000 && rawToChar(readBin(p, "raw", 4)) == "%PDF"; out$png <<- basename(output$cmp_dl)
    session$setInputs(cmp_query = "A; B; C", cmp_preset = "Kyoto, Japan"); session$flushReact()
  })
  expect_true("Savannah, Georgia, United States" %in% out$upd); expect_match(shown, "3 maps in a grid", fixed = TRUE); expect_match(shown, "Download PDF", fixed = TRUE)
  expect_true(out$pdf_ok); expect_match(out$pdf, "^compare_elkins.*savannah.*venice.*\\.pdf$"); expect_match(out$png, "\\.png$")
  expect_true(any(grepl("up to 3 other places", notes)))
})

test_that("PS1 each print size keeps an 8 inch layout, the right paper shape, and a pixel count that stays under the cap", {
  for (k in names(env$print_sizes)) {
    wh <- env$print_sizes[[k]]; pl <- env$print_plan(k, "png")
    expect_equal(pl$layout[1], 8); expect_equal(pl$layout[2], 8 * wh[2] / wh[1]); expect_equal(pl$px, round(wh * pl$dpi)); expect_lte(prod(pl$px), env$max_export_px * 1.001); expect_lte(pl$dpi, 300)
  }
  expect_equal(env$print_plan("8x10")$px, c(2400, 3000)); expect_equal(env$print_plan("11x14")$px, c(3300, 4200)); expect_equal(env$print_plan("16x20")$dpi, 300); expect_lt(env$print_plan("24x30")$dpi, 300)
  expect_equal(env$print_plan("18x24")$layout[2], 10.6666667, tolerance = 1e-6)                                  # 0.75 paper is a taller layout, not a stretched one
  expect_equal(env$print_plan("nonsense")$px, c(2400, 3000)); expect_true(is.na(env$print_plan("11x14", "pdf")$dpi)); expect_equal(env$print_plan("11x14", "svg")$size_in, c(11, 14))
  expect_equal(as.numeric(env$print_sizes[["8x10"]]), c(8, 10)); expect_true(all(unlist(lapply(env$print_sizes, function(z) z[1] / z[2])) >= 0.75 & unlist(lapply(env$print_sizes, function(z) z[1] / z[2])) <= 0.8))
})

test_that("PS2 a PNG at a larger print size has the pixels for that size and the same picture, scaled, not a bigger canvas with small type", {
  p <- env$make_map(demo_lines_prepped(), "Test", lon0, lat0, 3000, crs0, view = "hierarchy", style = "classic")
  a <- tempfile(fileext = ".png"); b <- tempfile(fileext = ".png"); c3 <- tempfile(fileext = ".png")
  env$draw_figure(p, a, "png", "classic"); env$draw_figure(p, b, "png", "classic", print_key = "11x14"); env$draw_figure(p, c3, "png", "classic", print_key = "18x24")
  ia <- png::readPNG(a); ib <- png::readPNG(b); ic <- png::readPNG(c3)
  expect_equal(dim(ia)[1:2], c(3000, 2400)); expect_equal(dim(ib)[1:2], c(4200, 3300)); expect_equal(dim(ic)[1:2], c(7200, 5400))
  # the map ring spans the same share of the width at every size, which only happens when the layout scales with the paper
  ring_share <- function(im) { r <- matrix(abs(im[, , 1] - 0xd8 / 255) < 0.02 & abs(im[, , 2] - 0xd2 / 255) < 0.02 & abs(im[, , 3] - 0xc4 / 255) < 0.02, nrow(im)); cc <- range(which(colSums(r) > 0)); diff(cc) / ncol(im) }
  expect_gt(ring_share(ia), 0.5); expect_equal(ring_share(ib), ring_share(ia), tolerance = 0.02); expect_equal(ring_share(ic), ring_share(ia), tolerance = 0.02)
  f <- tempfile(fileext = ".pdf"); env$draw_figure(p, f, "pdf", "classic", print_key = "11x14"); expect_true(file.size(f) > 5000)
})

test_that("PS3 the page offers the sizes, the note says what will be made, and the download is named and cached by size", {
  expect_match(as.character(env$ui(NULL)), 'id="print_size"', fixed = TRUE); expect_match(as.character(env$ui(NULL)), "24 \u00d7 30 inches", fixed = TRUE)
  with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); out <- list()
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, units = "km", draw = 1, format = "png", print_size = "8x10")
    out$n0 <<- as.character(output$print_note$html); out$f0 <<- basename(output$download)
    session$setInputs(print_size = "11x14"); out$n1 <<- as.character(output$print_note$html); p <- output$download; out$f1 <<- basename(p); out$dim <<- dim(png::readPNG(p))[1:2]
    session$setInputs(print_size = "24x30"); out$n2 <<- as.character(output$print_note$html)
    session$setInputs(format = "pdf"); out$n3 <<- as.character(output$print_note$html)
    session$setInputs(print_size = "bogus", format = "png"); out$n4 <<- as.character(output$print_note$html)
  })
  expect_match(out$n0, "2,400 \u00d7 3,000 pixels (300 dpi)", fixed = TRUE); expect_match(out$n1, "11 \u00d7 14 inches", fixed = TRUE); expect_match(out$n1, "3,300 \u00d7 4,200", fixed = TRUE)
  expect_match(out$n2, "fewer dots per inch"); expect_match(out$n3, "PDF and SVG are laid out for 24 \u00d7 30 inches"); expect_match(out$n4, "8 \u00d7 10 inches", fixed = TRUE)
  expect_false(grepl("_11x14_", out$f0)); expect_match(out$f1, "_11x14_"); expect_equal(out$dim, c(4200, 3300))
})

# ---- click to inspect ------------------------------------------------------------------------------------------------
ins_lines <- function() { l <- env$prep_lines(fixture_roads(), lon0, lat0, 3000, crs0); l$maxspeed[l$name %in% "Third Street"] <- "35 mph"; l$maxspeed[l$name %in% "Corridor H"] <- "50"; l }
ins_panel <- list(x0 = 0.1, x1 = 0.9, y0 = 0.2, y1 = 0.8, xr = cx + c(-3000, 3000), yr = cy + c(-3000, 3000))
at_xy <- function(x, y, pn = ins_panel) c(fx = pn$x0 + (x - pn$xr[1]) / diff(pn$xr) * (pn$x1 - pn$x0), fy = pn$y0 + (pn$yr[2] - y) / diff(pn$yr) * (pn$y1 - pn$y0))
click <- function(x, y, lines = ins_lines()) { f <- at_xy(x, y); env$inspect_street(lines, ins_panel, f[["fx"]], f[["fy"]], 3000, crs0) }

test_that("I1 the panel is found exactly where the map circle is drawn (checked against the ring's own pixels)", {
  lines <- demo_lines_prepped()
  for (v in c("hierarchy", "speed")) {
    p <- env$make_map(lines, "Test", lon0, lat0, 3000, crs0, view = v, style = "classic", shape = NULL, inside = NULL)
    pn <- env$map_panel(p, "classic"); expect_false(is.null(pn), label = v)
    f <- tempfile(fileext = ".png"); showtext::showtext_opts(dpi = env$preview_dpi); ragg::agg_png(f, width = 8, height = 10, units = "in", res = env$preview_dpi, background = env$styles$classic$bg); print(p); dev.off()
    a <- png::readPNG(f); im <- matrix(grDevices::rgb(a[, , 1], a[, , 2], a[, , 3]), nrow(a))        # the real render, text at the true scale
    ring <- matrix(toupper(im) == toupper(env$styles$classic$ring), nrow(im)); rr <- range(which(rowSums(ring) > 0)); cc <- range(which(colSums(ring) > 0))
    expect_equal(c(pn$x0, pn$x1) * ncol(im), cc + c(-1, 0), tolerance = 0.01, scale = ncol(im), label = paste(v, "x"))        # within about 1% of the picture's width
    expect_equal(c(pn$y0, pn$y1) * nrow(im), rr + c(-1, 0), tolerance = 0.01, scale = nrow(im), label = paste(v, "y"))
    expect_equal(pn$xr, cx + c(-3000, 3000), tolerance = 1e-6, scale = 1); expect_equal(pn$yr, cy + c(-3000, 3000), tolerance = 1e-6, scale = 1)
  }
})

test_that("I2 clicks map to ground coordinates: the middle of the panel is the center, the corners are the limits, and outside is nothing", {
  expect_equal(unname(env$panel_xy(ins_panel, 0.5, 0.5)), c(cx, cy), tolerance = 1e-9, scale = 1)
  expect_equal(unname(env$panel_xy(ins_panel, 0.1, 0.2)), c(cx - 3000, cy + 3000), tolerance = 1e-9, scale = 1)      # top left is west and north
  expect_equal(unname(env$panel_xy(ins_panel, 0.9, 0.8)), c(cx + 3000, cy - 3000), tolerance = 1e-9, scale = 1)
  expect_null(env$panel_xy(ins_panel, 0.05, 0.5)); expect_null(env$panel_xy(ins_panel, 0.5, 0.85)); expect_null(env$panel_xy(NULL, 0.5, 0.5)); expect_null(env$panel_xy(ins_panel, NA, 0.5))
})

test_that("I3 the street nearest a click is reported with its class, limit, direction, and lengths", {
  b <- click(cx + 1400, cy + 603)                                                          # 3 m off Beverly Pike, beyond the grid that shares its row
  expect_equal(b$status, "ok"); expect_equal(b$name, "Beverly Pike"); expect_equal(b$kind, "secondary"); expect_equal(b$role, "Major collectors"); expect_equal(b$heading, "E–W")
  expect_true(is.na(b$speed)); expect_equal(b$n_pieces, 1); expect_equal(b$piece_km, 3, tolerance = 0.002); expect_equal(b$street_km, 3, tolerance = 0.002); expect_equal(b$dist_m, 3, tolerance = 1e-6)
  t3 <- click(cx - 1400, cy - 598); expect_equal(t3$name, "Third Street"); expect_equal(t3$speed, "35 mph")
  c8 <- click(cx + 2, cy + 1500); expect_equal(c8$name, "Corridor H"); expect_equal(c8$speed, "50 km/h"); expect_equal(c8$heading, "N–S"); expect_equal(c8$street_km, 2 * 2.964, tolerance = 0.002)   # clipped to the ring
  u <- click(cx + 200, cy + 500); expect_equal(u$status, "ok"); expect_true(is.na(u$name)); expect_equal(u$kind, "residential"); expect_equal(u$role, "Local streets")
  expect_equal(click(cx + 100, cy - 100)$status, "none")                                   # 100 m from everything: beyond the 45 m reach
  expect_equal(env$inspect_street(ins_lines(), ins_panel, 0.01, 0.5, 3000, crs0)$status, "outside")
  l <- ins_lines(); l$name[l$name %in% "Beverly Pike"] <- NA                                # a street with the same name is summed, an unnamed one is a single piece
  expect_equal(click(cx + 1400, cy + 603, l)$n_pieces, 1)
  l2 <- ins_lines(); extra <- l2[l2$name %in% "Third Street", ]; extra$geometry <- sf::st_geometry(extra) + c(0, -10); sf::st_crs(extra) <- sf::st_crs(l2); l2 <- rbind(l2, extra)
  w <- click(cx - 1400, cy - 598, l2); expect_equal(w$n_pieces, 2); expect_equal(w$street_km, 2 * w$piece_km, tolerance = 1e-9)
})

test_that("I4 the card says what to do, what it found, and when nothing is near; and it speaks the chosen unit", {
  txt <- function(res, u = "km") as.character(env$inspect_card(res, u))
  expect_length(txt(NULL), 0); expect_length(txt(list(status = "outside")), 0); expect_length(txt(click(cx + 100, cy - 100)), 0)      # round 17: no card unless a street is selected
  ok <- click(cx + 1400, cy + 603); expect_match(txt(ok), "Beverly Pike"); expect_match(txt(ok), "secondary (major collectors)", fixed = TRUE); expect_match(txt(ok), "not mapped"); expect_match(txt(ok), "3.0 km")
  expect_match(txt(ok, "mi"), "1.9 mi"); expect_match(txt(click(cx + 200, cy + 500)), "Unnamed street")
  expect_equal(env$with_units("mi", env$fmt_len_km(0.1)), "330 ft"); expect_equal(env$fmt_len_km(0.45), "450 m"); expect_equal(env$fmt_len_km(12.34), "12.3 km")
})

test_that("I5 the server answers a click from the measured panel, clears when the map type changes, and shows nothing with the switch off", {
  with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); out <- list()
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, units = "km", draw = 1, inspect = FALSE)
    out$off <<- as.character(output$stats$html)
    session$setInputs(inspect = TRUE); out$hint <<- as.character(output$stats$html)
    pn <- env$map_panel(built(), "classic"); f <- c(pn$x0 + (4400 / 6000) * (pn$x1 - pn$x0), pn$y0 + 0.5 * (pn$y1 - pn$y0))   # 1.4 km east of the center, on the avenue and past the end of the grid
    session$setInputs(map_click = list(fx = f[1], fy = f[2], t = 1)); session$flushReact(); out$hit <<- as.character(output$stats$html)
    session$setInputs(map_click = list(fx = 0.001, fy = 0.001, t = 2)); session$flushReact(); out$outside <<- as.character(output$stats$html)
    session$setInputs(map_click = list(fx = f[1], fy = f[2], t = 3)); session$flushReact(); session$setInputs(view = "direction"); session$flushReact(); out$cleared <<- as.character(output$stats$html)
  })
  expect_false(grepl("class=\"inspect\"", out$off, fixed = TRUE)); expect_false(grepl("class=\"inspect\"", out$hint, fixed = TRUE))      # round 17: switching it on shows no card until a street is picked
  expect_match(out$hit, "Davis Avenue"); expect_match(out$hit, "primary"); expect_false(grepl("class=\"inspect\"", out$outside, fixed = TRUE)); expect_false(grepl("class=\"inspect\"", out$cleared, fixed = TRUE))
})

test_that("I6 the page has the inspect switch and sends clicks to the server instead of zooming while it is on", {
  expect_match(as.character(env$ui(NULL)), 'id="inspect"', fixed = TRUE)
  src <- paste(readLines(Sys.getenv("APP_FILE"), warn = FALSE), collapse = "\n"); expect_match(src, "setInputValue('map_click'", fixed = TRUE); expect_match(src, "$('#inspect').is(':checked')", fixed = TRUE)
})

test_that("U6 the page holds the Draw button while the radius is converted to a new unit", {
  src <- paste(readLines(Sys.getenv("APP_FILE"), warn = FALSE), collapse = "\n")
  expect_match(src, "$(document).on('change', 'input[name=units]'", fixed = TRUE); expect_match(src, "$('#draw').prop('disabled', true)", fixed = TRUE)
  expect_match(src, "if (s) s.update({postfix: ' ' + u}); $('#draw').prop('disabled', false);", fixed = TRUE)      # released by the server's radiusUnit message
})

# ---- highlight and find a street -------------------------------------------------------------------------------------
test_that("HL1 polylines come back as picture fractions that land where the street is, and only for the streets asked for", {
  l <- ins_lines(); i <- which(l$name %in% "Beverly Pike"); pl <- env$hl_polylines(l, i, ins_panel, 3000)
  expect_gte(length(pl), 1); pts <- do.call(rbind, lapply(strsplit(unlist(pl), ","), as.numeric))
  expect_true(all(pts[, 1] >= ins_panel$x0 - 1e-6 & pts[, 1] <= ins_panel$x1 + 1e-6)); expect_true(all(pts[, 2] >= ins_panel$y0 - 1e-6 & pts[, 2] <= ins_panel$y1 + 1e-6))
  f <- at_xy(cx + 1400, cy + 600); k <- which.min(abs(pts[, 1] - f[["fx"]])); expect_equal(pts[k, 2], f[["fy"]], tolerance = 0.002)      # the avenue runs east-west 600 m north of center
  expect_equal(env$hl_polylines(l, integer(0), ins_panel, 3000), list()); expect_equal(env$hl_polylines(l, i, NULL, 3000), list())
  expect_lte(length(env$hl_polylines(l, rep(i, 3000), ins_panel, 3000, max_lines = 10)), 10)
})

test_that("HL2 a click can only pick a street that is drawn, and reports every piece of a named street", {
  l <- ins_lines(); f <- at_xy(cx + 200, cy + 500)                                               # an unnamed local street
  expect_equal(env$inspect_street(l, ins_panel, f[["fx"]], f[["fy"]], 3000, crs0)$status, "ok")
  keep <- env$drawn_rows(l, "hierarchy", FALSE); expect_false(any(l$role[keep] == env$roles$role[5])); expect_equal(length(env$drawn_rows(l, "speed", FALSE)), nrow(l)); expect_equal(length(env$drawn_rows(l, "hierarchy", TRUE)), nrow(l))
  expect_equal(env$inspect_street(l, ins_panel, f[["fx"]], f[["fy"]], 3000, crs0, keep = keep)$status, "none")
  l2 <- l; extra <- l2[l2$name %in% "Third Street", ]; extra$geometry <- sf::st_geometry(extra) + c(0, -10); sf::st_crs(extra) <- sf::st_crs(l2); l2 <- rbind(l2, extra)
  g <- click(cx - 1400, cy - 598, l2); expect_length(g$idx, 2); expect_true(g$piece %in% g$idx)
})

test_that("HL3 street search matches every word, ignores case and accents, reports total length, and says why nothing shows", {
  l <- ins_lines(); r <- env$street_search(l, "third")
  expect_equal(r$status, "ok"); expect_equal(r$items$name, "Third Street"); expect_equal(unname(r$items$km), sum(l$km[l$name %in% "Third Street"]))
  expect_equal(env$street_search(l, "STREET third")$status, "ok"); expect_equal(env$street_search(l, "thirdx")$status, "none"); expect_equal(env$street_search(l, "third zzz")$status, "none")
  expect_equal(env$street_search(l, "x")$status, "short"); expect_equal(env$street_search(l, strrep("a", 90))$status, "long"); expect_equal(env$street_search(l, NULL)$status, "short")
  l$name[l$name %in% "Third Street"] <- "Th\u00edrd Street"; expect_equal(env$street_search(l, "third")$status, "ok")
  keep <- env$drawn_rows(l, "hierarchy", FALSE); hid <- which(l$role == env$roles$role[5] & !is.na(l$name))
  if (length(hid)) { l$name[hid[1]] <- "Quietlane"; expect_equal(env$street_search(l, "quietlane", keep = env$drawn_rows(l, "hierarchy", FALSE))$status, "hidden") }
  expect_equal(env$street_search(l, "zzzz")$status, "none")
})

test_that("HL4 the server shows the matches with lengths in the chosen unit, and clears them when the map changes", {
  with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); out <- list()
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, units = "km", draw = 1, inspect = FALSE)
    session$setInputs(street_q = "davis", street_go = 1); session$flushReact(); out$km <<- as.character(output$street_found$html)
    session$setInputs(units = "mi"); session$flushReact(); out$cleared <<- is.null(output$street_found)
    session$setInputs(street_q = "davis", street_go = 2); session$flushReact(); out$mi <<- as.character(output$street_found$html)
    session$setInputs(street_q = "nonesuch", street_go = 3); session$flushReact(); out$none <<- as.character(output$street_found$html)
    session$setInputs(street_q = "d", street_go = 4); session$flushReact(); out$short <<- as.character(output$street_found$html)
  })
  expect_match(out$km, "1 street match"); expect_match(out$km, "Davis Avenue"); expect_match(out$km, " km"); expect_true(out$cleared); expect_match(out$mi, " mi"); expect_match(out$none, "No street with that name"); expect_match(out$short, "at least two letters")
})

test_that("HL5 the page draws the overlay in the browser and offers the switches", {
  html <- as.character(env$ui(NULL)); expect_match(html, 'id="street_q"', fixed = TRUE); expect_match(html, 'id="street_go"', fixed = TRUE); expect_match(html, 'id="hl_all"', fixed = TRUE)
  src <- paste(readLines(Sys.getenv("APP_FILE"), warn = FALSE), collapse = "\n"); expect_match(src, "addCustomMessageHandler('hl'", fixed = TRUE); expect_match(src, "vector-effect", fixed = TRUE); expect_match(src, "pointer-events:none", fixed = TRUE)
})

test_that("SB1 the scale bar uses round lengths in the chosen unit, stays inside the corners, and does not move the measured panel", {
  expect_equal(env$nice_length(0.43), 0.2); expect_equal(env$nice_length(0.5), 0.5); expect_equal(env$nice_length(7.9), 5); expect_equal(env$nice_length(12), 10); expect_equal(env$nice_length(1), 1); expect_true(is.na(env$nice_length(0)))
  ctr <- sf::st_sfc(sf::st_point(c(cx, cy)), crs = crs0); pal <- env$get_style("classic")
  for (u in c("km", "mi")) {
    lay <- env$with_units(u, env$map_furniture(ctr, 3000, pal)); expect_length(lay, 5)
    seg <- lay[[1]]$data; expect_true(all(abs(c(seg$x, seg$xend) - cx) <= 3000)); expect_true(all(abs(c(seg$y, seg$yend) - cy) <= 3000))
    len_u <- (seg$xend - seg$x) / 1000 / (if (u == "mi") env$km_per_mile else 1); expect_equal(len_u, 0.5, tolerance = 1e-9)        # 0.3 of the 3 km radius is 0.9 km, or 0.56 mi: both round down to a half unit
    # nothing of the bar or arrow lies inside the circle
    pts <- rbind(cbind(c(seg$x, seg$xend), c(seg$y, seg$yend)), cbind(lay[[4]]$data$x, lay[[4]]$data$y)); expect_true(all(sqrt((pts[, 1] - cx)^2 + (pts[, 2] - cy)^2) > 3000))
  }
  expect_match(as.character(env$with_units("mi", env$map_furniture(ctr, 3000, pal))[[3]]$aes_params$label), "mi$"); expect_match(as.character(env$with_units("km", env$map_furniture(ctr, 3000, pal))[[3]]$aes_params$label), "km$")
  p <- env$make_map(demo_lines_prepped(), "Test", lon0, lat0, 3000, crs0, view = "hierarchy", style = "classic"); pn <- env$map_panel(p, "classic")
  expect_equal(pn$xr, cx + c(-3000, 3000), tolerance = 1e-6, scale = 1)
})

# ---- bike and walk -----------------------------------------------------------------------------------------------------
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

test_that("BW1 sidewalk and bike lane tags are read from every spelling, and lanes, tracks and separate count but shared lanes and no do not", {
  f <- env$facility_flags(bw_roads()$osm_lines); n <- nrow(bw_roads()$osm_lines)
  expect_length(f$walk, n); expect_equal(which(f$walk), c(1, 5, 6)); expect_equal(which(f$bike), c(2, 5))                       # both, separate, sidewalk:right=yes; lane, track
  l <- bw_roads()$osm_lines; names(l)[names(l) == "cycleway.left"] <- "cycleway:left"; expect_equal(which(env$facility_flags(l)$bike), c(2, 5))     # colon spelling
  bare <- fixture_roads()$osm_lines; fb <- env$facility_flags(bare); expect_false(any(fb$walk)); expect_false(any(fb$bike)); expect_length(fb$walk, nrow(bare))
  one <- bw_roads()$osm_lines[1, ]; expect_equal(env$facility_flags(one)$walk, TRUE)                                              # a single row works too
  pl <- env$prep_lines(bw_roads(), lon0, lat0, 3000, crs0); expect_true(all(c("walk_tag", "bike_tag") %in% names(pl))); expect_true(any(pl$walk_tag)); expect_true(any(pl$bike_tag))
  expect_equal(as.character(env$active_class(pl)[pl$walk_tag & pl$bike_tag][1]), "Sidewalk and bike lane")
  expect_equal(levels(env$active_class(pl)), env$active_levels); expect_equal(length(env$active_levels), 6)
})

test_that("BW2 separate paths are classified, crossings are dropped, everything is clipped to the ring, and nothing gives NULL", {
  p <- env$prep_paths(bw_paths(), lon0, lat0, 3000, crs0)
  expect_equal(sort(unique(p$kind)), c(5L, 6L)); expect_equal(sum(p$kind == 6L), 2L)                                          # cycleway and the path marked for bicycles
  expect_equal(sum(p$kind == 5L), 3L)                                                                                          # sidewalk, plain path, pedestrian; the crossing is gone
  expect_equal(sum(p$km[p$kind == 6L]), 1 + 0.6, tolerance = 0.002)
  small <- env$prep_paths(bw_paths(), lon0, lat0, 450, crs0); expect_lt(sum(small$km), sum(p$km)); expect_equal(max(small$km) * 1000 <= 2 * 450 * 0.988 + 1, TRUE)
  expect_null(env$prep_paths(list(osm_lines = NULL), lon0, lat0, 3000, crs0)); expect_null(env$prep_paths(fixture_roads(), lon0, lat0, 3000, crs0))   # streets only: no paths
  cw <- bw_paths(); cw$osm_lines <- cw$osm_lines[cw$osm_lines$footway %in% "crossing", ]; expect_null(env$prep_paths(cw, lon0, lat0, 3000, crs0))
  expect_true("paths" %in% names(formals(env$view_data))); expect_equal(env$detail_classes("paths"), env$path_hw); expect_false(any(env$path_hw %in% env$hw))
})

test_that("BW3 the Bike & walk numbers add up and the text is honest about what tags mean", {
  l <- env$prep_lines(bw_roads(), lon0, lat0, 3000, crs0); p <- env$prep_paths(bw_paths(), lon0, lat0, 3000, crs0)
  vd <- env$view_data("active", l, 3000, ctr_m(), paths = p); expect_equal(vd$view, "active"); expect_equal(unname(vd$km_by[["Bike path"]]), sum(p$km[p$kind == 6L])); expect_equal(unname(vd$km_by[["Walking path"]]), sum(p$km[p$kind == 5L]))
  expect_equal(sum(vd$km_by[1:4]), sum(l$km), tolerance = 1e-9)
  expect_equal(vd$tiles[[2]]$value, paste0(round(100 * sum(l$km[l$walk_tag]) / sum(l$km)), "%")); expect_equal(vd$tiles[[3]]$value, paste0(round(100 * sum(l$km[l$bike_tag]) / sum(l$km)), "%"))
  expect_match(vd$caption, "not a survey"); expect_match(vd$caption, "may well have a sidewalk"); expect_match(vd$subtitle, "tagged with a sidewalk")
  v0 <- env$view_data("active", l, 3000, ctr_m(), paths = NULL); expect_false(v0$has_paths); expect_equal(unname(v0$km_by[["Bike path"]]), 0)
  mi <- env$view_data("active", l, 3000, ctr_m(), units = "mi", paths = p); expect_match(mi$subtitle, " mi| ft"); expect_false(grepl(" km", mi$subtitle))
  expect_equal(env$get_view("active"), "active"); expect_equal(unname(env$views["active"]), "Bike & walk")
})

test_that("BW4 the map draws all six classes in every style with a legend that says how much of each there is, and the file is a real picture", {
  l <- env$prep_lines(bw_roads(), lon0, lat0, 3000, crs0); p <- env$prep_paths(bw_paths(), lon0, lat0, 3000, crs0)
  for (st in c("classic", "dusk", "mono")) {
    m <- env$make_map(l, "Test", lon0, lat0, 3000, crs0, view = "active", style = st, paths = p); expect_s3_class(m, "patchwork")
    f <- tempfile(fileext = ".png"); env$draw_figure(m, f, "png", st, preview = TRUE); expect_gt(file.size(f), 30000)
    pn <- env$map_panel(m, st); expect_false(is.null(pn)); expect_equal(pn$xr, cx + c(-3000, 3000), tolerance = 1e-6, scale = 1)
  }
  m0 <- env$make_map(l, "Test", lon0, lat0, 3000, crs0, view = "active", style = "classic", paths = NULL); f <- tempfile(fileext = ".png"); env$draw_figure(m0, f, "png", "classic", preview = TRUE); expect_gt(file.size(f), 30000)
  m <- env$make_map(l, "Test", lon0, lat0, 3000, crs0, view = "active", style = "classic", paths = p); b <- ggplot2::ggplot_build(m[[1]]); lab <- unlist(lapply(b$plot$scales$scales, function(s) if ("colour" %in% s$aesthetics) s$labels))
  expect_true(any(grepl("Bike path  \u00b7", lab))); expect_true(any(grepl("Sidewalk tagged", lab)))
})

test_that("BW5 the paths are downloaded only when Bike & walk is chosen, once per place, with the paths class of features, and the map type survives a share link", {
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); fetched <- list(); out <- list()
  env$fetch_roads <- (function(old) { force(old); function(lon, lat, radius_m, crs_m, shape = NULL, on_try = NULL, detail = "all") { fetched[[length(fetched) + 1]] <<- detail; if (detail == "paths") bw_paths() else bw_roads() } })(env$fetch_roads)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, units = "km", draw = 1)
    out$before <<- unlist(fetched); session$setInputs(view = "active"); session$flushReact(); html <- as.character(output$stats$html); out$tiles <<- html; f <- output$map; out$img <<- !is.null(f$src) && nzchar(f$src)
    session$setInputs(view = "hierarchy"); session$flushReact(); session$setInputs(view = "active"); session$flushReact()
    out$after <<- unlist(fetched); out$dl <<- file.size(output$download) > 30000
  })
  expect_false("paths" %in% out$before); expect_equal(sum(out$after == "paths"), 1); expect_match(out$tiles, "Sidewalks tagged"); expect_match(out$tiles, "Bike lanes tagged"); expect_true(out$img); expect_true(out$dl)
  expect_equal(env$share_params(list(query = "Elkins", view = "active", units = "km", radius = 3), 1)$v, "active")
})

test_that("BW6 comparing in Bike & walk loads each place's paths, and a street click reports its sidewalk and bike lane tags", {
  calls <- with_stubs(NULL); cmp_geo(calls); fetched <- character()
  env$fetch_roads <- function(lon, lat, radius_m, crs_m, shape = NULL, on_try = NULL, detail = "all") { fetched <<- c(fetched, detail); if (detail == "paths") bw_paths() else bw_roads() }
  g <- cmp_g(); g$lines <- env$prep_lines(bw_roads(), lon0, lat0, 3000, crs0)
  r <- env$compare_build(g, "Morgantown", "active", "classic", "km"); expect_null(r$error); expect_equal(sum(fetched == "paths"), 2); expect_s3_class(r$fig, "patchwork")
  pn <- ins_panel; l <- g$lines; i <- which(l$walk_tag)[1]; mid <- sf::st_coordinates(sf::st_line_sample(l[i, ], sample = 0.5))[1, 1:2]; f <- at_xy(unname(mid[1]), unname(mid[2]))
  res <- env$inspect_street(l, pn, f[["fx"]], f[["fy"]], 3000, crs0); expect_equal(res$status, "ok"); expect_true(isTRUE(res$walk)); expect_false(is.null(res$bike))
  card <- as.character(env$inspect_card(res, "km")); expect_match(card, "Sidewalk"); expect_match(card, "Bike lane")
})

test_that("SV1 the saved line holds the options but never the address, and the switch is off by default", {
  inp <- list(query = "Elkins", view = "speed", units = "mi", radius = 1.5, style = "dusk", area = "circle", locals = FALSE, labels = TRUE, use_addr = TRUE, addr = "12 Main St")
  p <- env$share_params(inp, 1); line <- sub("&go=1$", "", env$share_url(p[setdiff(names(p), c("ua", "ad"))], ""))
  expect_false(grepl("12%20Main|ad=|ua=|go=", line)); expect_match(line, "q=Elkins"); expect_match(line, "v=speed"); expect_false(grepl("cv=", line, fixed = TRUE))
  st <- env$parse_share(line); expect_false(isTRUE(st$go)); expect_equal(st$v, "speed"); expect_equal(st$u, "mi"); expect_equal(st$r, 1.5)
  html <- as.character(env$ui(NULL)); expect_match(html, 'id="remember"', fixed = TRUE); expect_match(html, "Forget saved settings", fixed = TRUE); expect_match(html, "address is never saved", fixed = TRUE)
  expect_false(grepl('id="remember"[^>]*checked', html))
  src <- paste(readLines(Sys.getenv("APP_FILE"), warn = FALSE), collapse = "\n"); expect_match(src, "localStorage.setItem('overmaps.settings'", fixed = TRUE); expect_match(src, "localStorage.removeItem('overmaps.settings')", fixed = TRUE)
})

test_that("SV2 saved settings fill the form at the start of a visit with a place lookup but no drawing, and a shared link takes priority", {
  run <- function(saved, link = NULL) {
    calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); ups <- list(); drawn <- NULL
    rec <- function(nm) function(session, inputId, ...) { a <- list(...); ups[[length(ups) + 1]] <<- c(list(fn = nm, id = inputId), a); invisible() }
    testthat::local_mocked_bindings(updateRadioButtons = rec("radio"), updateTextInput = rec("text"), updateSelectInput = rec("select"), updateSliderInput = rec("slider"), showNotification = function(ui, ...) invisible(), .package = "shiny", .env = environment())
    testthat::local_mocked_bindings(update_switch = function(id, value = NULL, ...) { ups[[length(ups) + 1]] <<- list(fn = "switch", id = id, value = value); invisible() }, .package = "bslib", .env = environment())
    env$parse_share <- function(qs) if (!is.null(link) && !identical(qs, saved)) link else orig_parse(qs)
    on.exit(env$parse_share <- orig_parse, add = TRUE)
    testServer(env$server, { session$setInputs(saved_settings = saved); session$flushReact(); drawn <<- tryCatch({ geo(); TRUE }, error = function(e) FALSE) })
    list(ups = ups, calls = calls, drawn = drawn)
  }
  val <- function(r, id, k = "selected") { u <- Filter(function(x) identical(x$id, id), r$ups); if (length(u)) u[[1]][[k]] else NULL }
  r <- run("?q=Elkins&r=2&u=km&v=density&s=mono&a=circle&l=1&lb=0")
  expect_equal(val(r, "query", "value"), "Elkins"); expect_equal(val(r, "view"), "density"); expect_equal(val(r, "style"), "mono"); expect_equal(val(r, "area"), "circle")
  expect_true(val(r, "remember", "value")); expect_true(val(r, "locals", "value")); expect_false(val(r, "labels", "value"))
  expect_gte(length(r$calls$geo), 1); expect_length(r$calls$fetch, 0); expect_false(r$drawn)                         # the place was looked up; no streets were downloaded and nothing was drawn
  link <- list(q = "FromLink", v = "speed", go = FALSE)
  r2 <- run("?q=Elkins&v=density", link = link)
  expect_equal(val(r2, "query", "value"), "FromLink"); expect_equal(val(r2, "view"), "speed"); expect_null(val(r2, "remember", "value"))        # the link was applied, the saved line ignored
  r3 <- run("?v=density"); expect_null(val(r3, "query", "value")); expect_length(r3$calls$geo, 0)                  # a saved line with no place does nothing
  r4 <- run("not a query string \u00a7"); expect_length(r4$calls$geo, 0)
})

test_that("PD1 the panel is read from the real render, so it does not depend on the measuring dpi, and the page measures at the on-screen dpi", {
  p <- env$make_map(demo_lines_prepped(), "Elkins, West Virginia", lon0, lat0, 3000, crs0, view = "hierarchy", style = "classic", show_local = FALSE)
  a <- env$map_panel(p, "classic", dpi = 150); b <- env$map_panel(p, "classic", dpi = env$preview_dpi)
  expect_lt(abs(a$y0 - b$y0), 0.002); expect_lt(abs(a$y1 - b$y1), 0.002)                                           # the old capture method moved by about 0.005 between these dpi values
  expect_equal(b$x1 - b$x0, (b$y1 - b$y0) * 10 / 8, tolerance = 0.01)                                              # the panel is a square: the circle's bounding box
  with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326)); got <- NULL; seen <- NULL
  orig <- env$map_panel; env$map_panel <- function(p, style = "classic", size = env$fig_in, dpi = 150) { seen <<- c(seen, dpi); orig(p, style, size, dpi) }
  withr::defer(env$map_panel <- orig)
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", radius = 3, labels = TRUE, style = "classic", view = "hierarchy", area = "circle", locals = FALSE, units = "km", draw = 1, inspect = TRUE)
    session$setInputs(map_click = list(fx = 0.5, fy = 0.5, t = 1)); session$flushReact()
  })
  expect_true(length(seen) >= 1 && all(seen == env$preview_dpi), label = paste("dpi used:", paste(unique(seen), collapse = ",")))
})

# ---- options change without a new download -------------------------------------------------------------------------------
test_that("LV1 a smaller radius, another area type, and fewer streets redraw from the streets on hand; a bigger reach asks for a new download", {
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(1000), 4326)); got <- list()
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1); session$setInputs(choice = "1", radius = 3, units = "km", area = "circle", all_streets = FALSE, draw = 1)
    g0 <- geo(); got$r0 <<- g0$radius_m; got$stale0 <<- g0$stale
    session$setInputs(radius = 2); got$r1 <<- geo()$radius_m; got$stale1 <<- geo()$stale; got$n1 <<- nrow(geo()$lines)
    session$setInputs(area = "dim"); got$inside <<- !is.null(geo()$inside); got$r2 <<- geo()$radius_m
    session$setInputs(area = "city"); got$shape <<- !is.null(geo()$shape); got$r3 <<- geo()$radius_m
    session$setInputs(area = "circle", radius = 3.05); got$r4 <<- geo()$radius_m; got$stale4 <<- geo()$stale            # inside the box the download covers (circle plus 100 m)
    session$setInputs(radius = 3.5); got$r5 <<- geo()$radius_m; got$stale5 <<- geo()$stale
    got$hint <<- as.character(output$need_draw$html)
    session$setInputs(radius = 3); got$back <<- geo()$stale                                                     # back inside: no longer stale
  })
  expect_length(calls$fetch, 1)                                                                                 # every change above came from one download
  expect_equal(got$r0, 3000); expect_null(got$stale0)
  expect_equal(got$r1, 2000); expect_null(got$stale1); expect_gt(got$n1, 0)
  expect_true(got$inside); expect_equal(got$r2, 2000)
  expect_true(got$shape); expect_equal(got$r3, round(env$shape_radius(env$city_shape(st_transform(sq_shape(1000), 4326), crs0))))
  expect_equal(got$r4, 3050)
  expect_equal(got$r5, 3050); expect_match(got$stale5, "streets farther out")                                    # the map stays as it was
  expect_match(got$hint, "Loading streets farther out"); expect_match(got$hint, "arrive")      # round 16: the larger reach downloads by itself, so the hint says so instead of asking for a click
  expect_null(got$back)
})

test_that("LV2 a new place, a new address, or local streets that were never downloaded need a new download; fewer streets never do", {
  calls <- with_stubs(NULL, addr = tibble(label = "401 Davis Avenue, Elkins", lat = lat0 + 0.002, lon = lon0 + 0.002)); got <- list()
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1); session$setInputs(choice = "1", radius = 10, units = "km", area = "circle", all_streets = TRUE, draw = 1)
    got$d0 <<- geo()$detail                                                                                    # 10 km with every street switched on is full detail
    session$setInputs(all_streets = FALSE); got$d1 <<- geo()$detail; got$stale1 <<- geo()$stale                 # fewer streets: filtered, no download
    got$classes1 <<- unique(as.character(geo()$lines$highway))
    session$setInputs(radius = 5); got$d2 <<- geo()$detail; got$stale2 <<- geo()$stale                          # full detail was downloaded, so 5 km can have it
    session$setInputs(all_streets = FALSE, radius = 10, draw = 2); got$d3 <<- geo()$detail                      # a second download without local streets
    session$setInputs(all_streets = TRUE); got$stale3 <<- geo()$stale                                           # local streets were never downloaded
    session$setInputs(all_streets = FALSE, radius = 5); got$stale4 <<- geo()$stale
    session$setInputs(all_streets = TRUE, radius = 10, choice = "2"); got$stale5 <<- geo()$stale                # another place
    session$setInputs(choice = "1", all_streets = FALSE, use_addr = TRUE, addr = "401 Davis Ave"); got$stale6 <<- geo()$stale   # an address that was never looked up
  })
  expect_length(calls$fetch, 2)
  expect_equal(got$d0, "all"); expect_equal(got$d1, "main"); expect_null(got$stale1)
  expect_false(any(c("residential", "unclassified") %in% got$classes1)); expect_true(all(got$classes1 %in% env$detail_classes("main")))
  expect_equal(got$d2, "all"); expect_null(got$stale2); expect_equal(got$d3, "main")
  expect_match(got$stale3, "local streets"); expect_match(got$stale4, "local streets"); expect_match(got$stale5, "place you found"); expect_match(got$stale6, "new address")
})

test_that("LV3 typing in a box that does not affect the map leaves the drawn map untouched", {
  calls <- with_stubs(NULL); same <- NULL
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1); session$setInputs(choice = "1", radius = 3, units = "km", area = "circle", all_streets = FALSE, use_addr = FALSE, draw = 1)
    g0 <- geo(); session$setInputs(addr = "401 Dav"); session$setInputs(addr = "401 Davis"); same <<- identical(geo(), g0)
  })
  expect_true(same); expect_length(calls$fetch, 1)
})

test_that("LV4 plan_live decides from the stored box: same options keep the map, a covered change rebuilds it, an uncovered one asks for a download", {
  ld <- list(roads = fixture_roads(), key = "k", crs_m = crs0, base_lon = lon0, base_lat = lat0, detail = "all", center_txt = NULL, shape = NULL, inside = NULL,
             cover = st_as_sfc(st_bbox(st_buffer(st_sfc(st_point(c(cx, cy)), crs = crs0), 3100))), sig = env$option_sig(3, "circle", FALSE, ""), net = NULL, lines = NULL, radius_m = 3000L)
  st <- list(key = "k", radius_km = 3, area = "circle", all_streets = FALSE, addr_sig = "", boundary = NULL)
  expect_identical(env$plan_live(ld, st), ld)
  w <- env$plan_live(ld, utils::modifyList(st, list(radius_km = 1))); expect_equal(w$radius_m, 1000); expect_true(isTRUE(w$live)); expect_gt(nrow(w$lines), 0)
  bb <- st_bbox(w$lines); expect_lte(max(abs(c(bb[["xmin"]], bb[["xmax"]]) - cx), abs(c(bb[["ymin"]], bb[["ymax"]]) - cy)), 988.5); expect_gt(max(abs(c(bb[["xmin"]], bb[["xmax"]]) - cx)), 900)      # clipped to the smaller ring
  expect_s3_class(env$plan_live(ld, utils::modifyList(st, list(radius_km = 3.2))), "need_draw")
  expect_s3_class(env$plan_live(ld, utils::modifyList(st, list(key = "other"))), "need_draw")
  expect_s3_class(env$plan_live(ld, utils::modifyList(st, list(area = "city"))), "need_draw")                          # no boundary to outline
  expect_equal(env$plan_live(ld, utils::modifyList(st, list(radius_km = 1.00001)))$radius_m, 1000)                       # the radius is rounded: a near-identical echo is not a change
})

# ---- suggestions while typing and addresses inside the place ----------------------------------------------------------------
pl_items <- tibble(label = c("Elkins, Randolph County, West Virginia, United States", "Elkins Park, Pennsylvania, United States"), lon = c(lon0, -75.1), lat = c(lat0, 40.07), osm_type = c("R", "N"), osm_id = c(123456, 777))
ad_items <- tibble(label = c("401 Davis Avenue, Elkins, West Virginia", "401 Davis Street, Elkins, West Virginia"), lon = c(lon0 + 0.003, lon0 + 0.004), lat = c(lat0 + 0.002, lat0 + 0.003), osm_type = "W", osm_id = c(1, 2))

test_that("SG1 place suggestions appear after enough text, only while the switch is on, and a pick looks the exact object up", {
  look <- tibble(label = "Elkins, Randolph County, West Virginia, United States", lat = lat0, lon = lon0, boundary = list(NULL), south = lat0 - .05, north = lat0 + .05, west = lon0 - .05, east = lon0 + .05)
  calls <- with_stubs(NULL, photon = list(place = pl_items, address = ad_items), lookup = look); got <- list()
  testServer(env$server, {
    session$setInputs(suggest = TRUE, query = "El", typed = list(kind = "place", text = "El", t = 1)); got$short <<- is.null(sugg$place)
    Sys.sleep(0.45); session$setInputs(typed = list(kind = "place", text = "Elk", t = 2)); got$n <<- nrow(sugg$place$items)
    got$html <<- as.character(output$place_sugg$html)
    session$setInputs(pick = list(kind = "place", i = 1, t = 3)); session$flushReact()
    got$cleared <<- is.null(sugg$place); got$places <<- places()
    Sys.sleep(0.45); session$setInputs(suggest = FALSE, typed = list(kind = "place", text = "Elkin", t = 4)); got$off <<- length(calls$photon)
    session$setInputs(suggest = TRUE, typed = list(kind = "bogus", text = "Elkin", t = 5)); got$bogus <<- length(calls$photon)
    session$setInputs(pick = list(kind = "place", i = 99, t = 6)); got$badpick <<- length(calls$lookup)
  })
  expect_true(got$short); expect_equal(got$n, 2)
  expect_match(got$html, 'role="listbox"', fixed = TRUE); expect_match(got$html, 'data-kind="place"', fixed = TRUE); expect_match(got$html, "Elkins Park", fixed = TRUE)
  expect_equal(calls$lookup[[1]]$type, "R"); expect_equal(calls$lookup[[1]]$id, 123456); expect_length(calls$geo, 0)       # an exact lookup, not a name search
  expect_true(got$cleared); expect_equal(got$places$label, look$label)
  expect_equal(got$off, 1); expect_equal(got$bogus, 1); expect_equal(got$badpick, 1)
})

test_that("UI1 the chosen place is shown as a card, a list of matches appears only when there are several, and a clicked suggestion is not suggested again", {
  look <- tibble(label = "Elkins, Randolph County, West Virginia, United States", lat = lat0, lon = lon0, boundary = list(NULL), south = lat0 - .05, north = lat0 + .05, west = lon0 - .05, east = lon0 + .05)
  calls <- with_stubs(NULL, photon = list(place = pl_items, address = ad_items), lookup = look); got <- list()
  testServer(env$server, {
    session$flushReact(); got$none <<- output$has_place
    Sys.sleep(0.45); session$setInputs(suggest = TRUE, query = "Elk", typed = list(kind = "place", text = "Elk", t = 1)); n1 <<- length(calls$photon)
    session$setInputs(pick = list(kind = "place", i = 1, t = 2)); session$flushReact()
    got$card1 <<- as.character(output$place_card$html); got$has1 <<- output$has_place
    Sys.sleep(0.45); session$setInputs(query = look$label, typed = list(kind = "place", text = look$label, t = 3)); got$again <<- length(calls$photon) - n1      # the box holds the clicked text
    session$setInputs(query = "Elkins", find = 1); session$flushReact()
    got$card2 <<- as.character(output$place_card$html); got$sugg_after_find <<- is.null(sugg$place)
  })
  expect_equal(got$none, "no"); expect_equal(got$has1, "yes")
  expect_match(got$card1, "place-card", fixed = TRUE); expect_match(got$card1, "Elkins", fixed = TRUE); expect_match(got$card1, "Randolph County", fixed = TRUE)
  expect_false(grepl('id="choice"', got$card1, fixed = TRUE))                               # one match: nothing to choose
  expect_equal(got$again, 0)                                                                # no new suggestions for the text just picked
  expect_match(got$card2, 'id="choice"', fixed = TRUE); expect_match(got$card2, "Other matches", fixed = TRUE)    # a name search with two matches offers the list
  expect_true(got$sugg_after_find)
  ui <- as.character(env$ui(NULL)); expect_match(ui, "Choose a place", fixed = TRUE); expect_match(ui, "Settings and privacy", fixed = TRUE); expect_match(ui, "Explore this map", fixed = TRUE)
  expect_equal(lengths(regmatches(ui, gregexpr('id="units"', ui, fixed = TRUE))), 1L)       # one units control
})

test_that("SG2 a place pick whose lookup fails falls back to a name search with the suggestion's own text", {
  calls <- with_stubs(NULL, photon = list(place = pl_items), lookup = NULL); got <- NULL
  testServer(env$server, {
    session$setInputs(suggest = TRUE, typed = list(kind = "place", text = "Elkins", t = 1)); session$setInputs(pick = list(kind = "place", i = 2, t = 2)); session$flushReact()
    got <<- places()
  })
  expect_equal(calls$geo, pl_items$label[2]); expect_equal(nrow(got), 2)
})

test_that("SG3 address suggestions need the switch and a found place, search inside that place's box, and a pick centers the map without another lookup", {
  p_bb <- c(lat0 - .05, lat0 + .05, lon0 - .05, lon0 + .05)
  calls <- with_stubs(NULL, photon = list(place = pl_items, address = ad_items)); got <- list()
  testServer(env$server, {
    session$setInputs(suggest = TRUE, use_addr = TRUE, query = "Elkins", find = 1, choice = "1")
    session$setInputs(typed = list(kind = "address", text = "401 Dav", t = 1)); got$n <<- nrow(sugg$address$items)
    got$html <<- as.character(output$addr_sugg$html)
    session$setInputs(pick = list(kind = "address", i = 2, t = 2), addr = "401 Davis Street, Elkins, West Virginia")
    got$status <<- as.character(output$addr_status$html); got$addr_cleared <<- is.null(sugg$address)
    session$setInputs(radius = 3, units = "km", area = "circle", all_streets = FALSE, draw = 1)
    got$lon <<- geo()$lon; got$lat <<- geo()$lat; got$txt <<- geo()$center_txt
    session$setInputs(addr = "401 Davis Street, Elkins, West Virginia 2"); got$ignored <<- geo()$stale        # edited after the pick: the pick no longer applies
  })
  expect_equal(got$n, 2); expect_match(got$html, 'data-kind="address"', fixed = TRUE)
  expect_equal(calls$photon[[1]]$kind, "address"); expect_length(calls$photon[[1]]$near, 4)
  expect_match(got$status, "Selected: 401 Davis Street"); expect_true(got$addr_cleared)       # the list closes when an address is picked
  expect_equal(c(got$lon, got$lat), c(ad_items$lon[2], ad_items$lat[2])); expect_match(got$txt, "401 Davis Street")
  expect_length(calls$addr, 0)                                                     # no geocoding call: the suggestion already had the position
  expect_match(got$ignored, "new address")
  calls2 <- with_stubs(NULL, photon = list(address = ad_items)); n2 <- NULL
  testServer(env$server, { session$setInputs(suggest = TRUE, use_addr = FALSE, query = "Elkins", find = 1); session$setInputs(typed = list(kind = "address", text = "401 Dav", t = 1)); n2 <<- length(calls2$photon) })
  expect_equal(n2, 0)                                                              # address suggestions only while "Center on a street address" is on
})

test_that("SG4 an address is searched inside the place first and wider only when nothing is there, and the page says which", {
  seen <- list()
  fn <- function(q, bbox, limit, bounded) { seen[[length(seen) + 1]] <<- bounded; if (isTRUE(bounded)) NULL else ad_items[1, 1:3] }
  calls <- with_stubs(NULL, addr = fn); got <- list()
  testServer(env$server, {
    session$setInputs(query = "Elkins", find = 1, choice = "1", use_addr = TRUE, addr = "401 Davis Ave"); session$setInputs(find_addr = 1)
    got$html <<- as.character(output$addr_status$html)
  })
  expect_equal(unlist(seen), c(TRUE, FALSE)); expect_match(got$html, "Nothing matched inside"); expect_match(got$html, "401 Davis Avenue")
  seen <- list(); fn2 <- function(q, bbox, limit, bounded) { seen[[length(seen) + 1]] <<- bounded; ad_items[1, 1:3] }
  calls <- with_stubs(NULL, addr = fn2)
  testServer(env$server, { session$setInputs(query = "Elkins", find = 1, choice = "1", use_addr = TRUE, addr = "401 Davis Ave"); session$setInputs(find_addr = 1); got$html2 <<- as.character(output$addr_status$html) })
  expect_equal(unlist(seen), TRUE); expect_false(grepl("Nothing matched", got$html2))   # found inside: one lookup, no warning
})

test_that("SG5 photon_search keeps places or addresses as asked, builds readable labels, sends the box as west,south,east,north, and tolerates failure", {
  feat <- function(name, key, value, type, id, xy, ...) list(geometry = list(coordinates = as.list(xy)), properties = c(list(name = name, osm_key = key, osm_value = value, osm_type = type, osm_id = id), list(...)))
  body <- list(features = list(feat("Elkins", "place", "city", "R", 11, c(-79.8, 38.9), county = "Randolph County", state = "West Virginia", country = "United States"),
                               feat("Davis Avenue", "highway", "residential", "W", 22, c(-79.81, 38.91), street = "Davis Avenue", housenumber = "401", city = "Elkins", state = "West Virginia"),
                               feat("Elkins Diner", "amenity", "restaurant", "N", 33, c(-79.82, 38.92), street = "Main St", city = "Elkins")))
  seen <- NULL
  mock <- function(req) { seen <<- req$url; httr2::response_json(body = body) }
  pl <- httr2::with_mocked_responses(mock, env$photon_search("Elk", "place"))
  # round 16: a place search also returns addresses, flagged, so one box takes either
  expect_equal(pl$label[1], "Elkins, Randolph County, West Virginia, United States"); expect_equal(pl$osm_type[1], "R"); expect_equal(pl$osm_id[1], 11); expect_equal(c(pl$lon[1], pl$lat[1]), c(-79.8, 38.9))
  expect_equal(pl$is_addr, c(FALSE, TRUE, TRUE)); expect_equal(pl$where[1], ""); expect_match(pl$where[2], "Elkins, West Virginia", fixed = TRUE)
  ad <- httr2::with_mocked_responses(mock, env$photon_search("401 Dav", "address", c(38.8, 39.0, -79.9, -79.7)))
  expect_equal(ad$label, c("401 Davis Avenue, Elkins, West Virginia", "Main St, Elkins")); expect_match(seen, "bbox=-79.90000%2C38.80000%2C-79.70000%2C39.00000", fixed = TRUE)
  expect_null(httr2::with_mocked_responses(function(req) httr2::response(status_code = 503), env$photon_search("Elk", "place")))
  expect_null(httr2::with_mocked_responses(function(req) httr2::response_json(body = list(features = list())), env$photon_search("Elk", "place")))
})

test_that("SG6 geocode_lookup asks for one exact object and refuses anything that is not N, W, or R with a number", {
  seen <- NULL
  r <- httr2::with_mocked_responses(function(req) { seen <<- req$url; httr2::response_json(body = list(list(display_name = "Elkins, WV", lat = "38.9", lon = "-79.8", boundingbox = list("38.8", "39.0", "-79.9", "-79.7")))) }, env$geocode_lookup("R", 123456))
  expect_match(seen, "osm_ids=R123456", fixed = TRUE); expect_equal(r$label, "Elkins, WV"); expect_equal(c(r$south, r$north, r$west, r$east), c(38.8, 39.0, -79.9, -79.7))
  expect_null(env$geocode_lookup("X", 1)); expect_null(env$geocode_lookup("R", NA)); expect_null(env$geocode_lookup(NULL, 1))
})

# ---- the color-vision option is gone, and the side panel and typeahead behave ---------------------------------------------
test_that("CV0 there is no color-vision option: styles are plain names, hostile values fall back, and links and saved lines carry no mode", {
  expect_false(exists("cvd_palettes", envir = env)); expect_false(exists("cvd_modes", envir = env))
  expect_equal(env$get_style_name("classic"), "classic"); expect_equal(env$get_style_name("dusk"), "dusk")
  expect_equal(env$get_style_name("classic:deutan"), "classic"); expect_equal(env$get_style_name("<b>x</b>"), "classic"); expect_equal(env$get_style_name(NULL), "classic"); expect_equal(env$get_style_name(NA), "classic")
  expect_identical(env$get_style("dusk"), env$styles$dusk); expect_identical(env$get_style("nonsense"), env$styles$classic)
  ui <- as.character(env$ui(NULL)); expect_false(grepl('id="cvd"', ui, fixed = TRUE)); expect_false(grepl("Color vision", ui, fixed = TRUE))
  p <- env$share_params(list(query = "Elkins", view = "speed", units = "km", radius = 3, style = "dusk", cvd = "deutan", area = "circle"), 1)
  expect_false("cv" %in% names(p)); expect_null(env$parse_share("?q=Elkins&cv=tritan&s=dusk")$cv); expect_equal(env$parse_share("?q=Elkins&cv=tritan&s=dusk")$s, "dusk")
})

# ---- Overmaps, one search box, block length, automatic downloads, resizable side panel -------------------------------------
test_that("R16a looks_like_address needs a leading house number and city_of_label returns the place an address lies in", {
  yes <- c("401 Davis Avenue, Elkins", "12B Main St", "  7 Elm Street Springfield", "1600 Pennsylvania Ave NW, Washington")
  no <- c("Elkins, West Virginia", "Davis Avenue", "Elkins", "", "10001", "5, Paris", "123")
  for (q in yes) expect_true(env$looks_like_address(q), info = q)
  for (q in no) expect_false(env$looks_like_address(q), info = q)
  expect_equal(env$city_of_label("401 Davis Avenue, Elkins, Randolph County, West Virginia"), "Elkins, Randolph County, West Virginia")      # house number and street in one part
  expect_equal(env$city_of_label("Elkins, Randolph County, West Virginia"), "Randolph County, West Virginia")
  expect_equal(env$city_of_label(""), "")
})

test_that("R16b the app is named Overmaps and offers six views; Block length has km and mile scales", {
  ui <- as.character(env$ui(NULL))
  expect_true(grepl("Overmaps", ui, fixed = TRUE)); expect_false(grepl("Street Maps", ui, fixed = TRUE)); expect_false(grepl("Königsberg Maps", ui, fixed = TRUE))
  expect_length(env$views, 6); expect_true("blocks" %in% names(env$views)); expect_equal(names(env$view_blurbs), names(env$views))
  old <- env$unit_env$u; withr::defer(env$unit_env$u <- old)
  env$unit_env$u <- "km"; sk <- env$block_scheme(); expect_equal(sk$short, "200 m"); expect_equal(length(sk$labels), 4)
  expect_equal(as.character(env$block_class(c(50, 150, 300, 900))), sk$labels)
  env$unit_env$u <- "mi"; sm <- env$block_scheme(); expect_equal(sm$short, "600 ft"); expect_match(sm$labels[1], "ft")
  expect_equal(as.character(env$block_class(c(50, 150, 300, 900))), sm$labels)
  expect_false(any(grepl("[\u2013\u2014]", c(env$views, env$view_blurbs), perl = TRUE)))
})

test_that("R16c a typed address in the one box finds its town, then centers the map on the address; a plain town clears it", {
  adr <- tibble(label = "401 Davis Avenue, Elkins, Randolph County, West Virginia", lat = lat0 + 0.002, lon = lon0 + 0.002)
  calls <- with_stubs(NULL, boundary = st_transform(sq_shape(2500), 4326), addr = adr); got <- list()
  testServer(env$server, {
    session$setInputs(query = "401 Davis Avenue, Elkins", find = 1, choice = "1"); session$flushReact()
    got$q <<- calls$geo; got$p <<- places()$label[1]; got$flag <<- isTRUE(input$use_addr)
    session$setInputs(use_addr = TRUE, addr = "401 Davis Avenue, Elkins, Randolph County, West Virginia"); session$flushReact()
    session$setInputs(query = "Elkins", find = 2); session$flushReact()
    got$p2 <<- places()$label[1]; got$all <<- calls$geo
  })
  expect_equal(got$q[1], "Elkins, Randolph County, West Virginia")                   # the address was turned into its town for the outline lookup
  expect_match(got$p, "Elkins"); expect_equal(calls$addr[[1]]$q, "401 Davis Avenue, Elkins"); expect_match(got$p2, "Elkins")
  expect_equal(got$all[2], "Elkins")                                                   # a plain town goes straight to the place lookup
  calls <- with_stubs(NULL, addr = NULL); testServer(env$server, { session$setInputs(query = "Elkins", find = 1) }); expect_length(calls$addr, 0)
})

test_that("R16d the address controls are hidden behind the one box, the side panel is resizable, and the radius change reloads on its own", {
  ui <- as.character(env$ui(NULL))
  expect_match(ui, 'class="[^"]*addr-internal', perl = TRUE); expect_false(grepl("Center on a street address", ui, fixed = TRUE))
  expect_match(ui, "bslib-sidebar-layout", fixed = TRUE); expect_match(ui, "resizable|collapse-toggle", perl = TRUE)
})

# ---- Explore history, the Roman roads --------------------------------------------------------------------------------------
test_that("H1 the bundled Itiner-e roads load with kind, evidence, slope, and travel times, and every region has roads", {
  r <- env$roman_roads(); expect_s3_class(r, "sf"); expect_gt(nrow(r), 14000)
  expect_true(all(c("kind", "status", "slope", "d_foot_road", "d_foot_sea", "d_cart_road", "d_courier_sea") %in% names(r)))
  expect_setequal(unique(r$kind), c("main", "secondary")); expect_setequal(unique(r$status), c("certain", "conjectured", "hypothetical"))
  expect_gt(mean(!is.na(r$d_foot_road)), 0.95); expect_true(all(r$d_foot_sea <= r$d_foot_road + 0.2, na.rm = TRUE))      # the sea never makes a trip longer
  expect_equal(sum(as.numeric(st_length(r))) / 1000, 294000, tolerance = 0.03)
  for (k in names(env$roman_regions)) { d <- env$roman_data(k, "kind"); expect_false(is.null(d), info = k); expect_gt(d$n, 50) }
  expect_gt(env$roman_data("empire")$total_km, env$roman_data("italy")$total_km)
  expect_equal(unname(env$roman_regions$empire$bb), c(-10, 22, 42, 56))
})

test_that("H2 each view classes the roads into the right buckets, bad input falls back, and the numbers add up", {
  e <- env$roman_data("empire", "evidence"); expect_equal(e$certain_pct + e$conj_pct + e$hyp_pct, 100, tolerance = 1e-6); expect_gt(e$conj_pct, 80)
  expect_equal(levels(e$roads$cls), c("Certain", "Conjectured", "Hypothetical"))
  k <- env$roman_data("empire", "kind"); expect_equal(sum(table(k$roads$cls)), k$n); expect_equal(levels(k$roads$cls), c("Main road", "Secondary road"))
  s <- env$roman_data("empire", "slope"); expect_equal(length(levels(s$roads$cls)), 5); expect_false(anyNA(s$roads$cls))
  dd <- env$roman_data("empire", "days", "foot", FALSE); expect_equal(levels(dd$roads$cls), c("Under 15", "15 to 30", "30 to 60", "60 to 120", "120 or more"))
  expect_gt(dd$days_med, env$roman_data("empire", "days", "foot", TRUE)$days_med)      # the sea saves time
  expect_gt(dd$days_med, env$roman_data("empire", "days", "courier", FALSE)$days_med)  # a courier is faster than walking
  expect_gt(dd$days_med, env$roman_data("empire", "days", "cart", FALSE)$days_med)
  expect_equal(unname(table(dd$roads$cls)[1] > 0), TRUE)
  rome <- env$roman_data("empire", "days", "foot", FALSE)$roads; near <- rome[!is.na(rome$d_foot_road) & rome$d_foot_road < 15, ]; expect_gt(nrow(near), 10)
  bad <- env$roman_data("<script>", "nonsense", "teleport", NA); expect_equal(c(bad$region, bad$view, bad$how, bad$sea), c("empire", "kind", "foot", "FALSE"))
})

test_that("H3 the maps draw in every view and style, the text is plain and accurate, and nothing says more than the data does", {
  for (v in names(env$roman_views)) for (st in names(env$styles)) {
    d <- env$roman_data("britain", v, "cart", TRUE); p <- env$roman_map(d, st)
    f <- tempfile(fileext = ".png"); env$draw_figure(p, f, "png", st, preview = TRUE); expect_gt(file.size(f), 20000, label = paste(v, st))
  }
  txt <- c(unlist(lapply(names(env$roman_regions), function(k) unlist(lapply(names(env$roman_views), function(v) { d <- env$roman_data(k, v); c(env$roman_words(d), env$roman_alt(d), env$roman_caption(d)) })))), env$roman_views, env$roman_view_blurbs)
  expect_false(any(grepl("[–—]", txt))); expect_false(any(grepl("\\b[A-Za-z]+n't\\b|\\b(it|that|there)'s\\b", txt)))
  expect_match(env$roman_words(env$roman_data("britain", "evidence")), "certain, ")
  expect_equal(env$roman_pct(0.2), "under 1%"); expect_equal(env$roman_pct(0), "0%"); expect_equal(env$roman_pct(47.4), "47%")
  expect_match(env$roman_words(env$roman_data("empire", "kind")), "across the whole empire"); expect_match(env$roman_words(env$roman_data("italy", "kind")), "in Italy")
  expect_match(env$roman_caption(env$roman_data("empire", "days")), "Hotham"); expect_false(grepl("Hotham", env$roman_caption(env$roman_data("empire", "kind"))))
  expect_match(env$roman_caption(env$roman_data("empire", "kind")), "CC BY 4.0")
  expect_false(is.null(env$history_file("itinere_simplified.rds"))); expect_null(env$history_file("no_such_file.rds"))
})

test_that("H4 the Explore history window opens from the header, draws a map, and downloads a real file", {
  ui <- as.character(env$ui(NULL)); expect_match(ui, 'id="history_open"', fixed = TRUE); expect_match(ui, "Explore history", fixed = TRUE)
  m <- as.character(env$history_modal("classic")); for (id in c("h_region", "h_view", "h_how", "h_sea", "h_style", "h_format", "h_download", "h_map")) expect_match(m, paste0('id="', id, '"'), fixed = TRUE)
  expect_match(m, "Itiner-e", fixed = TRUE); expect_match(m, "CC BY 4.0", fixed = TRUE)
  shown <- NULL; out <- list()
  testthat::local_mocked_bindings(showModal = function(ui, ...) { shown <<- as.character(ui); invisible() }, .package = "shiny")
  testServer(env$server, {
    session$setInputs(history_open = 1); session$flushReact(); out$title <<- output$h_title
    session$setInputs(h_region = "britain", h_view = "days", h_how = "courier", h_sea = FALSE, h_style = "dusk", h_format = "png"); session$flushReact()
    out$blurb <<- output$h_blurb; img <- output$h_map; out$src <<- img$src; out$alt <<- img$alt
    p <- output$h_download; out$dl <<- basename(p); out$ok <<- file.exists(p) && file.size(p) > 20000 && identical(readBin(p, "raw", 4)[2:4], charToRaw("PNG"))
    session$setInputs(h_format = "pdf"); out$pdf <<- basename(output$h_download); session$setInputs(h_format = "../../etc/passwd"); out$bad <<- basename(output$h_download)
  })
  expect_match(out$title, "^Explore history: The roads of the Roman Empire$"); expect_match(out$blurb, "How long it took"); expect_match(out$alt, "Britain"); expect_true(out$ok)
  expect_match(out$dl, "^roman_roads_britain_days_[0-9]{8}\\.png$"); expect_match(out$pdf, "\\.pdf$"); expect_match(out$bad, "\\.png$")
})


# ---- house addresses in Photon, the Census fallback, a window that can fill the screen, and ancient city plans ------------------------
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
test_that("R18e the history window has a full-screen button, a title that follows the tab, and the same pickers as the main page", {
  m <- as.character(env$history_modal("classic"))
  for (id in c("h_tab", "h-full-btn", "c_site", "c_view", "c_style", "c_draw", "c_format", "c_download", "c_map", "h_style", "h_view")) expect_match(m, paste0('id="', id, '"'), fixed = TRUE)
  expect_match(m, "overmapsHistoryFull", fixed = TRUE); expect_match(m, "Full screen", fixed = TRUE); expect_match(m, "Exit full screen", fixed = TRUE)
  expect_equal(lengths(regmatches(m, gregexpr("Choose a look", m, fixed = TRUE))), 2L); expect_equal(lengths(regmatches(m, gregexpr('class="style-picker"', m, fixed = TRUE))), 2L)
  expect_equal(lengths(regmatches(m, gregexpr('class="view-picker"', m, fixed = TRUE))), 2L); expect_false(grepl("selectInput.*h_style", m)); expect_false(grepl('id="h_style" class="shiny-input-select', m, fixed = TRUE))
  expect_equal(env$history_title("roads"), "Explore history: The roads of the Roman Empire"); expect_equal(env$history_title(NULL), "Explore history: The roads of the Roman Empire")
  expect_equal(env$history_title("cities"), "Explore history: Streets of ancient cities")
  expect_match(env$app_js, "history-full", fixed = TRUE); expect_match(env$app_css, "modal-dialog.history-full", fixed = TRUE)
  expect_false(grepl("the roads of the Roman Empire", as.character(env$ui(NULL)), fixed = TRUE))      # the old lower-case wording is gone everywhere
})
test_that("R18f the ancient sites are well formed and grouped by civilization", {
  s <- env$ancient_sites
  expect_equal(anyDuplicated(s$id), 0L); expect_true(all(abs(s$lat) <= 90 & abs(s$lon) <= 180)); expect_true(all(s$radius_km >= 0.3 & s$radius_km <= 3))
  expect_true(all(nzchar(s$note)) && all(grepl("\\.$", s$note))); expect_true(all(grepl(", [A-Z]", s$name))); expect_true(all(unlist(env$ancient_site_choices) %in% s$id))
  expect_gte(length(unique(s$civ)), 8); expect_false(any(grepl("[–—]", c(s$note, s$name)))); expect_false(any(grepl("n't|'s |'re|'ll", s$note)))
  expect_equal(env$ancient_site("pompeii")$name, "Pompeii, Italy"); expect_null(env$ancient_site("nowhere")); expect_null(env$ancient_site(NULL)); expect_null(env$ancient_site(c("a", "b")[0]))
  expect_equal(env$get_city_view("blocks"), "blocks"); expect_equal(env$get_city_view("hierarchy"), "direction"); expect_equal(env$get_city_view(NULL), "direction")
})
test_that("R18g an ancient city is downloaded once with its own street classes, drawn in each map type, reports coverage, and downloads", {
  withr::defer(env$ancient_sites <- orig_sites); orig_sites <- env$ancient_sites
  env$ancient_sites <- dplyr::bind_rows(orig_sites, tibble(id = "test", name = "Testville, Elkins", civ = "Roman", lat = lat0, lon = lon0, radius_km = 1.5, note = "A fixture."))
  calls <- with_stubs(NULL); out <- list()
  testServer(env$server, {
    session$setInputs(history_open = 1, h_tab = "cities", c_site = "test", c_view = "blocks", c_style = "dusk", c_format = "png"); session$flushReact()
    out$title <<- output$h_title; out$before <<- as.character(output$c_status$html); out$has0 <<- output$c_has_map
    session$setInputs(c_draw = 1); session$flushReact()
    out$status <<- as.character(output$c_status$html); out$has <<- output$c_has_map; out$note <<- as.character(output$c_note$html)
    img <- output$c_map; out$alt <<- img$alt; out$ok <<- is.character(img$src) && nzchar(img$src)
    session$setInputs(c_view = "density"); session$flushReact(); out$alt2 <<- output$c_map$alt; session$setInputs(c_view = "direction"); out$alt3 <<- output$c_map$alt
    p <- output$c_download; out$dl <<- basename(p); out$file <<- file.exists(p) && file.size(p) > 20000
    session$setInputs(c_format = "pdf"); out$pdf <<- basename(output$c_download); session$setInputs(c_format = "../x"); out$bad <<- basename(output$c_download)
    session$setInputs(c_site = "pompeii"); session$flushReact(); out$stale <<- as.character(output$c_status$html)
  })
  expect_equal(out$title, "Explore history: Streets of ancient cities"); expect_match(out$before, "press Draw map", ignore.case = TRUE); expect_equal(out$has0, "no")
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
  testServer(env$server, { session$setInputs(history_open = 1, h_tab = "cities", c_site = "test", c_draw = 1); session$flushReact(); out$has <<- output$c_has_map; out$status <<- as.character(output$c_status$html) })
  expect_equal(out$has, "no"); expect_true(any(grepl("all servers busy", shown))); expect_match(out$status, "press Draw map", ignore.case = TRUE)
  with_stubs(NULL); env$fetch_roads <- function(...) list(osm_lines = NULL)
  testServer(env$server, { session$setInputs(history_open = 1, h_tab = "cities", c_site = "test", c_draw = 1); session$flushReact(); out$has2 <<- output$c_has_map; out$status2 <<- as.character(output$c_status$html) })
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
  testServer(env$server, { session$setInputs(history_open = 1, h_tab = "cities", c_site = "test", c_draw = 1); session$flushReact()
    out$has <<- output$c_has_map; out$status <<- as.character(output$c_status$html) })
  expect_length(calls$fetch, 0); expect_equal(out$has, "yes"); expect_match(out$status, "snapshot taken November 13, 2026")
  env$history_cache$cities <- list(test = list(roads = fixture_roads(), taken = "November 13, 2026", radius_m = 900L))
  calls <- with_stubs(NULL)
  testServer(env$server, { session$setInputs(history_open = 1, h_tab = "cities", c_site = "test", c_draw = 1); session$flushReact(); out$status2 <<- as.character(output$c_status$html) })
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

test_that("R19c the full text of a failed request keeps the cause curl reported, once, and a refused connection is classed as unreachable", {
  parent <- structure(class = c("error", "condition"), list(message = "Could not connect to server", call = NULL))
  e <- structure(class = c("rlang_error", "error", "condition"), list(message = "Failed to perform HTTP request.", call = NULL, parent = parent))
  txt <- env$error_text(e)
  expect_match(txt, "Failed to perform HTTP request", fixed = TRUE)
  expect_equal(lengths(regmatches(txt, gregexpr("Could not connect to server", txt))), 1L)
  expect_equal(env$explain_failure(paste("status check failed:", txt))[1], "unreachable")
})

# ---- Release: version, copy, and writing gates ---------------------------------------------------------------------------------
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

test_that("REL5 the documents state the real counts of ancient sites and civilizations", {
  expect_equal(nrow(env$ancient_sites), 15L); expect_equal(length(unique(env$ancient_sites$civ)), 11L)
  expect_match(read_root("README.md"), "fifteen ancient cities"); expect_match(read_root("README.md"), "11 civilizations")
  expect_match(read_root("CHANGELOG.md"), "11 civilizations")
})

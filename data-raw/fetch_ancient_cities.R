# Saves a snapshot of the OpenStreetMap streets, water, parks and street lamps at every ancient site to data/ancient_cities.rds, so the
# Ancient cities tab draws at once and does not depend on the public Overpass servers being free.
#
# The public servers block a client that asks too often, so the script asks once per site, then rests between passes over the sites that failed.
# It stops by itself after four passes. If many sites still fail, wait about half an hour and run it again.
#
# Run from the project root:  Rscript data-raw/fetch_ancient_cities.R
# Sites already in the file are skipped. Run it again to fill in any that failed, or add --refresh to download every site anew.
# The saved streets are OpenStreetMap data, so keep the credit that data/README.md gives.

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
if (!file.exists("app.R")) stop("Run this script from the project root, the folder that holds app.R.")
suppressMessages({ library(sf); library(dplyr) })
env <- new.env(parent = globalenv())
suppressMessages(sys.source("app.R", envir = env))

out <- file.path("data", "ancient_cities.rds")
bundle <- if (file.exists(out) && !refresh) readRDS(out) else list()
rounds <- 4            # passes over the sites that still fail
cool_down_s <- as.numeric(Sys.getenv("OVERMAPS_COOLDOWN_S", "300"))     # rest between passes, so the public servers can recover
failed <- character()

# One site: streets, then water, parks and lamps. Returns TRUE when both are saved. A single try per request, with no long waiting inside a site:
# a server that is busy or blocking this computer is far more likely to answer after a rest between passes than after another minute of asking.
force_ids <- trimws(strsplit(Sys.getenv("OVERMAPS_REFETCH", ""), ",")[[1]])   # sites to download again even though they are saved
fetch_site <- function(s, pass = 1L) {
  # Streets are saved out to about twice the listed radius (never past 3 km), so the app can see where the remains really end and fit the starting radius to them.
  radius_m <- round(min(3, max(2 * s$radius_km, s$radius_km + 0.5)) * 1000)
  # A very dense area (a big modern city, or many tiny mapped features) can be too large for the servers to send. Each later pass asks for a smaller area,
  # never smaller than the listed radius plus a margin.
  radius_m <- max(round(radius_m * c(1, 0.75, 0.55, 0.4)[min(pass, 4L)]), round((s$radius_km + 0.2) * 1000))
  have <- bundle[[s$id]]
  floor_m <- round((s$radius_km + 0.2) * 1000)
  # A snapshot saved at a smaller radius after a dense area failed counts as done, so a rerun does not ask for the large area again.
  covers <- function(m) is.numeric(m) && length(m) == 1 && m >= floor_m && m <= round(min(3, max(2 * s$radius_km, s$radius_km + 0.5)) * 1000)
  same <- is.list(have) && covers(have$radius_m)
  need_roads <- !same || s$id %in% force_ids
  need_extras <- need_roads || !(same && is.list(have$extras) && covers(have$extras$radius_m))
  if (!need_roads && !need_extras) return(NA)                      # nothing to do
  if (s$id %in% force_ids) {      # the app keeps recent downloads, so a site asked for again must not be answered from that folder
    old <- list.files(env$roads_cache_dir(), pattern = sprintf("^roads_%.4f_%.4f_", s$lon, s$lat), full.names = TRUE)
    if (length(old)) invisible(file.remove(old))
  }
  crs <- env$utm_epsg(s$lon, s$lat)
  one_try <- function(fun) tryCatch(fun(), error = function(e) { message("  ", substr(conditionMessage(e), 1, 160)); NULL })

  if (need_roads) {
    message("Downloading the streets of ", s$name, " ...")
    got <- one_try(function() env$fetch_roads(s$lon, s$lat, radius_m, crs, NULL, detail = "ancient"))
    if (is.null(got)) return(FALSE)
    lines <- got$osm_lines
    if (!is.null(lines) && nrow(lines) > 0) lines <- lines[, intersect(c("osm_id", "name", "highway", "bridge", "tunnel", "oneway"), names(lines))]
    bundle[[s$id]] <<- list(roads = list(osm_lines = lines), taken = format(Sys.Date(), "%B %d, %Y"), radius_m = radius_m)
    dir.create("data", showWarnings = FALSE); saveRDS(bundle, out)
    Sys.sleep(5)
  }
  # Water, parks, street lamps and lit streets, in one request, for the Nature & water and Street lights map types.
  message("Downloading the water, parks and lamps of ", s$name, " ...")
  ex <- one_try(function() env$fetch_roads(s$lon, s$lat, radius_m, crs, NULL, detail = "site"))
  if (is.null(ex)) return(FALSE)
  ex$radius_m <- radius_m
  bundle[[s$id]]$extras <<- ex
  saveRDS(bundle, out)                      # saved after every site, so a stopped run keeps its progress
  Sys.sleep(5)
  TRUE
}

todo <- seq_len(nrow(env$ancient_sites))
for (r in seq_len(rounds)) {
  if (r > 1) { message("\nPass ", r, " of ", rounds, ": ", length(todo), " site(s) left. Resting ", cool_down_s %/% 60, " minutes first ..."); Sys.sleep(cool_down_s) }
  still <- integer(); streak <- 0L
  for (i in todo) {
    s <- as.list(env$ancient_sites[i, ])
    ok <- fetch_site(s, r)
    if (isFALSE(ok)) {
      still <- c(still, i); streak <- streak + 1L
      if (streak >= 5L) { message("Five sites in a row failed, so the servers are refusing this computer for now. Moving to the rest period."); still <- c(still, setdiff(todo[todo > i], still)); break }
    } else streak <- 0L
  }
  todo <- still
  if (!length(todo)) break
}
failed <- if (length(todo)) env$ancient_sites$name[todo] else character()

n_ok <- sum(vapply(bundle, is.list, logical(1)))
message("\nSaved ", n_ok, " of ", nrow(env$ancient_sites), " sites in ", out, ".")

# What was found at each site, so a site that is empty or sits in the wrong place can be spotted and fixed in ancient_sites.
sizes <- do.call(rbind, lapply(seq_len(nrow(env$ancient_sites)), function(i) {
  s <- as.list(env$ancient_sites[i, ]); b <- bundle[[s$id]]
  l <- if (is.list(b)) b$roads$osm_lines else NULL
  e <- if (is.list(b)) b$extras else NULL
  n_nat <- if (is.list(e)) sum(vapply(e$nature[c("osm_polygons", "osm_multipolygons", "osm_lines")], function(x) if (is.null(x)) 0L else nrow(x), 0L)) else NA_integer_
  fit <- if (is.null(l)) NA_real_ else env$fit_radius_km(l, s$lon, s$lat, b$radius_m)
  data.frame(site = s$name, listed_km = s$radius_km, fitted_km = fit, streets = if (is.null(l)) 0L else if ("osm_id" %in% names(l)) dplyr::n_distinct(l$osm_id) else nrow(l), nature_features = n_nat,
             lamps = if (is.list(e)) e$n_lamps else NA_integer_, lit_streets = if (is.list(e)) e$n_lit else NA_integer_,
             stringsAsFactors = FALSE)
}))
sizes$street_lights_offered <- !is.na(sizes$lamps) & (sizes$lamps + sizes$lit_streets) >= env$city_min_lights
message("\nWhat each site holds. fitted_km is the starting radius the app will use (NA means too little is mapped, so listed_km stays). Streets under 25 are drawn with a warning that the site is thinly mapped:")
print(sizes, row.names = FALSE)
if (length(failed)) {
  message("\nThese could not be downloaded: ", paste(failed, collapse = ", "), ". Run the script again later.")
  quit(status = 1)
}

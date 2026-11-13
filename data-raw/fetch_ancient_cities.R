# Saves a snapshot of the OpenStreetMap streets at every ancient site to data/ancient_cities.rds, so the
# Ancient cities tab draws at once and does not depend on the public Overpass servers being free.
#
# The public servers block a client that asks too often, so the script waits between tries. If every server refuses, stop it, wait
# about half an hour, and run it again.
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
tries_per_site <- 4
failed <- character()

for (i in seq_len(nrow(env$ancient_sites))) {
  s <- as.list(env$ancient_sites[i, ])
  radius_m <- round(s$radius_km * 1000)
  have <- bundle[[s$id]]
  if (is.list(have) && identical(as.integer(have$radius_m), as.integer(radius_m))) { message("Already saved: ", s$name); next }
  message("Downloading ", s$name, " ...")
  got <- NULL
  for (k in seq_len(tries_per_site)) {
    got <- tryCatch(env$fetch_roads(s$lon, s$lat, radius_m, env$utm_epsg(s$lon, s$lat), NULL, detail = "ancient"),
                    error = function(e) { message("  ", conditionMessage(e)); NULL })
    if (!is.null(got)) break
    if (k < tries_per_site) { message("  Waiting before try ", k + 1, " of ", tries_per_site); Sys.sleep(120 * k) }
  }
  if (is.null(got)) { failed <- c(failed, s$name); next }
  lines <- got$osm_lines
  if (!is.null(lines) && nrow(lines) > 0) lines <- lines[, intersect(c("osm_id", "name", "highway", "bridge", "tunnel", "oneway"), names(lines))]
  bundle[[s$id]] <- list(roads = list(osm_lines = lines), taken = format(Sys.Date(), "%B %d, %Y"), radius_m = radius_m)
  dir.create("data", showWarnings = FALSE)
  saveRDS(bundle, out)                      # saved after every site, so a stopped run keeps its progress
  Sys.sleep(10)
}

message("\nSaved ", sum(vapply(bundle, is.list, logical(1))), " of ", nrow(env$ancient_sites), " sites in ", out, ".")
if (length(failed)) {
  message("These could not be downloaded: ", paste(failed, collapse = ", "), ". Run the script again later.")
  quit(status = 1)
}

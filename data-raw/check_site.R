# Shows what each map server says about one ancient site, so an empty result can be traced to its cause.
# Run from the project root:  Rscript data-raw/check_site.R carthage
# It never reads or writes the saved snapshot or the download folder.

id <- commandArgs(trailingOnly = TRUE)[1]
if (!file.exists("app.R")) stop("Run this script from the project root, the folder that holds app.R.")
if (is.na(id)) stop("Give a site id, for example: Rscript data-raw/check_site.R carthage")
suppressMessages({ library(sf); library(dplyr); library(httr2) })
env <- new.env(parent = globalenv())
suppressMessages(sys.source("app.R", envir = env))
s <- env$ancient_sites[env$ancient_sites$id == id, ]
if (nrow(s) != 1) stop("No site with that id. Ids look like: ", paste(head(env$ancient_sites$id, 6), collapse = ", "), " ...")
cat(sprintf("%s: lon %.4f, lat %.4f, listed radius %.2f km\n", s$name, s$lon, s$lat, s$radius_km))
r <- 1000
d <- r / 111320
bb <- c(s$lon - d / cos(s$lat * pi / 180), s$lat - d, s$lon + d / cos(s$lat * pi / 180), s$lat + d)
q <- sprintf('[out:json][timeout:60];way["highway"](%f,%f,%f,%f);out count;', bb[2], bb[1], bb[4], bb[3])
for (u in env$overpass_mirrors_for(bb)) {
  if (identical(u, env$osm_api_url)) next
  res <- tryCatch({
    resp <- request(u) |> req_body_form(data = q) |> req_timeout(70) |> req_error(is_error = function(x) FALSE) |> req_perform()
    paste0("HTTP ", resp_status(resp), ": ", substr(gsub("\\s+", " ", resp_body_string(resp)), 1, 160))
  }, error = function(e) paste("failed:", conditionMessage(e)))
  cat(sprintf("\n%s\n  %s\n", u, res))
}

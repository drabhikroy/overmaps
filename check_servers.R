# check_servers.R - find out which OpenStreetMap servers your computer can actually reach, and how fast.
# Run from the folder that holds app.R:   Rscript check_servers.R
# It asks each street-data server for its status and for one tiny test query, then asks the place-search service (Nominatim) for
# one address. Nothing is saved. Send the printed table to whoever helps you if every row says FAIL.
suppressPackageStartupMessages(library(httr2))
ua <- "Overmaps (R Shiny app; data from OpenStreetMap)"
servers <- c("https://ovp.dresel.systems/api-na/interpreter", "https://ovp.dresel.systems/api/interpreter",
             "https://overpass-api.de/api/interpreter", "https://overpass.private.coffee/api/interpreter",
             "https://maps.mail.ru/osm/tools/overpass/api/interpreter")
tiny <- "[out:json][timeout:20];way(38.9200,-79.8520,38.9210,-79.8500)[highway];out ids 1;"

timed <- function(req) {
  t0 <- Sys.time()
  r <- tryCatch(req_perform(req_error(req, is_error = function(r) FALSE)), error = function(e) e)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  if (inherits(r, "error")) list(ok = FALSE, secs = secs, note = substr(gsub("\\s+", " ", conditionMessage(r)), 1, 70))
  else list(ok = resp_status(r) == 200, secs = secs, note = if (resp_status(r) == 200) "ok" else sprintf("HTTP %d", resp_status(r)))
}
rows <- lapply(servers, function(u) {
  st <- timed(req_timeout(req_user_agent(request(sub("/interpreter$", "/status", u)), ua), 10))
  q  <- timed(req_timeout(req_body_form(req_user_agent(request(u), ua), data = tiny), 30))
  data.frame(server = sub("^https?://", "", sub("/interpreter$", "", u)), status = if (st$ok) "ok" else "FAIL", status_s = st$secs,
             query = if (q$ok) "ok" else "FAIL", query_s = q$secs, note = if (q$ok) "" else q$note, stringsAsFactors = FALSE)
})
out <- do.call(rbind, rows)
nom <- timed(req_timeout(req_user_agent(request("https://nominatim.openstreetmap.org/search?q=Elkins%20WV&format=json&limit=1"), ua), 15))
out <- rbind(out, data.frame(server = "nominatim (place search)", status = "-", status_s = NA, query = if (nom$ok) "ok" else "FAIL", query_s = nom$secs,
                             note = if (nom$ok) "" else nom$note, stringsAsFactors = FALSE))
print(out, row.names = FALSE)
cat("\n")
if (all(out$query == "FAIL")) cat("Nothing answered. Check your internet connection, VPN, proxy, or firewall (these addresses must be allowed).\n") else
  if (any(out$query[-nrow(out)] == "ok")) cat("At least one street server works. The app will use the working ones; slow or FAIL rows are skipped automatically.\n") else
  cat("Place search works but no street server does. They may be overloaded right now; try again in a few minutes.\n")

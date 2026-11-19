# Builds data/eastern_desert_routes.rds, the small file the app draws the "Egyptian desert" history tab from.
#
# Source: Manière, Crépy and Redon (2020), least-cost camel network of the Egyptian Eastern Desert, Ptolemaic and Roman periods
# (CNRS HiSoMA, Desert Networks project, ERC grant 759078). Zenodo record 4291876, https://doi.org/10.5281/zenodo.4291876.
# These are routes a model computed between known sites and watering places, not roads found on the ground.
#
# Run once, from the repository root, after taking two things out of the 1.8 GB Zenodo zip (no need to unpack all of it):
#   03_Output_Data/03_Postprocess_Cost_Computation_Network/network_cost.{shp,shx,dbf,prj,cpg}
#   01_Input_Data/Network_Site/desert_networks_sites_start.{shp,shx,dbf,prj}
# Put them in one folder and pass it:  Rscript data-raw/build_eastern_desert.R /path/to/folder
suppressMessages(library(sf))
dir <- commandArgs(TRUE)[1]; if (is.na(dir) || !dir.exists(dir)) stop("Give the folder holding network_cost.shp and desert_networks_sites_start.shp")
net <- st_read(file.path(dir, "network_cost.shp"), quiet = TRUE)
sites <- st_read(file.path(dir, "desert_networks_sites_start.shp"), quiet = TRUE)
stopifnot(all(c("cost_meter") %in% names(net)), st_crs(net)$epsg == 32636)
net <- net[as.numeric(st_length(net)) > 1 & net$cost_meter > 0, ]                       # drop zero-length stubs and the unscored links
net <- st_simplify(net, dTolerance = 40, preserveTopology = FALSE)                      # 40 m is far finer than the model's own accuracy
net <- net[!st_is_empty(net), ]
routes <- st_sf(cost_m = round(net$cost_meter, 2), km = round(as.numeric(st_length(net)) / 1000, 3), geometry = st_geometry(net))
routes <- st_transform(routes, 4326)
st_geometry(routes) <- st_sfc(lapply(st_geometry(routes), function(g) { g[] <- round(g, 4); g }), crs = 4326)
pts <- st_transform(sites, 4326)
sites <- st_sf(name = as.character(pts$name), type = as.character(pts$type), geometry = st_geometry(pts))
out <- list(routes = routes, sites = sites,
            source = "Manière, Crépy and Redon (2020), Zenodo 10.5281/zenodo.4291876, CC BY 4.0",
            made = format(Sys.Date()))
saveRDS(out, file.path("data", "eastern_desert_routes.rds"), compress = "xz")
cat(nrow(routes), "segments,", round(sum(routes$km)), "km,", nrow(sites), "sites ->", round(file.size("data/eastern_desert_routes.rds") / 1e6, 2), "MB\n")

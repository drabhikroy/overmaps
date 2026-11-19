# Data

`itinere_simplified.rds` is a simplified copy of the Itiner-e Roman road network, used by the Historical maps page. It is an `sf` table of line geometries in WGS 84 with these columns: `kind` (main or secondary), `status` (certain, conjectured, or hypothetical), `slope`, `name`, and six travel-time columns named `d_*` (days from Rome by foot, cart, and courier, over land and by sea).

Sources:

- Itiner-e: de Soto, P., Pažout, A., Brughmans, T., and Vahlstrup, P. (2024). The Itiner-e Roman road network, static version. CC BY 4.0.
- Hotham, I. *By Road and by Sea*, travel times from Rome. CC BY 4.0.

The geometry was simplified for display speed, so use the original Itiner-e release for measurement. The ancient city plans do not use this file. They are downloaded from OpenStreetMap when requested.

## Egyptian desert routes

`eastern_desert_routes.rds` holds the modeled camel routes for the Egyptian desert tab: a list with `routes` (an `sf` table of line segments in WGS 84 with `cost_m`, the relative travel cost per meter, which has no unit, and `km`) and `sites` (the 457 archaeological sites and watering places the routes join, with `name` and `type`).

These routes are computed, not found. Manière, Crépy and Redon (2020) modeled the cheapest camel route between known sites for the Ptolemaic and Roman periods from terrain, slope and ease of navigation, and calibrated and checked the method against 18th and 19th century travelers' routes. Source: Least cost network data for ancient camel transportation in the Eastern Desert of Egypt, Desert Networks project, CNRS HiSoMA, Zenodo, doi:10.5281/zenodo.4291876, CC BY 4.0. The file was simplified to a 40 m tolerance for display, so use the original for measurement.

To rebuild it, take `network_cost.{shp,shx,dbf,prj,cpg}` and `desert_networks_sites_start.{shp,shx,dbf,prj}` out of the Zenodo zip (about 6 MB of the 1.8 GB), put them in one folder, and run:

```sh
Rscript data-raw/build_eastern_desert.R /path/to/folder
```

## Ancient city snapshot

`ancient_cities.rds`, when present, holds the OpenStreetMap streets, paths, and tracks for each site, plus (in an `extras` entry) its water, woods, parks, street lamps and the ids of lit streets for the Nature & water and Street lights maps, on the Ancient cities tab, so those maps draw at once and do not wait on the public Overpass servers. Each entry records the site, the radius (a smaller radius is cut from it; a larger one downloads live), and the date the snapshot was taken. Build or refresh it from the project root:

```sh
Rscript data-raw/fetch_ancient_cities.R
```

The script skips sites already saved, and for a site saved without its water, parks and lamps it downloads only those, so run it again to fill in any that failed and after new sites are added. It ends with a table of what each site holds (streets, nature features, lamps, lit streets, and whether Street lights is offered), which shows a site that is empty or off center. Add `--refresh` to download every site anew. Without the file, the app downloads a site when you press Draw map.

The streets are copyright OpenStreetMap contributors and available under the Open Database License (<https://www.openstreetmap.org/copyright>).

The fetch script saves each ancient site out to about twice its listed radius (never past 3 km). The app fits the starting radius to the saved streets, so a site saved at the older, smaller radius should be fetched again by running `Rscript data-raw/fetch_ancient_cities.R`.

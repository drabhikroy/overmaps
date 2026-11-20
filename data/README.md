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

## Population estimates for ancient sites

The Ancient cities tab shows a published population estimate for the sites listed below, and for no others. They are estimates from scholarly studies, not counts, and studies of the same site often disagree widely, so the app gives each as the authors did, with the period it describes. The figures live in `ancient_estimates` in `app.R`. A site is added only where a citable published estimate exists.

- Ostia Antica: 22,000 to 58,000 people in the 2nd century CE. The range spans Storey, G. R. (1997), The population of ancient Rome, *Antiquity* 71(274), 966-978 (22,000); Packer, J. E. (1967), Housing and population in imperial Ostia and Rome, *Journal of Roman Studies* 57 (27,000); Calza, G. (1941) (36,000); and Meiggs, R. (1973), *Roman Ostia*, 2nd ed., Oxford University Press (up to 58,000).
- Knossos: about 25,000 people in the Neopalatial period. Cutler, J., and Whitelaw, T. (2019), Neopalatial and Mycenaean Knossos: urban expansion and collapse, Proceedings of the 12th International Congress of Cretan Studies.
- Teotihuacan: about 100,000 people in the Xolalpan and Metepec phases, with earlier estimates from 75,000 to over 200,000. Smith, M. E., Chatterjee, A., Huster, A. C., Stewart, S., and Forest, M. (2019), Apartment compounds, households, and population in the ancient city of Teotihuacan, Mexico, *Ancient Mesoamerica* 30(3), 399-418.
- Angkor: 700,000 to 900,000 people across the whole Greater Angkor Region, about 3,000 square kilometers, at its height in the 13th century CE. This is the region, not the walled city of Angkor Thom. Klassen, S., and others (2021), Diachronic modeling of the population within the medieval Greater Angkor Region settlement complex, *Science Advances* 7(19), eabf8441.
- Great Zimbabwe: no more than 10,000 people at its peak. Chirikure, S., Moultrie, T., Bandama, F., Dandara, C., and Manyanga, M. (2017), What was the population of Great Zimbabwe (CE 1000-1800)?, *PLOS ONE* 12(6), e0178335. The older figure of 18,000 is Huffman's 1986 estimate, as reported by Pikirayi, I. (2013), Great Zimbabwe in historical archaeology, *Historical Archaeology* 47(1), 26-37.

## Census population data

The Population and Housing maps on the Current maps tab are not built from a file in this folder. When one of them is chosen, the app asks the U.S. Census Bureau's TIGERweb service for the 2020 Census block groups around the map (residents, housing units and land area) and sends only the map's bounding box. The data covers the United States and its territories. Source: U.S. Census Bureau, 2020 Census, via the TIGERweb Census2020 Tracts_Blocks service. U.S. government data, in the public domain.

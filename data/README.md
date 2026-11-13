# Data

`itinere_simplified.rds` is a simplified copy of the Itiner-e Roman road network, used by the Explore history window. It is an `sf` table of line geometries in WGS 84 with these columns: `kind` (main or secondary), `status` (certain, conjectured, or hypothetical), `slope`, `name`, and six travel-time columns named `d_*` (days from Rome by foot, cart, and courier, over land and by sea).

Sources:

- Itiner-e: de Soto, P., Pažout, A., Brughmans, T., and Vahlstrup, P. (2024). The Itiner-e Roman road network, static version. CC BY 4.0.
- Hotham, I. *By Road and by Sea*, travel times from Rome. CC BY 4.0.

The geometry was simplified for display speed, so use the original Itiner-e release for measurement. The ancient city plans do not use this file. They are downloaded from OpenStreetMap when requested.

## Ancient city snapshot

`ancient_cities.rds`, when present, holds the OpenStreetMap streets, paths, and tracks for each site on the Ancient cities tab, so those maps draw at once and do not wait on the public Overpass servers. Each entry records the site, the radius, and the date the snapshot was taken. Build or refresh it from the project root:

```sh
Rscript data-raw/fetch_ancient_cities.R
```

The script skips sites already saved, so run it again to fill in any that failed. Add `--refresh` to download every site anew. Without the file, the app downloads a site when you press Draw map.

The streets are copyright OpenStreetMap contributors and available under the Open Database License (<https://www.openstreetmap.org/copyright>).

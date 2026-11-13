# Data

`itinere_simplified.rds` is a simplified copy of the Itiner-e Roman road network, used by the Explore history window. It is an `sf` table of line geometries in WGS 84 with these columns: `kind` (main or secondary), `status` (certain, conjectured, or hypothetical), `slope`, `name`, and six travel-time columns named `d_*` (days from Rome by foot, cart, and courier, over land and by sea).

Sources:

- Itiner-e: de Soto, P., Pažout, A., Brughmans, T., and Vahlstrup, P. (2024). The Itiner-e Roman road network, static version. CC BY 4.0.
- Hotham, I. *By Road and by Sea*, travel times from Rome. CC BY 4.0.

The geometry was simplified for display speed, so use the original Itiner-e release for measurement. The ancient city plans do not use this file. They are downloaded from OpenStreetMap when requested.

# Security

## What Overmaps is exposed to

Overmaps runs on the machine that starts it. It keeps no account and writes no telemetry. It does reach out to public map services, because that is how a map gets made. The text you type to find a place goes to these hosts, and only when you ask for a place or a map:

- `nominatim.openstreetmap.org` finds a place by name or address and supplies its city limits.
- `photon.komoot.io` offers suggestions while you type.
- `geocoding.geo.census.gov` is asked for U.S. street addresses that include a city and state.
- `tigerweb.geo.census.gov` supplies the 2020 Census block groups for the Population and Housing maps. It receives only the coordinates of the map's bounding box, and only when you choose one of those maps.
- `overpass-api.de`, `overpass.private.coffee`, `maps.mail.ru`, and `ovp.dresel.systems` supply street geometry. The app tries them in turn.
- `api.openstreetmap.org` is the fallback source of street geometry for small areas.
- Google Fonts is contacted at startup for the map typefaces. If it cannot be reached, the app falls back to a standard sans serif font.

Suggestions can be turned off in the Settings section of the sidebar. Nothing else about you is sent.

Three things are worth knowing.

A share link contains the place or address you typed, so check it before you pass it on.

Saved settings are optional. When on, they are kept in your browser under one key, `overmaps.settings`, and they never include the address.

Downloaded street data is cached on your computer in a `roads_cache` folder, keyed by numeric coordinates. A download log, a CSV file with no place names, is kept in the folder named by `STREET_MAPS_DATA` if set, and otherwise in your R user data folder. You can point the app at other Overpass servers with `STREET_MAPS_OVERPASS`, and at a different data folder with `OVERMAPS_DATA`.

## What the code does about it

Place text is capped at 200 characters. Values restored from a share link are checked against what the app offers before they are used. Error messages are cleaned before they reach the page, and text that goes into a map title or caption is escaped so it cannot inject markup.

## Reporting a problem

Open a private security advisory through the repository Security tab, or open a normal issue if the problem is not sensitive. Please include the version, what you did, and what you saw.

There is no service to take down and no user data to breach, so a report here is about the code rather than about an incident. Expect a first reply within a week.

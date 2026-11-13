# Changelog

## 1.0.1

- Ancient cities draw at once from a saved snapshot of OpenStreetMap streets when one ships with the app. `data-raw/fetch_ancient_cities.R` builds or refreshes the snapshot, and any site not in it is still downloaded when you press Draw map.
- Sites outside Europe and North America get more time and an extra pass when the public servers are busy.
- A failed server request now reports what actually went wrong, instead of a generic message.
- The README shows the Classic, Dusk, and Mono looks and the Explore history window.

## 1.0.0

First public release of Overmaps: Geek out on streets.

### Map any street network

- Search by address, city, or landmark, with suggestions as you type. House numbers are suggested, and U.S. addresses are matched against the Census Bureau address file as well.
- Six map types: road hierarchy, speed limits, direction, street density, bike and walk, and block length.
- Map a circle of 1 to 5 kilometers (or miles), outline the city limits, or map the city limits alone.
- Three looks: Classic, Dusk, and Mono.
- Download PNG, JPEG, TIFF, PDF, or SVG at print sizes from 8 by 10 to 24 by 30 inches.
- Compare places side by side, including well-known planned street grids.
- Share a link that redraws the same map. The link contains the place or address you typed.

### Explore history

- The roads of the Roman Empire from the Itiner-e network, by region, as main and secondary roads, strength of the evidence, steepness, or days of travel from Rome on foot, by cart, or by courier.
- Street plans of fifteen ancient cities across 11 civilizations: Roman, Greek and Roman, Greek, Minoan, Nabataean, Palmyrene, Indus Valley, Khmer, Teotihuacan, Maya, and Inca. Each plan reports how many streets OpenStreetMap holds for the site, and a saved snapshot of the streets can ship with the app so the plans draw at once.
- A full screen option, with a sidebar that starts at a normal width and can be widened, as on the main page.
- The same "Choose a look" picker as the main map.

### Privacy and accessibility

- No accounts and no telemetry. Place text is sent only to the lookup services, and the Settings panel explains which ones.
- Saved settings stay in the browser and never include the address.
- Map images have text descriptions, status messages are announced, and suggestions work from the keyboard.

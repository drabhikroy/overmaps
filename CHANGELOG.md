# Changelog

## 1.1.0

- Expand no longer disappears from the header on Egyptian desert and Ancient cities. The tabs that switch between the three sets of historical maps were being read as a change of page.
- When a server's quick status check fails, the next pass goes straight to the real request instead of failing on the same check again, and the log in the console now shows the whole reason a server failed rather than the first 120 characters.
- The dark page dims the Home map and darkens the Home cards, the Help panel, the Help topic tabs, and the place card that appears after a place is found. The page theme never changes a map's look.
- Expand now works on Historical maps the same way as on Current maps, and replaces the View map full screen button, which is gone. Exit expand has no outline.
- A Settings page replaces the Color vision menu in the header and the Settings and privacy and Download counts panels in the Current maps sidebar. The header now reads Home, Current maps, and Historical maps on the left, with more room between them, and Expand, Settings, Help, and the Light or Dark button on the right, all styled as the same kind of link.
- Current maps and Historical maps now share one empty page: the watermark graphic is centered in the stage with its line of text directly below, on both pages and reading "Choose a location on the left, then draw its map." on each, with the Königsberg bridges drawing recolored for the dark page (the header icon too), lighter, and fading out at its edges instead of ending in a circle, and the Historical stage ends exactly where the Current maps stage does instead of running past the window.
- A Dark or Light button in the header switches the page between a light and a dark theme, following the computer's setting on the first visit and remembering the choice after that. Every color vision option has its own dark variant that keeps 4.5:1 contrast, and the text on filled buttons turns dark to match.
- Historical maps now has the same page color and stage as Current maps. A rule meant to color the stage never matched the page, so the stage sat on the lighter page color.
- The site fetch script now treats a server answer that holds no streets as a failure and moves on to the next server, since a busy server can reply with an empty page that looks normal. `data-raw/check_site.R` shows what each server says about one site.
- A Home page opens first, with a very light street map behind it and one card for Current maps and one for Historical maps, each with a pastel icon and the source of its data. Home, Current maps, Historical maps and Help are tabs in the header, so switching is one click and each page keeps its settings. Explore history is no longer a pop-up window. Historical maps is a page of its own, built on the same frame as Current maps: a full-height sidebar on the left, the map on the right, and a picker at the top of the sidebar for the three sets (Roman roads, Egyptian desert, Ancient cities). A shared link or saved settings open the Current maps page.
- A Help tab follows the layout of the other apps: topics on the left, questions that open one at a time, a Sources and credits topic, and an About block with the version, license, copyright and a link to the newest release. It replaces the Good to know list on Home.
- Sources are now part of About this map on every map page (Current maps and each historical set), and stay on the Home cards and in Help. Every downloaded map still carries its data credit.
- Color vision menu in the header: Standard, Deuteranopia, Protanopia, Tritanopia, and Grayscale. It recolors the whole app (buttons, tabs, switches, links, and accents) as well as every map. On the maps it changes speed limits, direction, and nature and water for each kind of color blindness, and every color for Grayscale, on the current maps and on all three historical sets. Each palette was tested under simulated color vision so neighboring colors stay apart, and line weights and shapes carry the meaning as well. The choice is part of the share link.
- Each Download section has its own Color vision of the file menu, which starts at Standard, so a file can differ from what the screen shows. The look previews follow the header menu, and Grayscale now turns the whole page gray.
- Historical maps opens on the bridges picture and draws when Draw map is pressed, as Current maps does. The Help answers sit in a panel of fixed height that scrolls, with a button for a taller one. The area around Exit expand takes the stage color.
- The starting radius of an ancient city is fitted to the streets mapped there, with a small margin, so the circle ends just past the edge of the remains. When the saved streets run out to the edge of the snapshot (a modern town around the ruins), they do not mark the edge of the remains, so the listed radius stays. The saved snapshot now reaches out to about twice the listed radius (never past 3 km) so the app can see where the remains end; run data-raw/fetch_ancient_cities.R again to refresh the sites saved before, and it prints the fitted radius of every site. A site with very little mapped keeps its listed radius.
- `data-raw/fetch_ancient_cities.R` asks once per site and rests five minutes between passes over the sites that failed, instead of waiting up to twelve minutes inside each site. It stops after four passes or when five sites in a row fail, and it keeps everything already saved.
- The download from OpenStreetMap's main site no longer fails on a relation that lists a way outside the box ("way can not be found"); the file is read again without relations.
- The Königsberg bridges icon is now the page icon in the browser tab (SVG, PNG, ICO and Apple touch versions in www).
- The header lines up the brand, the tagline, the tabs and the Color vision menu on one center line, with the tabs moved a little to the right.
- Street lights is a new map type. Streets tagged as lit and individual street lamps glow against a night version of the chosen look (navy and orange for Classic, amber for Dusk, white for Mono). Lit streets come from the ordinary download; the lamps and lit walking and bike paths come from one extra request, made only when this map type is chosen and kept for the place on screen.
- Nature & water is a new map type. Parks, woods, grass, lakes, rivers, canals, streams, and the sea are drawn softly under the streets in every look, with the share of the map each covers. It uses one extra request, made only when this map type is chosen, and its caption says plainly that unmapped green space is not shown.
- The radius now starts just past the city limits. A town or city is centered on the middle of its limits instead of the point the geocoder reports, the starting radius adds a small margin (3% or 150 meters) and rounds up to the next quarter kilometer, and the slider moves in quarter kilometers (tenths of a mile) so it can land close. Street addresses still center the map on the address.
- Lighting is tagged unevenly in OpenStreetMap, so the map is always drawn and its subtitle and caption say how much was found: the share of street length tagged lit and the number of lamps, with a plain note when very little is mapped.
- Street density has a relief switch. Each hexagon rises into a spike drawn in the flat view, on one fixed scale for every place, so dense cores stand out and two maps can be compared. Hexagons outside the city limit are faded, and the city limit is drawn under the spikes.
- The relief setting is part of the share link and of the download cache key.
- The Egyptian desert set is a third tab of Historical maps, Egyptian desert, with modeled camel caravan routes across the Eastern Desert for the Ptolemaic and Roman periods (Manière, Crépy and Redon, 2020, CC BY 4.0, Zenodo 10.5281/zenodo.4291876). The routes are the output of a least-cost model, not roads found on the ground, and the tab, every map, and every download say so, together with the authors' own validation figures against 18th and 19th century travelers' routes. The tab covers no pharaonic period. `data-raw/build_eastern_desert.R` builds the small data file from the Zenodo record.
- More room around the text on every map: larger gaps between the title and subtitle, the subtitle and the map, the map and the legend, and the legend and the caption.
- Ancient cities grows to ninety sites in 22 regions. The newest additions are Carthage, Herculaneum, Italica, Pergamon, Hierapolis, Aphrodisias, Perge, Susa, Ctesiphon, Ashur, Mari, Ugarit, Byblos, Jericho, Karnak and Luxor, Taxila, Anuradhapura, Anyang, Nara, Calakmul, Pachacamac, Chaco Canyon, Mesa Verde, Aksum, Lalibela, and Kilwa Kisiwani. Before them: Babylon, Uruk, Ur, Persepolis, Pasargadae, Merv, Troy, Hattusa, Mycenae, Delphi, Olympia, Akrotiri, Phaistos, Paestum, Cyrene, Hadrian's Villa, Volubilis, Dougga, Sabratha, Jerash, Baalbek, Giza, Amarna, Meroë, Great Zimbabwe, Nalanda, Hampi, Polonnaruwa, Sigiriya, Bagan, Sukhothai, Monte Albán, Chichén Itzá, Uxmal, Tulum, Palenque, Copán, Cahokia, Caral, Chan Chan, Tiwanaku and Ollantaytambo, on top of the Indus Valley sites (Harappa, Dholavira, Lothal, Kalibangan, Rakhigarhi) and the Taş Tepeler sites (Göbekli Tepe, Karahan Tepe). Only archaeological sites that are not buried under a living city were chosen, so the mapped streets are evidence of the site. The Taş Tepeler sites predate streets, so their maps show only the paths mapped there today, and say so.
- Ancient cities has two new map types. Nature & water is offered for every site and shows the water, woods and parks mapped around it today, with a note that rivers and coasts have moved since antiquity. Street lights is offered only for sites whose snapshot found at least ten lamps or lit streets, and is labeled as modern lighting. Both come from one extra request per site, saved in the snapshot, or made live when a site is not in it.
- The Street lights map download is now dark to the edge of the page; before, narrow strips of the day color showed at the sides.
- File type menu: pictures first, then a thin divider, then the vector files in alphabetical order (EPS, PDF, SVG). The Resolution menu now really switches to Vector for those formats on every tab; before, it only did so on the last tab opened, because a loop variable was shared by all four menus.
- Titles, subtitles and captions no longer run into each other on any map. Their boxes now have a fixed width equal to the page less its margins, so a title that wraps to two or three lines gets the height it needs.
- EPS joins the file types. PDF, SVG and EPS are listed without "(vector)", and their resolution menu stays in place and offers only Vector.
- The Ancient cities tab has the main map's units (kilometers or miles), a radius slider that starts at each site's own radius, Reset options, and the density spikes switch. A radius up to the saved one is cut from the saved snapshot with no download; a larger one downloads from OpenStreetMap. Print size is now offered on every Explore history tab.
- Explore history tabs are ordered Roman roads, Egyptian desert, Ancient cities.
- Every map, in the main view, the compare window and each history tab, opens full screen from a button or a click on the map.
- Downloads have a resolution menu beside the file type: 150, 300, 600 dpi or Maximum (limited to about 45 million pixels per file, with a note when a size is capped). PDF and SVG are vector. File names carry a suffix when the resolution is not 300. The compare window has its own resolution menu.

## 1.0.1

- Ancient cities draw at once from a saved snapshot of OpenStreetMap streets when one ships with the app. `data-raw/fetch_ancient_cities.R` builds or refreshes the snapshot, and any site not in it is still downloaded when you press Draw map.
- Sites outside Europe and North America get more time and an extra pass when the public servers are busy.
- A failed server request now reports what actually went wrong, instead of a generic message.
- The README shows the Classic, Dusk, and Mono looks and the Historical maps page.

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

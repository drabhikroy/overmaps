import time, os, json
from playwright.sync_api import sync_playwright
os.makedirs("tests/shots", exist_ok=True)
R = {}
def ok(name, cond, extra=""):
    R[name] = bool(cond); print(("PASS " if cond else "FAIL ") + name, str(extra)[:200], flush=True)
def wait_stats_change(pg, before, timeout=40):
    t0 = time.time()
    while time.time() - t0 < timeout:
        if pg.inner_text("#stats")[:120] != before: return True
        time.sleep(0.5)
    return False
def fetches():
    try: return open("tests/fetch_calls.log").read().strip().splitlines()
    except Exception: return []
open("tests/fetch_calls.log", "w").close()
with sync_playwright() as p:
    b = p.chromium.launch(headless=True, args=["--no-sandbox"])
    ctx = b.new_context(viewport={"width": 1440, "height": 1300}); pg = ctx.new_page()
    errs = []; pg.on("console", lambda m: errs.append(m.text) if m.type == "error" else None); pg.on("pageerror", lambda e: errs.append(str(e)))
    pg.goto("http://127.0.0.1:8765/"); pg.wait_for_selector("#find", timeout=60000); time.sleep(1.5)
    # icon and favicon
    ok("header icon is inline svg", pg.query_selector("header svg, .brand svg, .navbar svg, .app-title svg, h1 svg") is not None or pg.evaluate("!!document.querySelector('svg .km-w, svg [class*=km-]')"))
    fav = pg.evaluate("(document.querySelector('link[rel=icon]')||{}).href||''"); ok("favicon set", fav.startswith("data:image/svg") or "svg" in fav, fav[:60])
    # typeahead for places
    pg.click("#query"); pg.keyboard.type("elk", delay=80); time.sleep(2.0)
    items = pg.query_selector_all(".sugg-item[data-kind=place]"); ok("place suggestions appear while typing", len(items) >= 2, len(items))
    pg.screenshot(path="tests/shots/place_suggestions.png")
    if items: items[0].click(); time.sleep(2.0)
    ok("picking a suggestion fills the query", "Elkins" in pg.input_value("#query"), pg.input_value("#query"))
    pg.wait_for_selector(".place-card", timeout=20000, state="attached"); time.sleep(1)
    pg.evaluate("document.querySelectorAll('.accordion-collapse').forEach(function(e){e.classList.add('show')})")
    # color vision select
    ok("color vision selector is gone", pg.query_selector("#cvd") is None)
    pg.click("#draw"); pg.wait_for_selector("#map img", timeout=90000); time.sleep(3)
    ok("map drawn", pg.is_visible("#map img")); n0 = len(fetches()); ok("one download for the first draw", n0 == 1, fetches())
    sub0 = pg.inner_text("#stats")[:120]
    pg.screenshot(path="tests/shots/first_draw.png", full_page=True)
    # live options: radius smaller, area change; no new download
    pg.evaluate("var s=$('#radius').data('ionRangeSlider'); s.update({from: 1.5}); $('#radius').trigger('change')"); changed = wait_stats_change(pg, sub0); time.sleep(1)
    sub1 = pg.inner_text("#stats")[:120]
    ok("smaller radius redraws live, no new download", len(fetches()) == n0 and changed, (len(fetches()), sub1[:60]))
    ok("no stale hint for a smaller radius", pg.inner_text("#need_draw").strip() == "", pg.inner_text("#need_draw"))
    pg.evaluate("var s=$('#radius').data('ionRangeSlider'); s.update({from: 4.5}); $('#radius').trigger('change')"); time.sleep(6)
    hint = pg.inner_text("#need_draw"); ok("larger radius shows the Draw hint", "Click Draw map" in hint, hint[:120]); ok("larger radius made no download yet", len(fetches()) == n0)
    pg.evaluate("var s=$('#radius').data('ionRangeSlider'); s.update({from: 3}); $('#radius').trigger('change')"); time.sleep(8)
    for v in ("circle", "dim"):
        pg.click("label:has(input[name=area][value=%s])" % v); time.sleep(3)
    ok("changing area uses loaded streets", len(fetches()) == n0, fetches())
    # scale bar / north arrow can only be judged visually
    pg.screenshot(path="tests/shots/live_circle.png", full_page=True)
    # color vision
    pg.click("input[name=style][value=dusk]", force=True); time.sleep(2.5); ok("style change redraws without download", len(fetches()) == n0)
    pg.click("input[name=style][value=classic]", force=True); time.sleep(2)
    # inspect + highlight
    pg.click("label:has(#inspect)") if pg.query_selector("label:has(#inspect)") else pg.click("#inspect"); time.sleep(1)
    img = pg.query_selector("#map img"); bb = img.bounding_box(); hit = False
    for fx in (0.5, 0.45, 0.55, 0.4, 0.6, 0.35, 0.65, 0.3, 0.7):
        for fy in (0.5, 0.45, 0.55, 0.4, 0.6, 0.35, 0.65):
            pg.mouse.click(bb["x"] + bb["width"] * fx, bb["y"] + bb["height"] * fy); time.sleep(1.2)
            if pg.query_selector(".inspect-row"): hit = True; break
        if hit: break
    ok("inspect finds a street", hit); npoly = pg.evaluate("document.querySelectorAll('#map .hl-svg polyline').length")
    ok("clicked street is highlighted (overlay drawn)", npoly >= 2, npoly)
    if hit:
        r = pg.evaluate("(function(){var i=document.querySelector('#map img').getBoundingClientRect(), s=document.querySelector('#map .hl-svg'); if(!s) return null; var q=s.getBoundingClientRect(); return [Math.abs(i.left-q.left), Math.abs(i.top-q.top), Math.abs(i.width-q.width), Math.abs(i.height-q.height)]})()")
        ok("overlay sits exactly on the picture", r is not None and max(r) < 1.5, r)
    pg.screenshot(path="tests/shots/highlight.png", full_page=True)
    pg.click("label:has(#hl_all)") if pg.query_selector("label:has(#hl_all)") else None; time.sleep(0.5)
    # street search
    pg.fill("#street_q", "Davis"); pg.click("#street_go"); time.sleep(2.5)
    found = pg.inner_text("#street_found"); ok("street search reports matches and length", "match" in found and ("km" in found or "mi" in found), found[:120])
    ok("street search highlights", pg.evaluate("document.querySelectorAll('#map .hl-svg polyline').length") >= 2)
    frac = pg.evaluate("""() => {
      const img = document.querySelector('#map img'), svg = document.querySelector('#map .hl-svg'); if (!img || !svg) return null;
      const c = document.createElement('canvas'); c.width = img.naturalWidth; c.height = img.naturalHeight; const x = c.getContext('2d'); x.drawImage(img, 0, 0);
      const plate = [252, 250, 245]; let n = 0, hit = 0;
      document.querySelectorAll('#map .hl-svg polyline.hl-line').forEach(pl => {
        const pts = pl.getAttribute('points').split(' ').map(s => s.split(',').map(Number));
        for (let i = 0; i < pts.length; i += 1) { const px = Math.round(pts[i][0] * c.width), py = Math.round(pts[i][1] * c.height); n++;
          let found = false; for (let dx = -5; dx <= 5 && !found; dx++) for (let dy = -5; dy <= 5 && !found; dy++) { const d = x.getImageData(px + dx, py + dy, 1, 1).data; if (Math.abs(d[0]-plate[0]) + Math.abs(d[1]-plate[1]) + Math.abs(d[2]-plate[2]) > 40) found = true; }
          if (found) hit++; } });
      return n ? hit / n : null; }""")
    ok("highlight lies on drawn streets (pixel check, 5 px)", frac is not None and frac > 0.95, frac)
    pg.screenshot(path="tests/shots/search.png", full_page=True)
    pg.fill("#street_q", "zzzzq"); pg.click("#street_go"); time.sleep(1.5); ok("no match says so and clears overlay", "No street" in pg.inner_text("#street_found") and pg.evaluate("document.querySelectorAll('#map .hl-svg polyline').length") == 0)
    # overlay survives window resize
    pg.fill("#street_q", "Davis"); pg.click("#street_go"); time.sleep(2); pg.set_viewport_size({"width": 1100, "height": 900}); time.sleep(1.2)
    r = pg.evaluate("(function(){var i=document.querySelector('#map img').getBoundingClientRect(), s=document.querySelector('#map .hl-svg'); if(!s) return null; var q=s.getBoundingClientRect(); return [Math.abs(i.left-q.left), Math.abs(i.top-q.top), Math.abs(i.width-q.width), Math.abs(i.height-q.height)]})()")
    ok("overlay follows a window resize", r is not None and max(r) < 2, r); pg.set_viewport_size({"width": 1440, "height": 1300}); time.sleep(1)
    # Bike & walk
    pg.click("label:has(input[name=view][value=active])"); time.sleep(12)
    ok("Bike & walk draws with its tiles", "sidewalks tagged" in pg.inner_text("#stats").lower() and pg.is_visible("#map img"), pg.inner_text("#stats")[:100])
    ok("paths downloaded once, only now", [l for l in fetches() if "paths" in l].__len__() == 1, fetches())
    pg.screenshot(path="tests/shots/bike_walk.png", full_page=True)
    pg.click("label:has(input[name=view][value=hierarchy])"); time.sleep(3)
    # network tiles incl. one-way
    ok("one-way tile shown", "One-way streets" in pg.inner_text("#stats"))
    # print size
    pg.select_option("#print_size", "18x24"); time.sleep(1.5); note = pg.inner_text("#print_note"); ok("print note updates", "18" in note and "24" in note and "dpi" in note, note[:140])
    # compare with presets
    pg.evaluate("document.querySelectorAll('.accordion-collapse').forEach(function(e){e.classList.add('show')})"); time.sleep(0.8)
    pg.select_option("#cmp_preset", index=1); time.sleep(1.2); v1 = pg.input_value("#cmp_query")
    pg.select_option("#cmp_preset", index=3); time.sleep(1.2); v2 = pg.input_value("#cmp_query")
    ok("presets fill the box and add one by one", v1 and ";" in v2, (v1, v2))
    pg.click("#cmp_go"); pg.wait_for_selector(".modal-dialog img", timeout=120000); time.sleep(3)
    mt = pg.inner_text(".modal-dialog"); ok("3-place compare modal with table columns", pg.evaluate("document.querySelectorAll('.cmp-table thead th').length") == 4, mt[:100])    # a blank corner plus three places
    ok("compare has PDF download", pg.query_selector("#cmp_pdf") is not None)
    pg.screenshot(path="tests/shots/compare3.png"); pg.keyboard.press("Escape"); time.sleep(1)
    # remember settings
    pg.evaluate("localStorage.clear()")
    pg.evaluate("document.querySelectorAll('.accordion-collapse').forEach(function(e){e.classList.add('show')})"); pg.click("label:has(#remember)") if pg.query_selector("label:has(#remember)") else pg.click("#remember"); time.sleep(3)
    saved = pg.evaluate("localStorage.getItem('streetmaps.settings')"); ok("settings saved to this browser when switched on", saved and "q=" in saved and "ad=" not in saved, saved)
    pg.goto("http://127.0.0.1:8765/"); pg.wait_for_selector("#find", timeout=60000); time.sleep(5)
    ok("saved settings restored without drawing", "Elkins" in pg.input_value("#query") and pg.query_selector("#map img") is None, (pg.input_value("#query"), pg.query_selector("#map img")))
    ok("remember switch comes back on", pg.is_checked("#remember"))
    pg.evaluate("document.querySelectorAll('.accordion-collapse').forEach(function(e){e.classList.add('show')})"); pg.click("#forget"); time.sleep(1.5); ok("forget clears storage", pg.evaluate("localStorage.getItem('streetmaps.settings')") is None and not pg.is_checked("#remember"))
    # light privacy check: remember off stores nothing
    pg.reload(); pg.wait_for_selector("#find", timeout=60000); time.sleep(2); pg.fill("#query", "Elkins"); time.sleep(3)
    ok("nothing stored when switch is off", pg.evaluate("localStorage.getItem('streetmaps.settings')") is None)
    ok("no browser errors", not [e for e in errs if "favicon" not in e], errs[:3])
    b.close()
bad = [k for k, v in R.items() if not v]; print("\nBROWSER:", len(R) - len(bad), "of", len(R), "passed", "| failed:", bad)

import time, os
from playwright.sync_api import sync_playwright
os.makedirs("tests/shots", exist_ok=True)
def ok(name, cond, extra=""): print(("PASS " if cond else "FAIL ") + name, str(extra)[:200], flush=True)
def wait_for(pg, fn, t=60):
    t0 = time.time()
    while time.time() - t0 < t:
        try:
            if fn(): return True
        except Exception: pass
        time.sleep(0.4)
    return False
def map_src(pg): return pg.evaluate("(document.querySelector('#h_map img')||{}).src || ''")
with sync_playwright() as p:
    b = p.chromium.launch(headless=True, args=["--no-sandbox"])
    ctx = b.new_context(viewport={"width": 1440, "height": 900}, accept_downloads=True); pg = ctx.new_page()
    errs = []; pg.on("console", lambda m: errs.append(m.text) if m.type == "error" else None); pg.on("pageerror", lambda e: errs.append(str(e)))
    pg.goto("http://127.0.0.1:8765/"); pg.wait_for_selector("#find", timeout=60000); time.sleep(2)
    ok("tagline", pg.inner_text(".brand-tag").strip() == "Geek out on streets", pg.inner_text(".brand-tag"))
    ok("Explore history button in the header", pg.is_visible("#history_open"))
    pg.click("#history_open"); pg.wait_for_selector("#h_view", timeout=20000)
    ok("window opens with the controls", all(pg.query_selector(s) for s in ("#h_region", "#h_style", "#h_format", "#h_download")))
    ok("map draws", wait_for(pg, lambda: map_src(pg) != "", 60))
    time.sleep(1.5); pg.screenshot(path="tests/shots/history_kind.png")
    s0 = map_src(pg)
    pg.click("#shiny-modal label:has(input[name=h_view][value=evidence])"); ok("evidence view redraws", wait_for(pg, lambda: map_src(pg) not in ("", s0), 60)); s1 = map_src(pg)
    ok("the map type cards describe each view", "sure" in pg.inner_text("#shiny-modal .view-picker"), pg.inner_text("#shiny-modal .view-picker")[:120])
    ok("travel controls hidden for evidence", not pg.is_visible("#h_how"))
    pg.click("#shiny-modal label:has(input[name=h_view][value=days])"); ok("travel controls shown for days", wait_for(pg, lambda: pg.is_visible("#h_how"), 10))
    ok("days view redraws", wait_for(pg, lambda: map_src(pg) not in ("", s1), 60)); s2 = map_src(pg)
    pg.click("input[name=h_how][value=courier]"); pg.click("label[for=h_sea], #h_sea"); time.sleep(0.5)
    ok("traveler or sea change redraws", wait_for(pg, lambda: map_src(pg) not in ("", s2), 60)); s3 = map_src(pg)
    pg.select_option("#h_region", "britain"); ok("region change redraws", wait_for(pg, lambda: map_src(pg) not in ("", s3), 60)); time.sleep(1.5)
    pg.screenshot(path="tests/shots/history_britain_days.png")
    pg.click("#shiny-modal .tab-pane.active label:has(input[name=h_style][value=dusk])"); time.sleep(5); pg.screenshot(path="tests/shots/history_dusk.png")
    with pg.expect_download(timeout=90000) as dl: pg.click("#h_download")
    d = dl.value; path = "tests/shots/download.png"; d.save_as(path)
    ok("download is a PNG named for the map", d.suggested_filename.startswith("roman_roads_britain_days_") and open(path, "rb").read(4) == b"\x89PNG", d.suggested_filename)
    pg.keyboard.press("Escape"); time.sleep(1.2)
    ok("window closes", not pg.is_visible("#h_view"))
    # inspector: no card until a street is picked
    pg.click("#query"); pg.keyboard.type("Elkins"); pg.click("#find"); time.sleep(4); pg.click("#draw"); pg.wait_for_selector("#map img", timeout=90000); time.sleep(3)
    pg.evaluate("document.querySelectorAll('.accordion-collapse').forEach(e => e.classList.add('show'))")
    pg.evaluate("var sw=document.getElementById('inspect'); if (sw && !sw.checked) sw.click()"); time.sleep(2)
    ok("no inspector card before a click", pg.query_selector(".inspect") is None)
    box = pg.query_selector("#map img").bounding_box(); pg.mouse.click(box["x"] + 6, box["y"] + 6); time.sleep(2)
    ok("a click on empty space leaves no card", pg.query_selector(".inspect") is None)
    ok("no browser errors", len(errs) == 0, errs[:3])
    b.close()

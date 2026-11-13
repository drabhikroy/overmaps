import time, os
from playwright.sync_api import sync_playwright
os.makedirs("tests/shots", exist_ok=True)
def ok(name, cond, extra=""): print(("PASS " if cond else "FAIL ") + name, str(extra)[:220], flush=True)
def fetches():
    try: return open("tests/fetch_calls.log").read().strip().splitlines()
    except Exception: return []
open("tests/fetch_calls.log", "w").close()
def stats(pg): return pg.inner_text("#stats")[:200].replace("\n", " | ")
def wait_for(pg, fn, t=60):
    t0 = time.time()
    while time.time() - t0 < t:
        if fn(): return True
        time.sleep(0.5)
    return False
with sync_playwright() as p:
    b = p.chromium.launch(headless=True, args=["--no-sandbox"])
    ctx = b.new_context(viewport={"width": 1440, "height": 900}); pg = ctx.new_page()
    errs = []; pg.on("console", lambda m: errs.append(m.text) if m.type == "error" else None); pg.on("pageerror", lambda e: errs.append(str(e)))
    pg.goto("http://127.0.0.1:8765/"); pg.wait_for_selector("#find", timeout=60000); time.sleep(2)
    ok("brand says Overmaps", "Overmaps" in pg.inner_text(".brand-name") and pg.title() == "Overmaps", pg.title())
    ok("six map types in two columns of three", len(pg.query_selector_all(".view-picker input[type=radio]")) == 6)
    xs = pg.evaluate("Array.from(document.querySelectorAll('.view-picker label')).map(l => Math.round(l.getBoundingClientRect().x))")
    ok("two columns", len(set(xs)) == 2, xs)
    ok("no address switch shown", pg.query_selector("#use_addr") is not None and not pg.is_visible("#use_addr"))
    # type an address, pick the suggestion
    pg.click("#query"); pg.keyboard.type("401 dav", delay=70); time.sleep(2.2)
    items = pg.query_selector_all(".sugg-item[data-kind=place]"); ok("address suggestions appear in the one box", len(items) == 2, [i.inner_text() for i in items])
    items[0].click(); time.sleep(4.5)
    ok("list closed", len(pg.query_selector_all(".sugg-item")) == 0)
    card = pg.inner_text(".place-card"); ok("card names the place and the address", "Elkins" in card and "401 Davis Avenue" in card, card)
    ok("address mode switched on behind the scenes", pg.is_checked("#use_addr") and "401 Davis Avenue" in pg.input_value("#addr"))
    pg.click("#draw"); pg.wait_for_selector("#map img", timeout=90000); time.sleep(3)
    ok("map drawn around the address", "401 Davis Avenue" in pg.inner_text("#stats") or "401 Davis Avenue" in pg.inner_text(".stage"), stats(pg))
    pg.screenshot(path="tests/shots/address_map.png")
    # use the middle instead
    pg.click("#clear_addr"); time.sleep(2); ok("address cleared", not pg.is_checked("#use_addr") and pg.input_value("#addr") == "")
    # typed address with Find
    pg.fill("#query", "401 Davis Avenue, Elkins"); pg.click("#find"); time.sleep(4.5)
    ok("typed address then Find centers on the address", "Map centered on" in pg.inner_text(".place-card"), pg.inner_text(".place-card"))
    # plain town clears it
    pg.fill("#query", "Elkins"); pg.click("#find"); time.sleep(3.5)
    ok("plain town uses the middle of the place", "Map centered on" not in pg.inner_text(".place-card") and not pg.is_checked("#use_addr"))
    # radius: draw then enlarge: automatic download
    pg.evaluate("var s=$('#radius').data('ionRangeSlider'); s.update({from: 2}); $('#radius').trigger('change')"); time.sleep(2)
    pg.click("#draw"); pg.wait_for_selector("#map img", timeout=90000); time.sleep(3)
    n0 = len(fetches()); s0 = stats(pg)
    pg.evaluate("var s=$('#radius').data('ionRangeSlider'); s.update({from: 4}); $('#radius').trigger('change')")
    got = wait_for(pg, lambda: len(fetches()) > n0, 40); time.sleep(4)
    ok("a larger radius downloads by itself", got, fetches()[n0:])
    ok("and the map follows", wait_for(pg, lambda: "4 km" in stats(pg), 40), stats(pg))
    # block length view
    pg.click("input[name=view][value=blocks]", force=True); time.sleep(5)
    ok("Block length view draws with its tiles", "MEDIAN BLOCK" in pg.inner_text("#stats").upper(), stats(pg))
    pg.screenshot(path="tests/shots/blocks.png")
    # sidebar resize
    w0 = pg.evaluate("document.querySelector('#sb, .sidebar').getBoundingClientRect().width")
    h = pg.query_selector(".bslib-sidebar-layout > .sidebar-resizer, .bslib-sidebar-layout [class*=resize]")
    ok("resize handle exists", h is not None)
    if h:
        bb = h.bounding_box(); pg.mouse.move(bb["x"] + bb["width"] / 2, bb["y"] + bb["height"] / 2); pg.mouse.down(); pg.mouse.move(bb["x"] + 160, bb["y"] + 10, steps=8); pg.mouse.up(); time.sleep(0.8)
        w1 = pg.evaluate("document.querySelector('#sb, .sidebar').getBoundingClientRect().width"); ok("dragging widens the side panel", w1 > w0 + 100, (w0, w1))
        pg.screenshot(path="tests/shots/wide_sidebar.png")
    ok("no browser errors", len(errs) == 0, errs)
    b.close()

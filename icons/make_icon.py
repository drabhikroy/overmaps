# Koenigsberg icon. The Pregel with Kneiphof in the middle, the east fork around Lomse, the two banks, and the seven bridges,
# drawn as an engraved city plan: block-by-block streets, shaded water with a pale shoreline, plank bridges with parapets, and one cross-shaped
# landmark on Kneiphof (the cathedral). Land is the page; the water is stroked on top, so the islands are what is left between the arms.
# Topology (Euler): north bank-Kneiphof x2, south bank-Kneiphof x2, Kneiphof-Lomse x1, north bank-Lomse x1, south bank-Lomse x1.
import math, random, sys

N = 512
# river centerlines (cubic Beziers: p0, p1, p2, p3)
TRUNK = ((-60, 256), (0, 256), (20, 256), (66, 256))
NA = ((80, 256), (98, 104), (292, 104), (328, 256))        # north arm around Kneiphof
SA = ((80, 256), (98, 408), (292, 408), (328, 256))
NE = ((314, 256), (346, 256), (360, 192), (432, 160))       # east fork, north branch
SE = ((314, 256), (346, 256), (360, 320), (432, 352))
NE2 = ((432, 160), (484, 137), (545, 100), (630, 58))
SE2 = ((432, 352), (484, 375), (545, 412), (630, 454))
ARMS = [(TRUNK, 66), (NA, 48), (SA, 48), (NE, 44), (SE, 44), (NE2, 44), (SE2, 44)]

def pt(c, t):
    (x0, y0), (x1, y1), (x2, y2), (x3, y3) = c; u = 1 - t
    return (u**3*x0 + 3*u*u*t*x1 + 3*u*t*t*x2 + t**3*x3, u**3*y0 + 3*u*u*t*y1 + 3*u*t*t*y2 + t**3*y3)
def tan(c, t):
    (x0, y0), (x1, y1), (x2, y2), (x3, y3) = c; u = 1 - t
    dx = 3*u*u*(x1-x0) + 6*u*t*(x2-x1) + 3*t*t*(x3-x2); dy = 3*u*u*(y1-y0) + 6*u*t*(y2-y1) + 3*t*t*(y3-y2)
    return dx, dy
def d_of(c): return "M{} {} C{} {} {} {} {} {}".format(*[v for p in c for v in p])
def sample(c, n=40): return [pt(c, i / n) for i in range(n + 1)]
POLY = [(sample(c), w) for c, w in ARMS]
def water_dist(x, y):
    best = 1e9
    for pts, w in POLY:
        for (ax, ay), (bx, by) in zip(pts, pts[1:]):
            vx, vy = bx - ax, by - ay; L = vx*vx + vy*vy or 1
            t = max(0, min(1, ((x - ax)*vx + (y - ay)*vy) / L)); px, py = ax + t*vx, ay + t*vy
            best = min(best, math.hypot(x - px, y - py) - w / 2)
    return best

# seven bridges: (curve, t, extra length each side)
BRIDGES = [(NA, 0.24, 13), (NA, 0.74, 13), (SA, 0.24, 13), (SA, 0.74, 13), (NE, 0.7, 12), (SE, 0.7, 12)]
def bridge_list():
    out = [(*pt(c, t), math.degrees(math.atan2(*tan(c, t)[::-1])), 44 if c in (NE, SE) else 48, e + 5) for c, t, e in BRIDGES]
    out.append((327, 256, 90, 56, 16))             # Kneiphof to Lomse: across the junction, along the river
    return out

# land regions bounded by arm centerlines (their grids are turned a different way from the banks)
def island_a():
    return sample(NA, 24) + sample(SA, 24)[::-1]
def island_d():
    return sample(NE, 20) + sample(NE2, 12)[1:] + [(640, 100), (640, 412)] + sample(SE2, 12)[::-1][:-1] + sample(SE, 20)[::-1]

def grid_lines(angle, cx, cy, step, n=14, major=3):
    a = math.radians(angle); ca, sa = math.cos(a), math.sin(a); minor, maj = "", ""
    for k in range(-n, n + 1):
        for (x1, y1, x2, y2) in ((k*step, -n*step, k*step, n*step), (-n*step, k*step, n*step, k*step)):
            p = [(cx + x*ca - y*sa, cy + x*sa + y*ca) for x, y in ((x1, y1), (x2, y2))]
            s = "M%.1f %.1f L%.1f %.1f " % (p[0][0], p[0][1], p[1][0], p[1][1])
            if k % major == 0: maj += s
            else: minor += s
    return minor, maj

def blocks(angle, cx, cy, step, seed, keep, n=14, fill=0.74):
    """small building footprints inside grid cells that are on land and well clear of the water"""
    rnd = random.Random(seed); a = math.radians(angle); ca, sa = math.cos(a), math.sin(a); out = []
    for i in range(-n, n):
        for j in range(-n, n):
            u, v = (i + 0.5) * step, (j + 0.5) * step; x, y = cx + u*ca - v*sa, cy + u*sa + v*ca
            if not (-10 < x < N + 10 and -10 < y < N + 10) or not keep(x, y) or water_dist(x, y) < step * 0.5 + 3 or rnd.random() > fill: continue
            w, h = step * rnd.uniform(0.52, 0.74), step * rnd.uniform(0.46, 0.7)
            out.append((x, y, w, h, angle, rnd.random()))
    return out

def in_poly(x, y, poly):
    ins = False; j = len(poly) - 1
    for i in range(len(poly)):
        xi, yi = poly[i]; xj, yj = poly[j]
        if (yi > y) != (yj > y) and x < (xj - xi) * (y - yi) / (yj - yi) + xi: ins = not ins
        j = i
    return ins

def pts_attr(poly): return " ".join("%.1f,%.1f" % p for p in poly)

THEMES = {
    "light": dict(gap="#fdfaf1", blk=("#eadfc4", "#e3d6b4", "#efe6cf"), park="#c8dfae", shore="#a9c6d3", w1="#86c1dc", w2="#4f9bc4", wave="#ffffff", wave_o=.55,
                  deck="#fffdf6", deck_edge="#6c5f43", badge="#fffdf6", ink="#4a3f2a", lab="#6c5f43", rim="#fff9e6", rim2="#cdbb8a", edge="#4a3a22", vig=.14, gloss=.20),
    "dark": dict(gap="#17233a", blk=("#243653", "#2b405f", "#1f2f4a"), park="#25523f", shore="#2d6f8e", w1="#1f86b1", w2="#0d3f66", wave="#bfeaf8", wave_o=.35,
                 deck="#ffe7a6", deck_edge="#0a1220", badge="#ffe7a6", ink="#0a1220", lab="#8fb0d8", rim="#6f8fbd", rim2="#2b4366", edge="#0a1220", vig=.22, gloss=.14),
    # the watermark behind the empty map page: the same plan, drawn in the page's own quiet tones
    "bg": dict(gap="#efece1", blk=("#e4dfcd", "#e1dbc6", "#e8e4d4"), park="#dbe3cc", shore="#cdd9d6", w1="#cddbdc", w2="#bccfd3", wave="#f6f9f7", wave_o=.6,
               deck="#f6efd9", deck_edge="#c9bd9a", badge="#f6efd9", ink="#a89c7c", lab="#bdb393", rim="#d6cfba", rim2="#d6cfba", edge="#cfc8b4", vig=0, gloss=0),
}

def cells(angle, cx, cy, step, seed, gap, n=14):
    rnd = random.Random(seed); a = math.radians(angle); ca, sa = math.cos(a), math.sin(a); out = []
    for i in range(-n, n):
        for j in range(-n, n):
            u, v = (i + 0.5) * step, (j + 0.5) * step; x, y = cx + u*ca - v*sa, cy + u*sa + v*ca
            if -step < x < N + step and -step < y < N + step: out.append((x, y, step - gap, angle, rnd.random()))
    return out

LETTERS = "abcdefg"
def art(theme, pid, detail=True):
    t = THEMES[theme]; d = []
    d.append(f'<defs><linearGradient id="{pid}wat" gradientUnits="userSpaceOnUse" x1="0" y1="90" x2="512" y2="430"><stop offset="0" stop-color="{t["w1"]}"/><stop offset="1" stop-color="{t["w2"]}"/></linearGradient>'
             f'<clipPath id="{pid}ca"><polygon points="{pts_attr(island_a())}"/></clipPath><clipPath id="{pid}cd"><polygon points="{pts_attr(island_d())}"/></clipPath></defs>')
    d.append(f'<rect width="{N}" height="{N}" fill="{t["gap"]}"/>')
    A, D = island_a(), island_d()
    for key, ang, cx, cy, step, seed, clip, poly in (("b", -9, 256, 40, 54, 3, None, None), ("a", 27, 206, 256, 34, 5, f"{pid}ca", A), ("d", -24, 520, 256, 38, 7, f"{pid}cd", D)):
        g = [f'<polygon points="{pts_attr(poly)}" fill="{t["gap"]}"/>'] if poly else []
        stp = step if detail else step * 1.7
        paths = {}
        for (x, y, w, a, r) in cells(ang, cx, cy, stp, seed, 7 if detail else 9):
            col = t["park"] if r > .93 else t["blk"][0] if r < .55 else t["blk"][1] if r < .8 else t["blk"][2]
            h = (w - 5) / 2; ca, sa = math.cos(math.radians(a)), math.sin(math.radians(a))
            pts = [(x + u*ca - v*sa, y + u*sa + v*ca) for u, v in ((-h, -h), (h, -h), (h, h), (-h, h))]
            paths.setdefault(col, []).append("M" + "L".join("%d %d" % (round(px), round(py)) for px, py in pts) + "Z")
        for col, ds in paths.items():
            g.append(f'<path d="{"".join(ds)}" fill="{col}" stroke="{col}" stroke-width="5" stroke-linejoin="round"/>')
        d.append(f'<g clip-path="url(#{clip})">{"".join(g)}</g>' if clip else f'<g>{"".join(g)}</g>')
    # river: shoreline, water, a pale center line, small wave marks
    for c, w in ARMS: d.append(f'<path d="{d_of(c)}" stroke="{t["shore"]}" stroke-width="{w + 12}" fill="none" stroke-linecap="round" stroke-linejoin="round"/>')
    for c, w in ARMS: d.append(f'<path d="{d_of(c)}" stroke="url(#{pid}wat)" stroke-width="{w}" fill="none" stroke-linecap="round" stroke-linejoin="round"/>')
    if detail:
        for c, w in ARMS[:5]:
            for off, dash in ((-w * 0.22, "16 30"), (w * 0.22, "10 34")):
                d.append(f'<path d="{d_of(c)}" transform="translate(0 {off:.1f})" stroke="{t["wave"]}" stroke-width="3" fill="none" stroke-linecap="round" stroke-dasharray="{dash}" opacity="{t["wave_o"]}"/>')
    # the seven bridges are streets that carry on across the water
    br = bridge_list()
    for k, (x, y, ang, w, ext) in enumerate(br):
        L = w + 2 * ext + 6
        P = L - 2 * ext + 4                      # parapets run only over the water; the road carries on to land at both ends
        d.append(f'<g transform="translate({x:.1f} {y:.1f}) rotate({ang:.1f})"><rect x="-8.5" y="-{L/2}" width="17" height="{L}" fill="{t["deck"]}"/>'
                 f'<rect x="-9.5" y="-{P/2}" width="3.4" height="{P}" fill="{t["deck_edge"]}"/><rect x="6.1" y="-{P/2}" width="3.4" height="{P}" fill="{t["deck_edge"]}"/></g>')
    if detail:
        # Euler's letters: a to g on the bridges, A to D on the land
        for k, (x, y, ang, w, ext) in enumerate(br):
            r = math.radians(ang); bx, by = x + 25 * math.cos(r), y + 25 * math.sin(r)
            if k == 6: bx, by = x, y - 26
            d.append(f'<circle cx="{bx:.1f}" cy="{by:.1f}" r="13" fill="{t["badge"]}" stroke="{t["deck_edge"]}" stroke-width="2.5"/>'
                     f'<text x="{bx:.1f}" y="{by + 5.5:.1f}" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="17" fill="{t["ink"]}">{LETTERS[k]}</text>')
        for (lx, ly, ch) in ((206, 268, "A"), (190, 66, "B"), (190, 462, "C"), (468, 262, "D")):
            d.append(f'<text x="{lx}" y="{ly}" text-anchor="middle" font-family="Georgia, Times New Roman, serif" font-style="italic" font-weight="700" font-size="46" fill="{t["lab"]}" opacity=".9">{ch}</text>')
    return "".join(d)

def tile(theme, pid, detail=True, label=True):
    t = THEMES[theme]; R = 116
    aria = 'role="img" aria-label="Street plan of the Konigsberg islands and their seven bridges"' if label else 'aria-hidden="true" focusable="false"'
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {N} {N}" {aria}><defs><clipPath id="{pid}t"><rect width="{N}" height="{N}" rx="{R}"/></clipPath>'
            f'<radialGradient id="{pid}vig" cx=".5" cy=".5" r=".72"><stop offset=".62" stop-color="#2a1a05" stop-opacity="0"/><stop offset="1" stop-color="#2a1a05" stop-opacity="{t["vig"]}"/></radialGradient>'
            f'<linearGradient id="{pid}gl" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#fff" stop-opacity="{t["gloss"]}"/><stop offset=".45" stop-color="#fff" stop-opacity="0"/></linearGradient></defs>'
            f'<g clip-path="url(#{pid}t)">{art(theme, pid, detail)}<rect width="{N}" height="{N}" fill="url(#{pid}vig)"/><rect width="{N}" height="{N//2+30}" fill="url(#{pid}gl)"/>'
            f'<rect x="13" y="13" width="{N-26}" height="{N-26}" rx="{R-13}" fill="none" stroke="{t["rim"]}" stroke-width="5" opacity=".9"/>'
            f'<rect x="22" y="22" width="{N-44}" height="{N-44}" rx="{R-22}" fill="none" stroke="{t["rim2"]}" stroke-width="2" opacity=".8"/></g>'
            f'<rect x="1.5" y="1.5" width="{N-3}" height="{N-3}" rx="{R}" fill="none" stroke="{t["edge"]}" stroke-width="3" opacity=".55"/></svg>')

def watermark():
    t = THEMES["bg"]
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {N} {N}"><defs><clipPath id="wm"><circle cx="256" cy="256" r="250"/></clipPath></defs>'
            f'<g clip-path="url(#wm)">{art("bg", "w", True)}</g><circle cx="256" cy="256" r="250" fill="none" stroke="#cfc8b6" stroke-width="4"/></svg>')

def mono():
    ds = lambda: "".join(f'<path d="{d_of(c)}" stroke="#fff" stroke-width="{w}" fill="none" stroke-linecap="round"/>' for c, w in ARMS)
    br = "".join(f'<g transform="translate({x:.1f} {y:.1f}) rotate({a:.1f})"><rect x="-13" y="-{(w+2*e)/2}" width="26" height="{w+2*e}" rx="6" fill="#000" stroke="#fff" stroke-width="7"/><rect x="-13" y="-{(w+2*e)/2}" width="26" height="{w+2*e}" rx="6" fill="#000"/></g>' for x, y, a, w, e in bridge_list())
    A, D = island_a(), island_d()
    cuts = ""
    for ang, cx, cy, step, poly in ((-9, 256, 40, 56, None), (27, 206, 256, 34, A), (-24, 520, 256, 38, D)):
        mi, ma = grid_lines(ang, cx, cy, step)
        cuts += f'<path d="{ma}" stroke="#fff" stroke-width="6" fill="none"/>' if poly is None else f'<clipPath id="mc{ang}"><polygon points="{pts_attr(poly)}"/></clipPath><g clip-path="url(#mc{ang})"><rect width="{N}" height="{N}" fill="#000"/><path d="{ma}" stroke="#fff" stroke-width="6" fill="none"/></g>'
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {N} {N}" role="img" aria-label="Street plan of the Konigsberg islands and their seven bridges"><defs><clipPath id="mt"><rect width="{N}" height="{N}" rx="116"/></clipPath></defs>'
            f'<g clip-path="url(#mt)"><rect width="{N}" height="{N}" fill="#000"/>{cuts}{ds()}{br}</g><rect x="6" y="6" width="{N-12}" height="{N-12}" rx="110" fill="none" stroke="#000" stroke-width="12"/></svg>')

if __name__ == "__main__":
    out = sys.argv[1] if len(sys.argv) > 1 else "."
    w = lambda n, s: open(f"{out}/{n}", "w").write(s)
    w("icon.svg", tile("light", "i", True)); w("icon-dark-tile.svg", tile("dark", "d", True)); w("icon-mono.svg", mono())
    w("icon-inline.svg", tile("light", "km", False, False)); w("watermark.svg", watermark())

# Synthetic but city-like street network around Elkins, WV, for offline UI and rendering checks.
suppressMessages({ library(sf); library(dplyr); library(tibble) })
lon0 <- -79.8467; lat0 <- 38.9262; crs0 <- 32617
ctr <- st_transform(st_sfc(st_point(c(lon0, lat0)), crs = 4326), crs0)
cx <- st_coordinates(ctr)[1]; cy <- st_coordinates(ctr)[2]
mkl <- function(pts, h, nm = NA_character_) st_sf(osm_id = "1", name = nm, highway = h, geometry = st_sfc(st_linestring(pts), crs = crs0))
smooth_line <- function(a, b, n = 60, amp = 40) {      # a gently curving line from a to b: a slow bend, no jitter
  t <- seq(0, 1, length.out = n); d <- c(b[2] - a[2], -(b[1] - a[1])); d <- d / sqrt(sum(d^2))
  off <- amp * sin(pi * t) * sin(2 * pi * t * runif(1, 0.5, 1.2) + runif(1, 0, pi))
  cbind(a[1] + (b[1] - a[1]) * t + d[1] * off, a[2] + (b[2] - a[2]) * t + d[2] * off)
}
wob <- smooth_line                                      # kept as an alias for older scripts
rot <- function(pts, deg, about) { r <- deg * pi / 180; x <- pts[, 1] - about[1]; y <- pts[, 2] - about[2]
  cbind(about[1] + x * cos(r) - y * sin(r), about[2] + x * sin(r) + y * cos(r)) }
demo_lines <- function(seed = 3) {
  set.seed(seed); L <- list(); R <- 5300
  streets <- c("Oak", "Maple", "Elm", "Cedar", "Pine", "Walnut", "Spruce", "Chestnut", "Hickory", "Laurel", "Davis", "Randolph")
  block <- function(x, y, horizontal, deg = 0, about = c(cx, cy)) {          # one straight block, optionally in a rotated subdivision
    p <- if (horizontal) rbind(c(cx + x, cy + y), c(cx + x + 140, cy + y)) else rbind(c(cx + x, cy + y), c(cx + x, cy + y + 140))
    mkl(if (deg == 0) p else rot(p, deg, about), "residential")
  }
  for (x in seq(-R, R, by = 140)) for (y in seq(-R, R, by = 140)) {
    if (runif(1) < 0.18) next
    in_ne <- x > 1000 && y > 600                                              # a subdivision laid out on a different angle
    if (in_ne) next
    L[[length(L) + 1]] <- block(x, y, runif(1) < 0.5)
    if (runif(1) < 0.55) L[[length(L) + 1]] <- block(x, y, runif(1) < 0.5)
  }
  for (x in seq(1000, R, by = 130)) for (y in seq(600, R, by = 130)) {          # the rotated subdivision (about 28 degrees)
    if (runif(1) < 0.15) next
    L[[length(L) + 1]] <- block(x, y, TRUE, 28, c(cx + 1000, cy + 600)); L[[length(L) + 1]] <- block(x, y, FALSE, 28, c(cx + 1000, cy + 600))
  }
  for (k in 1:9) {                                                              # curving loops and cul-de-sacs
    c0 <- c(cx + runif(1, -3800, 800), cy + runif(1, -3800, 3000)); th <- seq(0, runif(1, 3.5, 5.5), length.out = 40); rr <- runif(1, 70, 140)
    L[[length(L) + 1]] <- mkl(cbind(c0[1] + rr * cos(th), c0[2] + rr * sin(th)), "residential")
  }
  for (k in seq(-4800, 4800, by = 650)) {
    nm1 <- paste(sample(streets, 1), "Street"); nm2 <- paste(sample(streets, 1), "Avenue")
    L[[length(L) + 1]] <- mkl(smooth_line(c(cx - R, cy + k), c(cx + R, cy + k + rnorm(1, 0, 90)), 80, 70), "tertiary", nm1)
    L[[length(L) + 1]] <- mkl(smooth_line(c(cx + k, cy - R), c(cx + k + rnorm(1, 0, 90), cy + R), 80, 70), "tertiary", nm2)
  }
  L[[length(L) + 1]] <- mkl(smooth_line(c(cx - R, cy + 700), c(cx + R, cy + 900), 80, 140), "secondary", "Beverly Pike")
  L[[length(L) + 1]] <- mkl(smooth_line(c(cx - 1900, cy - R), c(cx - 1500, cy + R), 80, 140), "secondary", "Harrison Avenue")
  L[[length(L) + 1]] <- mkl(smooth_line(c(cx - R, cy - 300), c(cx + R, cy + 200), 80, 110), "primary", "Davis Avenue")
  L[[length(L) + 1]] <- mkl(smooth_line(c(cx + 200, cy - R), c(cx - 100, cy + R), 80, 110), "primary", "Randolph Avenue")
  th <- seq(-0.4, 1.7, length.out = 90)
  L[[length(L) + 1]] <- mkl(cbind(cx + 3300 * cos(th) - 600, cy + 3300 * sin(th) - 800), "trunk", "Corridor H")
  bind_rows(L)
}
demo_roads <- function() {
  l <- demo_lines(); set.seed(1)                       # a plausible, patchy US speed-limit tagging
  l$maxspeed <- ifelse(l$highway %in% c("motorway", "trunk"), "55 mph", ifelse(l$highway == "primary", "35 mph",
                ifelse(l$highway == "residential", sample(c("25 mph", NA, "20 mph"), nrow(l), TRUE), NA)))
  list(osm_lines = st_transform(l, 4326))
}

# A city-limits-style boundary: an irregular blob about 5 km across, in lon/lat like Nominatim returns it.
demo_boundary <- function() {
  th <- seq(0, 2 * pi, length.out = 121)
  r <- 2500 * (1 + 0.22 * sin(3 * th + 1) + 0.12 * cos(2 * th))
  xy <- cbind(cx + 200 + r * cos(th), cy - 150 + r * sin(th)); xy[nrow(xy), ] <- xy[1, ]
  st_transform(st_sfc(st_polygon(list(xy)), crs = crs0), 4326)
}

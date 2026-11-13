#!/usr/bin/env Rscript
# Run Overmaps in a browser tab (default) or in its own popup window.
#
#   Rscript launch.R                  browser tab
#   Rscript launch.R --popup          app-style window: no tabs, no address bar
#   Rscript launch.R --popup --port 8080
#
# From an R console:
#   source("launch.R"); run_street_maps("popup")
#
# Popup mode uses Chrome, Edge, Brave, or Chromium in its "app" mode. If none is found, the app opens in your default
# browser instead and says so. To choose a specific browser, set STREET_MAPS_BROWSER to its full path.
# Stop the app with Ctrl+C (or the stop button in RStudio); closing the window does not stop the R process.

find_app_browser <- function(os = Sys.info()[["sysname"]], override = Sys.getenv("STREET_MAPS_BROWSER", unset = "")) {
  if (nzchar(override)) return(if (file.exists(override)) override else NA_character_)
  candidates <- switch(os,
    Windows = {
      roots <- unique(Sys.getenv(c("PROGRAMFILES", "PROGRAMFILES(X86)", "LOCALAPPDATA"), unset = NA))
      roots <- roots[!is.na(roots)]
      unlist(lapply(roots, function(r) file.path(r, c("Google/Chrome/Application/chrome.exe", "Microsoft/Edge/Application/msedge.exe",
                                                       "BraveSoftware/Brave-Browser/Application/brave.exe", "Chromium/Application/chrome.exe"))))
    },
    Darwin = c("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
               "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser", "/Applications/Chromium.app/Contents/MacOS/Chromium"),
    unname(Sys.which(c("google-chrome", "google-chrome-stable", "chromium", "chromium-browser", "microsoft-edge", "brave-browser"))))
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit)) hit[[1]] else NA_character_
}

popup_args <- function(url, width = 1360, height = 900) {
  c(paste0("--app=", url), sprintf("--window-size=%d,%d", width, height), "--no-first-run", "--no-default-browser-check")
}

# Returns a function for shiny::runApp(launch.browser = ). It never errors: a failure to open a window falls back to the default browser.
make_launcher <- function(mode = c("browser", "popup"), find = find_app_browser, spawn = function(exe, args) system2(exe, args, wait = FALSE, stdout = FALSE, stderr = FALSE)) {
  mode <- match.arg(mode)
  function(url) {
    if (mode == "popup") {
      exe <- find()
      if (!is.na(exe)) {
        ok <- tryCatch({ spawn(exe, popup_args(url)); TRUE }, error = function(e) FALSE)
        if (ok) { message("Opened the app in its own window: ", url); return(invisible()) }
      }
      message("No Chrome, Edge, Brave, or Chromium was found (or it would not start), so the app is opening in your default browser.")
    }
    utils::browseURL(url)
  }
}

run_street_maps <- function(mode = c("browser", "popup"), port = NULL, app = "app.R") {
  mode <- match.arg(mode)
  shiny::runApp(app, host = "127.0.0.1", port = port, launch.browser = make_launcher(mode))
}

if (sys.nframe() == 0) {                                 # only when run as a script, not when sourced
  args <- commandArgs(trailingOnly = TRUE)
  port <- if ("--port" %in% args) as.integer(args[match("--port", args) + 1]) else NULL
  here <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))), error = function(e) ".")
  run_street_maps(if ("--popup" %in% args) "popup" else "browser", port = port, app = file.path(here, "app.R"))
}

# Install the R packages Overmaps needs. Run from the project folder: Rscript install.R

if (getRversion() < "4.1.0") {
  stop("Overmaps needs R 4.1 or later. Update R, then run this installer again.")
}

# Use a library owned by the person running Overmaps, which avoids a system password prompt.
r_series <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
user_library <- file.path(tools::R_user_dir("overmaps", which = "data"), "library", r_series)
if (!dir.exists(user_library) && !dir.create(user_library, recursive = TRUE, showWarnings = FALSE)) {
  stop("Overmaps could not create a personal R package library at ", user_library, ".")
}
if (file.access(user_library, 2) != 0L) {
  stop("Overmaps cannot write to the personal R package library at ", user_library, ".")
}
.libPaths(unique(c(user_library, .libPaths())))

core <- c("shiny", "bslib", "sf", "osmdata", "dplyr", "tidyr", "purrr", "tibble", "stringr", "ggplot2", "ggtext",
          "showtext", "sysfonts", "glue", "patchwork", "httr2", "ragg", "jsonlite", "stringi", "rlang")
testing <- c("testthat", "withr", "xml2", "base64enc", "commonmark", "png")

repos <- getOption("repos")
cran_repo <- if (length(repos) && "CRAN" %in% names(repos)) repos[["CRAN"]] else NA_character_
if (is.null(repos) || !length(repos) || is.na(cran_repo) || identical(cran_repo, "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

need <- function(pkgs) pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
missing_core <- need(core)
if (length(missing_core)) {
  message("Installing: ", paste(missing_core, collapse = ", "))
  install.packages(missing_core, dependencies = c("Depends", "Imports", "LinkingTo"))
}
still <- need(core)
if (length(still)) {
  stop("Overmaps could not install these required packages: ", paste(still, collapse = ", "),
       ". Check the messages above, then run Rscript install.R again.")
}
message("Overmaps packages are ready. Run Rscript launch.R from this folder.")
message("The packages used only by the test suite are: ", paste(testing, collapse = ", "), ".")

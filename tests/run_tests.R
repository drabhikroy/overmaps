# Run the Overmaps test suite from the project root:  Rscript tests/run_tests.R
suppressMessages(library(testthat))
Sys.setenv(APP_FILE = normalizePath("app.R"))      # test_file() moves into tests/, so give the app path in full
res <- testthat::test_file("tests/test-app.R", reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(res)
cat(sprintf("\nTESTS: %d blocks | expectations passed: %d | failed: %d | errors: %d | skipped: %d\n",
            nrow(df), sum(df$passed), sum(df$failed), sum(df$error), sum(df$skipped)))
bad <- df[df$failed > 0 | df$error, c("test", "failed", "error")]
if (nrow(bad)) { print(bad); quit(status = 1) }

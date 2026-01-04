roxy_clean <- function() {
  unlink("man", recursive = TRUE)
  dir.create("man", showWarnings = FALSE)
  roxygen2::roxygenize()
}

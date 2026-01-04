#' Cleans the manual files for roxygen2
#'
#' @description The function deletes the manual files for the package and then recreates them using \code{roxygen2::roxygenize()}
#' @usage roxy_clean()
#'
#'
#'
#'
#'
#'
#'
#' @return The function returns nothing
#'
#' @author
#' Chitran Ghosal <ghosal.chitran@gmail.com>
#'
#' @examples
#' \dontrun{
#' roxy_clean()
#' }
#'
#'
#'
#' @export
roxy_clean <- function() {
  unlink("man", recursive = TRUE)
  dir.create("man", showWarnings = FALSE)
  roxygen2::roxygenize()
}

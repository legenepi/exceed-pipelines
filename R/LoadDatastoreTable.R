#' LoadDatastoreTable
#'
#' Load a table from datastore
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadDatastoreTable class
#' @importFrom R6 R6Class
#' @export
LoadDatastoreTable <- R6::R6Class(
  "LoadDatastoreTable",
  inherit = exceedapi::Step,

  private = list(
    .source = NULL,
    .dataset = NULL
  ),

  public = list(
    transform = function(.data, .refresh_cache = FALSE, ...) {
      self$client$datastore(
        source = self$args$source,
        dataset = self$args$dataset
      ) %>%
        collect(.refresh_cache = .refresh_cache)
    }
  )
)

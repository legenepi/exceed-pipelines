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

  public = list(
    get_dataset = function(source, dataset, ...) {
      message(glue::glue("{cli::symbol$bullet} datastore: {source}/{dataset}"))
      self$client$datastore(source = source, dataset = dataset) %>%
        collect(.concat = isTRUE(self$args$concat), ...)
    },

    transform = function(.data, ...) {
      self$get_dataset(
        source = self$args$source,
        dataset = self$args$dataset,
        ...
      )
    }
  )
)

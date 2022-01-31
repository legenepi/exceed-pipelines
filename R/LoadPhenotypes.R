#' LoadPhenotypes
#'
#' Load phenotypes defined by Leicester GenEpi group.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadPhenotypes class
#' @importFrom R6 R6Class
LoadPhenotypes <- R6::R6Class(
  "LoadPhenotypes",
  inherit = exceedapi::Step,

  private = list(
    .source = "exceed",
    .dataset = "phenotypes",

    get_dataset = function() {
      id <- self$client$datastore(
        source = private$.source,
        dataset = private$.dataset
      ) %>%
        collect() %>%
        purrr::pluck("id")

      self$client$datastore(
        source = private$.source,
        dataset = private$.dataset,
        id = id
      ) %>%
        collect()
    }
  ),

  public = list(
    transform = function(...) {
      private$get_dataset()
    }
  )
)

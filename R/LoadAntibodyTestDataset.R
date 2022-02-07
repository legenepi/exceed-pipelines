#' LoadAntibodyTestDataset
#'
#' Load antibody test dataset.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadAntibodyTestDataset class
#' @importFrom R6 R6Class
#' @export
LoadAntibodyTestDataset <- R6::R6Class(
  "LoadAntibodyTestDataset",
  inherit = exceedapi::Step,

  private = list(
    .source = "thriva",
    .dataset = NULL,

    get_dataset = function(dataset) {
      self$client$datastore(
        source = private$.source,
        dataset = dataset,
        quiet = self$quiet
      ) %>%
        collect() %>%
        purrr::pluck("id") %>%
        purrr::map(function(id) {
          self$client$datastore(source = private$.source, dataset = dataset, id = id) %>%
            collect()
        }) %>%
        dplyr::bind_rows()
    }
  ),

  public = list(
    initialize = function(pipeline, dataset, ...) {
      private$.dataset <- paste("antibodytest", dataset, sep = "-")
      super$initialize(pipeline, ...)
    },

    transform = function(...) {
      private$get_dataset(private$.dataset)
    }
  )
)

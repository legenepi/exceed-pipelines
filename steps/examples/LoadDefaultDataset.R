LoadDefaultDataset <- R6::R6Class(
  "LoadDefaultDataset",
  inherit = Step,

  private = list(
    .dataset = NULL
  ),

  public = list(
    initialize = function(pipeline, dataset, ...) {
      super$initialize(pipeline, ...)
      private$.dataset <- dataset
    },

    transform = function(...) {
      dplyr::as_tibble(private$.dataset)
    }
  )
)


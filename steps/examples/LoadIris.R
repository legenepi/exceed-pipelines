#' Load "Iris" dataset

LoadIris <- R6::R6Class(
  "LoadIris",
  inherit = DefaultDataset,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, dataset = datasets::iris, ...)
    }
  )
)

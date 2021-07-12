#' Load "faithful" dataset

LoadFaithful <- R6::R6Class(
  "LoadFaithful",
  inherit = DefaultDataset,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, dataset = datasets::faithful, ...)
    }
  )
)

#' Load "mtcars" dataset

LoadMotorTrendCars <- R6::R6Class(
  "LoadMotorTrendCars",
  inherit = DefaultDataset,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, dataset = datasets::mtcars, ...)
    }
  )
)

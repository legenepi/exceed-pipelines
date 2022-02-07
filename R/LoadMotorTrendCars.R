#' LoadMotorTrendCars
#'
#' Load mtcars default dataset. For demonstration purposes only.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadMotorTrendCars class
#' @importFrom R6 R6Class
#' @export
LoadMotorTrendCars <- R6::R6Class(
  "LoadMotorTrendCars",
  inherit = LoadDefaultDataset,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, dataset = datasets::mtcars, ...)
    }
  )
)

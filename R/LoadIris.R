#' LoadIris
#'
#' Load iris default dataset. For demonstration purposes only.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadIris class
#' @importFrom R6 R6Class
LoadIris <- R6::R6Class(
  "LoadIris",
  inherit = LoadDefaultDataset,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, dataset = datasets::iris, ...)
    }
  )
)

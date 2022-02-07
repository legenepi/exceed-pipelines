#' LoadFaithful
#'
#' Load faithful default dataset. For demonstration purposes only.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadFaithful class
#' @importFrom R6 R6Class
#' @export
LoadFaithful <- R6::R6Class(
  "LoadFaithful",
  inherit = LoadDefaultDataset,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, dataset = datasets::faithful, ...)
    }
  )
)

#' LoadDefaultDataset
#'
#' Load default datasets from the dataset package.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadDefaultDataset class
#' @importFrom R6 R6Class
#' @export
LoadDefaultDataset <- R6::R6Class(
  "LoadDefaultDataset",
  inherit = exceedapi::Step,

  public = list(
    transform = function(...) {
      tibble::as_tibble(self$args$dataset)
    }
  )
)


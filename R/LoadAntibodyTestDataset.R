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
  inherit = LoadDatastoreTable,

  private = list(
    .source = "thriva",
    .dataset_prefix = "antibodytest-"
  ),

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, concat = TRUE, ...)
    },

    get_dataset = function(source, dataset, ...) {
      super$get_dataset(
        source = private$.source,
        dataset = paste0(private$.dataset_prefix, dataset),
        ...
      )
    }
  )
)

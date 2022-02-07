#' LoadNHSNumbers
#'
#' Load NHS numbers
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadNHSNumbers class
#' @importFrom R6 R6Class
#' @export
LoadNHSNumbers <- R6::R6Class(
  "LoadNHSNumbers",
  inherit = LoadDatastoreTable,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, source = "nhs", dataset = "nhs-numbers", ...)
    },

    transform = function(.data, ...) {
      super$transform(...) %>%
        filter(score >= 15, pmi_nhs_number_status == 5) %>%
        select(exceed_id, nhs_no) %>%
        distinct()
    }
  )
)

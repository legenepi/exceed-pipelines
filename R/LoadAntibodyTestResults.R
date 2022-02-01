#' LoadAntibodyTestResults
#'
#' Load antibody test results.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadAntibodyTestResults class
#' @importFrom R6 R6Class
LoadAntibodyTestResults <- R6::R6Class(
  "LoadAntibodyTestResults",
  inherit = exceedapi::Step,

  private = list(
    get_results = function(.refresh_cache = FALSE, ...) {
      results <- self$client$pipeline() %>%
        add_step(LoadAntibodyTestDataset, dataset = "results") %>%
        collect(.refresh_cache = .refresh_cache)

      specimen_ids <- self$client$pipeline() %>%
        add_step(LoadAntibodyTestDataset, dataset = "specimen-ids") %>%
        rename(
          specimenId = Tests_Sample_ID,
          subjectId = Tests_External_User_ID
        ) %>%
        collect(.refresh_cache = .refresh_cache)

      results %>%
        select(-subjectId) %>%
        dplyr::left_join(specimen_ids, by = "specimenId")
    }
  ),

  public = list(
    transform = function(...) {
      private$get_results(...)
    }
  )
)

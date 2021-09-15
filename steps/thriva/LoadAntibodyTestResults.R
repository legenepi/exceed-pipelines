#' LoadAntibodyTestResults - pipeline step for loading antibody test datasets
LoadAntibodyTestResults <- R6::R6Class(
  "LoadAntibodyTestResults",
  inherit = Step,

  private = list(
    get_results = function() {
      results <- self$client$pipeline() %>%
        add_step(LoadAntibodyTestDataset, dataset = "results") %>%
        collect()

      specimen_ids <- self$client$pipeline() %>%
        add_step(LoadAntibodyTestDataset, dataset = "specimen-ids") %>%
        rename(
          specimenId = Tests_Sample_ID,
          subjectId = Tests_External_User_ID
        ) %>%
        collect()

      results %>%
        select(-subjectId) %>%
        left_join(specimen_ids, by = "specimenId")
    }
  ),

  public = list(
    transform = function(...) {
      private$get_results()
    }
  )
)

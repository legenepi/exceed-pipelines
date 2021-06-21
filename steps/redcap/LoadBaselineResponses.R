#' LoadBaselineResponses - pipeline step for loading baseline
#' questionnaire responses. This step is designed to load responses from all
#' baseline questionnaires and combines them into a single data frame.
LoadBaselineResponses <- R6::R6Class(
  "LoadBaselineResponses", inherit = Step,

  public = list(

    #' load responses from a single REDCap survey
    fetch_responses = function(name) {

      self$logger$info("loading responses project=%s", name)

      project <- self$client$redcap(
        name,
        parse_factors = TRUE,
        parse_survey_fields = TRUE
      )

      # extract the name of survey instrument
      forms <- project %>%
        metadata() %>%
        pluck("info") %>%
        pluck("forms")

      responses <- project %>%
        rename_all(~ str_replace(., paste0(forms[1], "_"), "")) %>%
        rename(exceed_id = exceed_study_id) %>%
        private$apply_steps() %>%
        collect()

      self$logger$info(
        "retrieved %d responses project=%s", nrow(responses), name
      )

      return(responses)
    },

    #' transform method - loads and combines all baseline survey responses
    transform = function(...) {
      baseline <- self$client$redcap() %>%
        src_tbls() %>%
        str_subset("^scq") %>%
        map_dfr(self$fetch_responses)
    }
  )
)


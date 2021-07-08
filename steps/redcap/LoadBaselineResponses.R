#' LoadBaselineResponses - pipeline step for loading baseline
#' questionnaire responses. This step is designed to load responses from all
#' baseline questionnaires and combine them into a single data frame.
LoadBaselineResponses <- R6::R6Class(
  "LoadBaselineResponses",
  inherit = LoadSurveyResponses,

  private = list(
    get_responses = function(name, .exec) {
      responses <- super$get_responses(name) %>%
        rename(exceed_id = exceed_study_id) %>%
        private$apply_steps(.exec) %>%
        .exec()

      self$logger$info("received %d responses from %s", nrow(responses), name)

      return(responses)
    }
  ),

  public = list(
    transform = function(.data, .exec, ...) {
      private$redcap() %>%
        src_tbls() %>%
        str_subset("^scq") %>%
        map_dfr(private$get_responses, .exec=.exec, ...)
    }
  )
)


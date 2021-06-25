#' LoadBaselineResponses - pipeline step for loading baseline
#' questionnaire responses. This step is designed to load responses from all
#' baseline questionnaires and combine them into a single data frame.
LoadBaselineResponses <- R6::R6Class(
  "LoadBaselineResponses",
  inherit = LoadSurveyResponses,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, ...)
      self$add_step(RenameStep, exceed_id = exceed_study_id)
    },

    #' transform method - loads and combines all baseline survey responses
    transform = function(...) {
      private$redcap() %>%
        src_tbls() %>%
        str_subset("^scq") %>%
        map_dfr(private$get_responses)
    }
  )
)


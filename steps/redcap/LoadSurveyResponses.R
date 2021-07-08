#' LoadSurveyResponses - pipeline step for loading survey responses from any
#' questionnaire.
LoadSurveyResponses <- R6::R6Class(
  "LoadSurveyResponses",
  inherit = Step,

  private = list(
    .project = NULL,
    .parse_factors = TRUE,
    .parse_survey_fields = TRUE,

    # create redcap object
    redcap = function(project = NULL) {
      self$client$redcap(
        project = project,
        parse_factors = private$.parse_factors,
        parse_survey_fields = private$.parse_survey_fields
      )
    },

    #' load responses from a questionnaire
    get_responses = function(name, ...) {
      self$logger$info("loading responses project=%s", name)

      project <- private$redcap(name)

      # extract the name of survey instrument
      forms <- project %>%
        metadata() %>%
        pluck("info") %>%
        pluck("forms")

      responses <- project %>%
        rename_all(~ str_replace(., paste0(forms[1], "_"), ""))

      return(responses)
    }
  ),

  public = list(
    initialize = function(
      pipeline,
      project = NULL,
      parse_factors = TRUE,
      parse_survey_fields = TRUE,
      ...
    ) {
      private$.project <- project
      private$.parse_factors <- parse_factors
      private$.parse_survey_fields <- parse_survey_fields
      super$initialize(pipeline, ...)
    },

    transform = function(...) {
      private$get_responses(private$.project)
    }
  )
)

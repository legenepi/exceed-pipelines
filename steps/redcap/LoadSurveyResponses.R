#' LoadSurveyResponses - pipeline step for loading survey responses from any
#' questionnaire.
LoadSurveyResponses <- R6::R6Class(
  "LoadSurveyResponses",
  inherit = Step,

  private = list(
    .project = NULL,

    # create redcap object
    redcap = function(project = NULL) {
      self$client$redcap(
        project = project,
        snapshot = self$snapshot,
        parse_factors = self$args$parse_factors,
        parse_survey_fields = self$args$parse_survey_fields
      )
    },

    #' load responses from a questionnaire
    get_responses = function(project) {
      self$logger$info("loading responses project=%s", project)

      project <- private$redcap(project)

      # extract the name of survey instrument
      forms <- project %>%
        metadata() %>%
        purrr::pluck("info") %>%
        purrr::pluck("forms")

      responses <- project %>%
        dplyr::rename_all(~ stringr::str_replace(., paste0(forms[1], "_"), ""))

      return(responses)
    }
  ),

  active = list(
    project = function() { private$.project }
  ),

  public = list(
    initialize = function(pipeline, project = NULL, parse_factors = TRUE, parse_survey_fields = TRUE, ...) {
      super$initialize(
        pipeline,
        parse_factors = !!parse_factors,
        parse_survey_fields = !!parse_survey_fields,
        ...
      )
      private$.project <- project
    },

    transform = function(...) {
      private$get_responses(project = private$.project)
    }
  )
)

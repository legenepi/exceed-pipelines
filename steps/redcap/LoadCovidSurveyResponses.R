#' LoadCovidSurveyResponses - pipeline step for loading survey responses from
#' COVID questionnaire.
LoadCovidSurveyResponses <- R6::R6Class(
  "LoadCovidSurveyResponses",
  inherit = LoadSurveyResponses,

  private = list(
    date_fields = c("s1_5b")
  ),

  public = list(
    initialize = function(pipeline, version = NULL, ...) {
      private$project <- paste(c("covid", version), collapse = "-")
      super$initialize(pipeline, ...)
    }
  )
)


#' LoadCovidResponses - pipeline step for loading survey responses from
#' COVID questionnaire.
LoadCovidResponses <- R6::R6Class(
  "LoadCovidResponses",
  inherit = LoadSurveyResponses,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, project="covid", ...)
    }
  )
)


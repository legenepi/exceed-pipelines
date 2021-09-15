#' LoadCovidSurveyResponses - pipeline step for loading survey responses from
#' COVID questionnaire.
LoadCovidSurveyResponses <- R6::R6Class(
  "LoadCovidSurveyResponses",
  inherit = LoadSurveyResponses,

  public = list(
    initialize = function(pipeline, version = NULL, ...) {
      super$initialize(
        pipeline,
        project = paste(c("covid", version), collapse = "-"),
        ...
      )
    }
  )
)


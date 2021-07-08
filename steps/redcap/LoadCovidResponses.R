#' LoadCovidResponses - pipeline step for loading survey responses from
#' COVID questionnaire.
LoadCovidResponses <- R6::R6Class(
  "LoadCovidResponses",
  inherit = LoadSurveyResponses,

  public = list(
    initialize = function(pipeline, version = NULL, ...) {
      project <- "covid"
      if (!is.null(version))
        project <- paste(project, version, sep = "-")
      super$initialize(pipeline, project = project, ...)
    }
  )
)


#' LoadAntibodyTestResponses - pipeline step for loading survey responses from
#' antibody test questionnaire.
LoadAntibodyTestResponses <- R6::R6Class(
  "LoadAntibodyTestResponses",
  inherit = LoadSurveyResponses,

  public = list(
    initialize = function(pipeline, version = NULL, ...) {
      project <- "antibodytest"
      if (!is.null(version))
        project <- paste(project, version, sep = "-")
      super$initialize(pipeline, project = project, ...)
    }
  )
)


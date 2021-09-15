#' LoadAntibodyTestSurveyResponses - pipeline step for loading survey responses
#' from antibody test questionnaire.
LoadAntibodyTestSurveyResponses <- R6::R6Class(
  "LoadAntibodyTestSurveyResponses",
  inherit = LoadSurveyResponses,

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, project = "antibodytest", ...)
    }
  )
)


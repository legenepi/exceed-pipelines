#' LoadAntibodyTestSurveyResponses - pipeline step for loading survey responses
#' from antibody test questionnaire.
LoadAntibodyTestSurveyResponses <- R6::R6Class(
  "LoadAntibodyTestSurveyResponses",
  inherit = LoadSurveyResponses,

  private = list(
    project = "antibodytest",
    parse_dates = c(
      "a7",
      "b4",
      "b6",
      "b10",
      "b12",
      "c3_anti_pos_date",
      "c3_other_pos_date",
      "c4"
    )
  )
)


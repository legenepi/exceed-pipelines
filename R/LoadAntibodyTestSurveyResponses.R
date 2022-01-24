#' LoadAntibodyTestSurveyResponses
#'
#' Load responses from antibody test questionnaire.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadAntibodyTestSurveyResponses class
#' @importFrom R6 R6Class
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


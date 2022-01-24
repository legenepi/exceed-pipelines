#' LoadResearchProfessionalCollectedResponses
#'
#' Load responses collected by research professionals.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadResearchProfessionalCollectedResponses class
#' @importFrom R6 R6Class
LoadResearchProfessionalCollectedResponses <- R6::R6Class(
  "LoadResearchProfessionalCollectedResponses",
  inherit = LoadSurveyResponses,

  private = list(
    project = "rpq-transfer",
    parse_dates = c("date_of_birth")
  )
)

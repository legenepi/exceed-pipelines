#' LoadResearchProfessionalCollectedResponses - pipeline step for loading
#' responses collected by research professionals for baseline questionnaire
LoadResearchProfessionalCollectedResponses <- R6::R6Class(
  "LoadResearchProfessionalCollectedResponses",
  inherit = LoadSurveyResponses,

  private = list(
    project = "rpq-transfer",
    parse_dates = c("date_of_birth")
  )
)

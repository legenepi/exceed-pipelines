#' LoadCovidSurveyResponses
#'
#' Load survey responses from COVID questionnaires.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadCovidSurveyResponses class
#' @importFrom R6 R6Class
LoadCovidSurveyResponses <- R6::R6Class(
  "LoadCovidSurveyResponses",
  inherit = LoadSurveyResponses,

  private = list(
    project = "covid",
    parse_dates = c("s1_5b")
  ),

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, ...)
      if (!is.null(self$args$version)) {
        private$project <- paste(
          c(private$project, as.integer(self$args$version)),
          collapse = "-v"
        )
      }
    }
  )
)


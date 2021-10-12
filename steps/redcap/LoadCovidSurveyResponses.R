#' LoadCovidSurveyResponses - pipeline step for loading survey responses from
#' COVID questionnaire.
LoadCovidSurveyResponses <- R6::R6Class(
  "LoadCovidSurveyResponses",
  inherit = LoadSurveyResponses,

  private = list(
    project = "covid",
    date_fields = c("s1_5b")
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


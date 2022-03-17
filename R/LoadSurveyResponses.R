#' LoadSurveyResponses
#'
#' Load survey responses from REDCap questionnaires.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadSurveyResponses class
#' @importFrom R6 R6Class
#' @export
LoadSurveyResponses <- R6::R6Class(
  "LoadSurveyResponses",
  inherit = exceedapi::Step,

  private = list(
    project = NULL,
    parse_dates = NULL,

    # create redcap object
    redcap = function(project = NULL) {
      self$client$redcap(
        project = project,
        snapshot = self$snapshot,
        parse_dates = private$parse_dates,
        parse_factors = self$args$parse_factors,
        parse_survey_fields = self$args$parse_survey_fields
      )
    }
  ),

  public = list(
    initialize = function(
      pipeline,
      parse_factors = TRUE,
      parse_survey_fields = TRUE,
      ...
    ) {
      super$initialize(
        pipeline,
        parse_factors = !!parse_factors,
        parse_survey_fields = !!parse_survey_fields,
        ...
      )
    },

    transform = function(.data, .collect, ...) {
      project <- ifelse(
        is.null(self$args$project),
        private$project,
        self$args$project
      )

      responses <- self$get_responses(
        project = project,
        .collect = .collect,
        ...
      )
      private$apply_steps(responses, .collect)
    },

    # load responses from a questionnaire
    get_responses = function(project, .collect, ...) {
      self$logger$info("loading responses project=%s", project)

      pb <- self$progress_bar(total = 1)
      pb$message(glue::glue("{cli::symbol$bullet} redcap: {project}"))

      project <- private$redcap(project)

      # extract the name of survey instrument
      forms <- project %>%
        metadata() %>%
        purrr::pluck("info") %>%
        purrr::pluck("forms")

      project %>%
        dplyr::rename_all(~ stringr::str_replace(., paste0(forms[1], "_"), ""))
    }
  )
)

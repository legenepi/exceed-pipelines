#' LoadSurveyResponses - pipeline step for loading survey responses from any
#' questionnaire.
LoadSurveyResponses <- R6::R6Class(
  "LoadSurveyResponses",
  inherit = Step,

  private = list(
    project = NULL,
    date_fields = NULL,

    # create redcap object
    redcap = function(project = NULL) {
      self$client$redcap(
        project = project,
        snapshot = self$snapshot,
        parse_factors = self$args$parse_factors,
        parse_survey_fields = self$args$parse_survey_fields
      )
    },

    convert_date_fields = function(.data, .collect) {
      if (is.null(private$date_fields))
        return(.data)

      date_fields <- names(purrr::keep(.data$vars, ~ . %in% private$date_fields))

      .data %>%
        .collect() %>%
        mutate(across(date_fields, ~ lubridate::ymd(.)))
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
      responses <- self$get_responses(
        project = private$project,
        collect = .collect
      )

      responses %>%
        private$apply_steps(.collect) %>%
        private$convert_date_fields(.collect)
    },

    #' load responses from a questionnaire
    get_responses = function(project, collect) {
      self$logger$info("loading responses project=%s", project)

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

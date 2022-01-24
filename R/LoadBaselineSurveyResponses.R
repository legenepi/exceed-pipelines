#' LoadBaselineSurveyResponses
#'
#' Load survey responses from baseline questionnaires.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadBaselineSurveyResponses class. This step load responses
#' from all baseline questionnaires and combine them into a single data frame.
#' @importFrom R6 R6Class
LoadBaselineSurveyResponses <- R6::R6Class(
  "LoadBaselineSurveyResponses",
  inherit = LoadSurveyResponses,

  private = list(
    parse_dates = c("gen1"),

    normalize_exceed_id = function(.data) {
      .vars <- names(purrr::keep(self$vars, ~ . == "exceed_study_id"))
      if (!length(.vars))
        return(.data)

      mutate(
        .data,
        across(.vars, ~ stringr::str_pad(., 6, pad = 0))
      )
    },

    normalize_gen1 = function(.data) {
      .vars <- names(purrr::keep(self$vars, ~ . == "gen1"))
      if (!length(.vars))
        return(.data)

      date_range <- seq.Date(
        from = lubridate::ymd("19000101"),
        to = lubridate::today() - lubridate::years(18),
        by = "day"
      )

      mutate(
        .data,
        across(.vars, ~ dplyr::case_when(. %in% date_range ~ .))
      )
    },

    normalize_gen2 = function(.data) {
      .vars <- names(purrr::keep(self$vars, ~ . == "gen2"))
      if (!length(.vars))
        return(.data)

      mutate(
        .data,
        across(.vars, function(.x) {
          gen2_levels <- list(Male = "Man", Female = "Woman")
          if (is.factor(.x) & all(gen2_levels %in% levels(.x)))
            forcats::fct_recode(
              .x,
              Male = gen2_levels$Male,
              Female = gen2_levels$Female
            )
          else
            return(.x)
        })
      )
    }
  ),

  public = list(
    get_responses = function(project, .collect, ...) {
      responses <- super$get_responses(project = project, .collect = .collect, ...) %>%
        dplyr::rename(exceed_id = exceed_study_id) %>%
        private$apply_steps(.collect) %>%
        .collect(...)

      responses <- responses %>%
        private$normalize_exceed_id() %>%
        private$normalize_gen1() %>%
        private$normalize_gen2()

      self$logger$info("received %d responses from %s", nrow(responses), project)

      return(responses)
    },

    transform = function(.data, ...) {
      tables <- private$redcap() %>%
        src_tbls() %>%
        stringr::str_subset("^scq")

      pb <- self$progress_bar(total = length(tables))
      tables %>%
        purrr::map_dfr(function(project) {
          pb$message(glue::glue("{cli::symbol$bullet} {project}"))
          responses <- self$get_responses(project = project, ...)
          pb$tick()
          return(responses)
        })
    }
  )
)

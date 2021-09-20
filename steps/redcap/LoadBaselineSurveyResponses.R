#' LoadBaselineSurveyResponses - pipeline step for loading baseline
#' questionnaire responses. This step is designed to load responses from all
#' baseline questionnaires and combine them into a single data frame.
LoadBaselineSurveyResponses <- R6::R6Class(
  "LoadBaselineSurveyResponses",
  inherit = LoadSurveyResponses,

  private = list(
    date_fields = c("gen1"),

    normalize_gen1 = function(.data, .vars) {
      .vars <- names(keep(.vars, ~ . == "gen1"))
      if (!length(.vars))
        return(.data)

      date_range <- seq.Date(
        from = lubridate::ymd("19000101"),
        to = lubridate::today() - lubridate::years(18),
        by = "day"
      )

      mutate(
        .data,
        across(.vars, ~ case_when(. %in% date_range ~ .))
      )
    },

    normalize_gen2 = function(.data, .vars) {
      .vars <- names(keep(.vars, ~ . == "gen2"))
      if (!length(.vars))
        return(.data)

      mutate(
        .data,
        across(.vars, function(.x) {
          gen2_levels <- list(Male = "Man", Female = "Woman")
          if (is.factor(.x) & all(gen2_levels %in% levels(.x)))
            fct_recode(
              .x,
              Male = gen2_levels$Male,
              Female = gen2_levels$Female
            )
          else
            return(.x)
        })
      )
    },

    get_responses = function(project, collect) {
      responses <- super$get_responses(project = project, collect = collect) %>%
        dplyr::rename(exceed_id = exceed_study_id) %>%
        private$apply_steps(collect)

      responses <- responses %>%
        private$convert_date_fields(.collect = collect) %>%
        private$normalize_gen1(responses$vars) %>%
        private$normalize_gen2(responses$vars)

      self$logger$info("received %d responses from %s", nrow(responses), project)

      return(responses)
    }
  ),

  public = list(
    transform = function(.data, .collect, ...) {
      tables <- private$redcap() %>%
        src_tbls() %>%
        stringr::str_subset("^scq")

      pb <- self$progress_bar(total = length(tables))
      tables %>%
        purrr::map_dfr(function(project) {
          pb$message(glue::glue("{cli::symbol$bullet} {project}"))
          responses <- private$get_responses(
            project = project,
            collect = .collect,
            ...
          )
          pb$tick()
          return(responses)
        })
    }
  )
)

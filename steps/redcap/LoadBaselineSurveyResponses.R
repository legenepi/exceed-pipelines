#' LoadBaselineSurveyResponses - pipeline step for loading baseline
#' questionnaire responses. This step is designed to load responses from all
#' baseline questionnaires and combine them into a single data frame.
LoadBaselineSurveyResponses <- R6::R6Class(
  "LoadBaselineSurveyResponses",
  inherit = LoadSurveyResponses,

  private = list(

    normalize_gen1 = function(.data) {
      mutate(
        .data,
        gen1 = lubridate::ymd(gen1),
        gen1 = case_when(
          lubridate::year(gen1) %in% seq(1900, 2003) ~ gen1
        )
      )
    },

    normalize_gen2 = function(.data) {
      .data %>%
        mutate(
          across(
            gen2,
            function(.x) {
              gen2_levels <- list(Male = "Man", Female = "Woman")
              if (is.factor(.x) & all(gen2_levels %in% levels(.x)))
                fct_recode(
                  .x,
                  Male = gen2_levels$Male,
                  Female = gen2_levels$Female
                )
              else
                return(.x)
            }
          )
        )
    },

    get_responses = function(project, .exec) {
      responses <- super$get_responses(project = project) %>%
        private$normalize_gen1() %>%
        private$normalize_gen2() %>%
        dplyr::rename(exceed_id = exceed_study_id) %>%
        private$apply_steps(.exec) %>%
        .exec()

      self$logger$info("received %d responses from %s", nrow(responses), project)

      return(responses)
    }
  ),

  public = list(
    transform = function(.data, .exec, ...) {
      tables <- private$redcap() %>%
        src_tbls() %>%
        stringr::str_subset("^scq")

      pb <- self$progress_bar(total = length(tables))
      tables %>%
        purrr::map_dfr(function(project) {
          pb$message(glue::glue("- baseline responses: {project}"))
          responses <- private$get_responses(project = project, .exec=.exec, ...)
          pb$tick()
          return(responses)
        })
    }
  )
)

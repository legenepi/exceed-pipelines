#' LoadBaselineResponses - pipeline step for loading baseline
#' questionnaire responses. This step is designed to load responses from all
#' baseline questionnaires and combine them into a single data frame.
LoadBaselineResponses <- R6::R6Class(
  "LoadBaselineResponses",
  inherit = LoadSurveyResponses,

  private = list(
    get_responses = function(name, .exec) {
      responses <- super$get_responses(name) %>%
        dplyr::rename(exceed_id = exceed_study_id) %>%
        private$apply_steps(.exec) %>%
        .exec()

      self$logger$info("received %d responses from %s", nrow(responses), name)

      return(responses)
    }
  ),

  public = list(
    transform = function(.data, .exec, ...) {
      tables <- private$redcap() %>%
        src_tbls() %>%
        stringr::str_subset("^scq")

      pb <- progress::progress_bar$new(total = length(tables))
      pb$tick(0)
      tables %>%
        purrr::map_dfr(function(name) {
          pb$message(glue::glue("- baseline responses: {name}"))
          responses <- private$get_responses(name, .exec=.exec, ...)
          pb$tick()
          return(responses)
        })
    }
  )
)


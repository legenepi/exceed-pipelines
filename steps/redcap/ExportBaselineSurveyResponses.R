#' ExportBaselineSurveyResponses - pipeline step for preparing export of
#' baseline survey
ExportBaselineSurveyResponses <- R6::R6Class(
  "ExportBaselineSurveyResponses",
  inherit = Step,

  public = list(
    transform = function(...) {
      self$client$pipeline() %>%
        add_step(LoadBaselineSurveyResponses) %>%
        add_step(
          ConvertToKilograms,
          unit = "mea1",
          kilograms = "mea1a",
          stones = "mea1b",
          pounds = "mea1c",
          into = "mea1a"
        ) %>%
        add_step(
          ConvertToCentimeters,
          unit = "mea2",
          centimeters = "mea2a",
          inches = "mea2b",
          into = "mea2a"
        ) %>%
        add_step(
          ConvertToCentimeters,
          unit = "mea3",
          centimeters = "mea3a",
          feet = "mea3b",
          inches = "mea3c",
          into = "mea3a"
        ) %>%
        collect() %>%
        mutate(mea1a = ifelse(mea1a >= 30 & mea1a <= 250, mea1a, NA)) %>%
        mutate(mea2a = ifelse(mea2a >= 10 & mea2a <= 200, mea2a, NA)) %>%
        mutate(mea3a = ifelse(mea3a >= 120 & mea3a <= 220, mea3a, NA))
    }
  )
)

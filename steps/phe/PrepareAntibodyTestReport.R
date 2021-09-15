#' PrepareAntibodyTestReport - pipeline step for preparing antibody test report.
PrepareAntibodyTestReport <- R6::R6Class(
  "PrepareAntibodyTestReport",
  inherit = GenericExport,

  private = list(
    .config = NULL,

    resolve_duplicates = function(.x) {
      .x <- purrr::discard(.x, is.na)
      if (n_distinct(.x) == 1)
        return(first(.x))
      else
        return(NA)
    },

    get_profiles = function() {
      profiles <- self$client$profiles()
      return(profiles)
    },

    calculate_age_group = function(age) {
      cut(
        age,
        breaks = c(-Inf, 35, 50, 70, Inf),
        labels = c("18 - 34", "35 - 49", "50 - 69", "70+")
      )
    },

    get_baseline_responses = function() {
      baseline <- self$client$pipeline() %>%
        add_step(LoadBaselineSurveyResponses) %>%
        select(exceed_id, timestamp, complete, dob = gen1, gender = gen2, gen5) %>%
        filter(complete == 2) %>%
        add_step(GroupEthnicities, from = "gen5", into = "ethnicity") %>%
        add_step(CalculateAge, from = "dob", to = "timestamp", into = "age") %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        filter(!is.na(uuid)) %>%
        collect()

      baseline %>%
        group_by(uuid) %>%
        mutate(postcode = "ABC") %>%
        mutate(ageGroup = private$calculate_age_group(age)) %>%
        mutate(across(c(ageGroup, gender, ethnicity), private$resolve_duplicates)) %>%
        distinct(uuid, ageGroup, gender, ethnicity, postcode)
    },

    get_results = function() {
      profiles <- private$get_profiles()

      baseline <- private$get_baseline_responses()

      results <- self$client$pipeline() %>%
        add_step(LoadAntibodyTestResults) %>%
        add_step(MergeUUIDs, domain = "thriva", by = "subjectId") %>%
        collect()

      results %>%
        left_join(baseline, by = "uuid")
    },

    prepare_data = function() {
      filename <- self$make_filename(
        prefix = "exceed",
        suffix = "csv",
        key = "results"
      )

      results <- private$get_results() %>%
        mutate(survey = "EXCEED") %>%
        select(
          survey,
          gender,
          ageGroup,
          postcode,
          ethnicity,
          testType,
          accessionDate,
          specimenProcessedDate,
          performingLabCode,
          analyser,
          resultValueCOVG,
          resultValueUnitOfMeasureCOVG,
          testResultCOVG,
          resultValueCOVS,
          resultValueUnitOfMeasureCOVS,
          testResultCOVS
        )
      return(results)
      self$write_csv(filename)

      return(self$summary)
    }
  ),

  public = list(
    transform = function(...) {
      private$prepare_data()
    }
  )
)


#' PrepareAntibodyTestReport class
#'
#' Prepare antibody test report.
#'
#' @docType class
#' @format An R6 class object.
#' @description PrepareAntibodyTestReport class
#' @importFrom R6 R6Class
PrepareAntibodyTestReport <- R6::R6Class(
  "PrepareAntibodyTestReport",
  inherit = GenericExport,

  private = list(
    resolve_duplicates = function(.x) {
      .x <- purrr::discard(.x, is.na)
      if (n_distinct(.x) == 1)
        return(first(.x))
      else
        return(NA)
    },

    get_profiles = function() {
      cli::cli_h1("Profiles")

      profiles <- self$client$pipeline() %>%
        add_step(LoadProfiles) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        select(uuid, postcode = primaryaddress__address__postcode) %>%
        collect()

      profiles %>%
        group_by(uuid) %>%
        mutate(across(c(postcode), private$resolve_duplicates)) %>%
        distinct(uuid, postcode)
    },

    lookup_postcodes = function(postcodes) {
      cli::cli_h1("Postcodes")

      self$client$pipeline() %>%
        add_step(LookupPostcodes, postcodes = postcodes) %>%
        select(postcode = query, postcode_outcode = result_outcode) %>%
        collect()
    },

    calculate_age_group = function(age) {
      cut(
        age,
        breaks = c(-Inf, 35, 50, 70, Inf),
        labels = c("18-34", "35-49", "50-69", "70+")
      )
    },

    get_baseline_responses = function() {
      cli::cli_h1("Baseline responses")

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
        mutate(ageGroup = private$calculate_age_group(age)) %>%
        group_by(uuid) %>%
        mutate(across(c(ageGroup, gender, ethnicity), private$resolve_duplicates)) %>%
        distinct(uuid, ageGroup, gender, ethnicity)
    },

    get_results = function() {
      cli::cli_h1("Test results")

      results <- self$client$pipeline() %>%
        add_step(LoadAntibodyTestResults) %>%
        add_step(MergeUUIDs, domain = "thriva", by = "subjectId") %>%
        collect()

      profiles <- private$get_profiles()
      baseline <- private$get_baseline_responses()

      results <- results %>%
        left_join(profiles, by = "uuid") %>%
        left_join(baseline, by = "uuid")

      postcodes <- private$lookup_postcodes(results$postcode)

      results %>%
        left_join(postcodes, by = "postcode") %>%
        select(-postcode) %>%
        rename(postcode = postcode_outcode)
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

      results %>%
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


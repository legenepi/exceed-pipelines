#' LoadDemographicProfiles
#'
#' Combine demographic data from various sources.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadDemographicProfiles class
#' Pipeline step for combining demographic data from various sources.
#' Current sources include:
#' - e-consent
#' - baseline survey (self-reported)
#' - research professional collected data
#' - electronic health records (primary care)
#' @importFrom R6 R6Class
LoadDemographicProfiles <- R6::R6Class(
  "LoadDemographicProfiles",
  inherit = exceedapi::Step,

  private = list(
    coalesce = function(data, col, func = NULL, slicer = dplyr::slice_min) {
      if (is.null(func))
        func <- function(.x) {
          dplyr::case_when(
            dplyr::n_distinct(.x) == 1 ~ .x
          )
        }

      data %>%
        filter(!is.na({{col}})) %>%
        dplyr::group_by(uuid) %>%
        mutate(across({{col}}, func)) %>%
        slicer(order_by = {{col}}, with_ties = FALSE) %>%
        filter(!is.na({{col}})) %>%
        dplyr::distinct(across(c(uuid, {{col}})))
    },

    coalesce_date = function(data, col, threshold = Inf, ...) {
      private$coalesce(data, {{col}}, func = function(.x) {
        dplyr::case_when(
          dplyr::n_distinct(.x) == 1 | diff(range(.x)) <= threshold ~ .x
        )},
        ...
      )
    },

    get_dataset = function(step, .collect, ...) {
      self$client$pipeline() %>%
        add_step({{step}}) %>%
        select(exceed_id, ...) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        .collect() %>%
        filter(!is.na(uuid))
    },

    get_profiles = function(.collect, ...) {
      profiles <- private$get_dataset(
        LoadProfiles,
        .collect,
        dob = birth_date,
        deceased,
        consent_date = basicconsent__date,
        consent_version = basicconsent__version,
        consent_withdrawn = basicconsent__withdrawals__scope,
        consent_withdrawn_date = basicconsent__withdrawals__date_effective,
        postcode = primaryaddress__address__postcode,
        ...
      ) %>%
        mutate(postcode = stringr::str_replace_all(postcode, " ", ""))

      postcode <- private$coalesce(profiles, postcode)
      deceased <- private$coalesce(profiles, deceased, any)
      consent <- private$coalesce(profiles, consent_date, min)
      consent_version <- private$coalesce(profiles, consent_version, max)
      consent_withdrawn <- private$coalesce(profiles, consent_withdrawn, max)
      consent_withdrawn_date <- private$coalesce(profiles, consent_withdrawn_date, min)

      profiles <- profiles %>%
        select(uuid, dob) %>%
        dplyr::left_join(deceased, by = "uuid") %>%
        dplyr::left_join(postcode, by = "uuid") %>%
        dplyr::left_join(consent, by = "uuid") %>%
        dplyr::left_join(consent_version, by = "uuid") %>%
        dplyr::left_join(consent_withdrawn, by = "uuid") %>%
        dplyr::left_join(consent_withdrawn_date, by = "uuid")

      drop_withdrawn <- self$args$drop_withdrawn

      if (isFALSE(drop_withdrawn))
        drop_withdrawn <- NULL
      else if (isTRUE(drop_withdrawn))
        drop_withdrawn <- seq(1,3)
      else if (is.null(drop_withdrawn))
        drop_withdrawn <- 3

      if (!is.null(drop_withdrawn))
        profiles <- filter(profiles, !(consent_withdrawn %in% drop_withdrawn))

      return(profiles)
    },

    get_primarycare_data = function(.collect) {
      private$get_dataset(
        LoadPrimaryCarePatients,
        .collect,
        dob = pseudo_dob,
        sex = gender
      )
    },

    get_survey_responses = function(step, .collect, ...) {
      private$get_dataset(
        {{step}},
        .collect,
        timestamp,
        complete,
        ...
      ) %>%
        filter(complete == 2)
    },

    get_postcodes = function(.collect) {
      self$client$pipeline() %>%
        add_step(LoadPostcodes) %>%
        select(
          postcode = pcd7,
          ccg = ccg21cd,
          lsoa = lsoa11cd,
          msoa = msoa11cd
        ) %>%
        .collect() %>%
        mutate(postcode = stringr::str_replace_all(postcode, " ", ""))
    }
  ),

  public = list(
    transform = function(.data, .collect, ...) {
      .collect <- purrr::partial(.collect, ...)

      profiles <- private$get_profiles(.collect)
      primarycare <- private$get_primarycare_data(.collect)
      postcodes <- private$get_postcodes(.collect)

      baseline <- private$get_survey_responses(
        LoadBaselineSurveyResponses,
        .collect,
        dob = gen1,
        sex = gen2,
        ethnicity = gen5
      )

      rpcollected <- private$get_survey_responses(
        LoadResearchProfessionalCollectedResponses,
        .collect,
        dob = date_of_birth,
        sex
      )

      dob <- dplyr::bind_rows(profiles, baseline, rpcollected, primarycare) %>%
        private$coalesce_date(dob, threshold = lubridate::years(1))

      sex <- dplyr::bind_rows(baseline, rpcollected, primarycare) %>%
        mutate(sex = forcats::fct_drop(sex)) %>%
        private$coalesce(sex)

      ethnicity <- private$coalesce(baseline, ethnicity)

      profiles %>%
        select(-dob) %>%
        dplyr::distinct(uuid, .keep_all = TRUE) %>%
        dplyr::arrange(uuid) %>%
        dplyr::left_join(dob, by = "uuid") %>%
        dplyr::left_join(sex, by = "uuid") %>%
        dplyr::left_join(ethnicity, by = "uuid") %>%
        dplyr::left_join(postcodes, by = "postcode") %>%
        select(-postcode)
    }
  )
)

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
#' @export
LoadDemographicProfiles <- R6::R6Class(
  "LoadDemographicProfiles",
  inherit = exceedapi::Step,

  private = list(
    coalesce = function(data, col, func = NULL, slicer = NULL) {
      if (is.null(func))
        func <- function(.x) {
          dplyr::case_when(
            dplyr::n_distinct(.x) == 1 ~ .x
          )
        }

      if (is.null(slicer)) {
        slicer <- function(.x) {
          dplyr::slice_min(.x, order_by = {{col}}, with_ties = FALSE)
        }
      }

      data %>%
        filter(!is.na({{col}})) %>%
        dplyr::group_by(uuid) %>%
        mutate(across({{col}}, func)) %>%
        slicer() %>%
        filter(!is.na({{col}})) %>%
        dplyr::distinct(across(c(uuid, {{col}})))
    },

    coalesce_date = function(data, col, threshold = Inf, ...) {
      private$coalesce(
        data,
        {{col}},
        func = function(.x) {
          dplyr::case_when(
            dplyr::n_distinct(.x) == 1 | diff(range(.x)) <= threshold ~ .x
          )
        },
        slicer = dplyr::slice_head,
        ...
      )
    },

    get_dataset = function(step, .collect, ..., snapshot = NULL) {
      self$client$pipeline(snapshot = snapshot) %>%
        add_step({{step}}) %>%
        select(exceed_id, ...) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        .collect() %>%
        filter(!is.na(uuid))
    },

    get_profiles = function(.collect, ...) {

      exclude_withdrawn <- self$args$exclude_withdrawn

      if (isFALSE(exclude_withdrawn))
        exclude_withdrawn <- NULL
      else if (isTRUE(exclude_withdrawn))
        exclude_withdrawn <- seq(1,3)
      else if (is.null(exclude_withdrawn))
        exclude_withdrawn <- 3

      profiles <- private$get_dataset(
        LoadProfiles,
        .collect,
        deceased,
        dob = birth_date,
        consent_date = basicconsent__date,
        consent_version = basicconsent__version,
        consent_withdrawn = basicconsent__withdrawals__scope,
        consent_withdrawn_date = basicconsent__withdrawals__date_effective,
        ccg = primaryaddress__address__ccg,
        lsoa = primaryaddress__address__lsoa,
        msoa = primaryaddress__address__msoa,
        ...
      )

      ccg <- private$coalesce(profiles, ccg)
      lsoa <- private$coalesce(profiles, lsoa)
      msoa <- private$coalesce(profiles, msoa)
      deceased <- private$coalesce(profiles, deceased, any)
      consent <- private$coalesce(profiles, consent_date, min)
      consent_version <- private$coalesce(profiles, consent_version, max)
      consent_withdrawn <- private$coalesce(profiles, consent_withdrawn, max)
      consent_withdrawn_date <- private$coalesce(profiles, consent_withdrawn_date, min)

      profiles <- profiles %>%
        select(uuid, dob) %>%
        dplyr::left_join(deceased, by = "uuid") %>%
        dplyr::left_join(ccg, by = "uuid") %>%
        dplyr::left_join(lsoa, by = "uuid") %>%
        dplyr::left_join(msoa, by = "uuid") %>%
        dplyr::left_join(consent, by = "uuid") %>%
        dplyr::left_join(consent_version, by = "uuid") %>%
        dplyr::left_join(consent_withdrawn, by = "uuid") %>%
        dplyr::left_join(consent_withdrawn_date, by = "uuid")

      if (!is.null(exclude_withdrawn))
        profiles <- filter(profiles, !(consent_withdrawn %in% exclude_withdrawn))

      return(profiles)
    },

    get_primarycare_data = function(.collect) {
      private$get_dataset(
        LoadPrimaryCarePatients,
        snapshot = "2019-08-12",
        .collect,
        dob = pseudo_dob,
        sex = gender
      )
    },

    get_survey_responses = function(
      step,
      .collect,
      ...,
      snapshot = NULL
    ) {
      responses <- private$get_dataset(
        {{step}},
        snapshot = snapshot,
        .collect,
        timestamp,
        complete,
        ...
      )

      exclude_incomplete_surveys <- TRUE
      if (!is.null(self$args$exclude_incomplete_surveys))
        exclude_incomplete_surveys <- self$args$exclude_incomplete_surveys

      if (exclude_incomplete_surveys)
        responses <- filter(responses, complete == 2)

      return(responses)
    }
  ),

  public = list(
    transform = function(.data, .collect, ...) {
      .collect <- purrr::partial(.collect, ...)

      profiles <- private$get_profiles(.collect)
      primarycare <- private$get_primarycare_data(.collect)

      if (!is.null(self$args$pseudo_dob_offset)) {
        primarycare <- primarycare %>%
          mutate(
            dob = dob + lubridate::days(self$args$pseudo_dob_offset)
          )
      }

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
        select(uuid, dob)

      profiles <- dplyr::select(profiles, -dob)

      if (self$args$allow_duplicates) {
        dob <- dplyr::filter(dob, !is.na(uuid), !is.na(dob))

        profiles <- profiles %>%
          dplyr::left_join(dob, by = "uuid") %>%
          dplyr::distinct(uuid, dob, .keep_all = TRUE)
      } else {
        dob <- dob %>%
          private$coalesce_date(dob, threshold = lubridate::years(1))

        profiles <- profiles %>%
          dplyr::left_join(dob, by = "uuid") %>%
          dplyr::distinct(uuid, .keep_all = TRUE)
      }

      sex <- dplyr::bind_rows(baseline, rpcollected, primarycare) %>%
        dplyr::mutate(sex = forcats::fct_drop(sex)) %>%
        private$coalesce(sex)

      ethnicity <- private$coalesce(baseline, ethnicity)

      profiles %>%
        dplyr::left_join(sex, by = "uuid") %>%
        dplyr::left_join(ethnicity, by = "uuid") %>%
        dplyr::arrange(uuid)
    }
  )
)

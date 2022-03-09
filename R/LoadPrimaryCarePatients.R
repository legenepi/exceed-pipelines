#' LoadPrimaryCarePatients
#'
#' Load patient data from primary care tables in electronic health records.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadPrimaryCarePatients class
#' @importFrom R6 R6Class
#' @export
LoadPrimaryCarePatients <- R6::R6Class(
  "LoadPrimaryCarePatients",
  inherit = exceedapi::Step,

  private = list(
    exceed_id_length = 6
  ),

  public = list(
    transform = function(.data, .collect, ...) {
      snapshot <- ifelse(is.null(self$snapshot), "2019-08-12", self$snapshot)

      primarycare_data <- self$client$ehr("primarycare", snapshot = snapshot)

      message(glue::glue("{cli::symbol$bullet} primarycare"))

      patients <- primarycare_data %>%
        tbl("Patient") %>%
        .collect(...)

      patient_practice <- primarycare_data %>%
        tbl("PatientPractice") %>%
        .collect(...)

      patients %>%
        dplyr::left_join(patient_practice, by = c(id = "patient_id")) %>%
        mutate(
          exceed_id = stringr::str_sub(
            stringr::str_pad(exceed_id, private$exceed_id_length, pad = "0"),
            -private$exceed_id_length,
            -1
          ),
          pseudo_dob = lubridate::ymd(pseudo_dob),
          gender = forcats::fct_recode(gender, Female = "F", Male = "M")
        )
    }
  )
)

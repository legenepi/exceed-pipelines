#' LoadPrimaryCarePatients - pipeline step for loading patient data from
#' primary care tables in electronic health records
LoadPrimaryCarePatients <- R6::R6Class(
  "LoadPrimaryCarePatients",
  inherit = exceedapi::Step,

  public = list(
    transform = function(.data, .collect, ...) {
      snapshot <- ifelse(is.null(self$snapshot), "2018-12-12", self$snapshot)

      primarycare_data <- self$client$ehr("primarycare", snapshot = snapshot)

      patient <- primarycare_data %>%
        tbl("Patient") %>%
        .collect(...)

      patient_practice <- primarycare_data %>%
        tbl("PatientPractice") %>%
        .collect(...)

      patient %>%
        left_join(patient_practice, by = c(id = "patient_id")) %>%
        mutate(
          pseudo_dob = lubridate::ymd(pseudo_dob),
          gender = fct_recode(gender, Female = "F", Male = "M")
        )
    }
  )
)

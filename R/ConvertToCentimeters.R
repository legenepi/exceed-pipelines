#' ConvertToCentimeters
#'
#' Convert length to centimeters from imperial units (feet and inches)
#'
#' @docType class
#' @format An R6 class object.
#' @description ConvertToCentimeters class
#' @importFrom R6 R6Class
#' @export
ConvertToCentimeters <- R6::R6Class(
  "ConvertToCentimeters",
  inherit = exceedapi::Step,

  private = list(
    extract_measurement = function(.data, .var) {
      if (is.null(.var))
        return(rep(0, nrow(.data)))

      dplyr::pull(.data, .var) %>%
        stringr::str_replace_all("[^0-9.]", "") %>%
        na_if("") %>%
        replace_na("0") %>%
        as.double()
    },

    convert = function(unit, centimeters, feet, inches) {
      unit <- as.numeric(unit)

      dplyr::case_when(
        unit == 1 ~ centimeters,
        unit == 2 ~ round(((feet * 12) + inches) * 2.54, digits = 2)
      ) %>%
        na_if(0)
    },

    convert_to_centimeters = function(.data, ...) {
      unit <- dplyr::pull(.data, self$args$unit)
      centimeters <- private$extract_measurement(.data, self$args$centimeters)
      feet <- private$extract_measurement(.data, self$args$feet)
      inches <- private$extract_measurement(.data, self$args$inches)

      .data %>%
        mutate(
          !!self$args$centimeters := private$convert(
            unit = unit,
            centimeters = centimeters,
            feet = feet,
            inches = inches
          )
        )
    }
  ),

  public = list(
    transform = function(.data, ...) {
      private$convert_to_centimeters(.data)
    }
  )
)


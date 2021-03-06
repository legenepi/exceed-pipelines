#' ConvertToKilograms
#'
#' Convert mass to kilograms from imperial units (pounds and stones)
#'
#' @docType class
#' @format An R6 class object.
#' @description ConvertToKilograms class
#' @importFrom R6 R6Class
#' @export
ConvertToKilograms <- R6::R6Class(
  "ConvertToKilograms",
  inherit = exceedapi::Step,

  private = list(
    extract_measurement = function(.data, .var) {
      dplyr::pull(.data, .var) %>%
        stringr::str_replace_all("[^0-9.]", "") %>%
        na_if("") %>%
        replace_na("0") %>%
        as.double()
    },

    convert = function(unit, kilograms, stones, pounds) {
      unit <- as.numeric(unit)

      dplyr::case_when(
        unit == 1 ~ kilograms,
        unit == 2 ~ round(((stones*14) + pounds) * 0.45359237, digits = 2)
      ) %>%
        na_if(0)
    },

    convert_to_kilograms = function(.data, ...) {
      unit <- dplyr::pull(.data, self$args$unit)
      kilograms <- private$extract_measurement(.data, self$args$kilograms)
      stones <- private$extract_measurement(.data, self$args$stones)
      pounds <- private$extract_measurement(.data, self$args$pounds)

      .data %>%
        mutate(
          !!self$args$kilograms := private$convert(
            unit = unit,
            kilograms = kilograms,
            stones = stones,
            pounds = pounds
          )
        )
    }
  ),

  public = list(
    transform = function(.data, ...) {
      private$convert_to_kilograms(.data)
    }
  )
)


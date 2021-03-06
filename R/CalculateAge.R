#' CalculateAge
#'
#' Calculate the age based on either today's date or a reference date from
#' a variable.
#'
#' @docType class
#' @format An R6 class object.
#' @description CalculateAge class
#' @importFrom R6 R6Class
#' @export
CalculateAge <- R6::R6Class(
  "CalculateAge",
  inherit = exceedapi::Step,

  private = list(
    as_date = function(x) {
      if (lubridate::is.Date(x))
        return(x)
      if (lubridate::is.POSIXt(x))
        return(lubridate::date(x))
      return(NA)
    },

    calculate_age = function(.data, from, to, into, unit, ...) {
      from <- dplyr::pull(.data, from)
      if (is.null(to))
        to <- lubridate::today()
      else
        to <- dplyr::pull(.data, to)

      to <- private$as_date(to)
      from <- private$as_date(from)

      if (is.null(unit))
        unit <- lubridate::years(1)

      .data %>%
        mutate(
          !!into := NA,
          across(
            into,
            function(.x) {
              lubridate::as.period(to - from) / unit
            }
          )
        )
    }
  ),

  public = list(
    transform = function(.data, ...) {
      private$calculate_age(
        .data,
        from = self$args$from,
        to = self$args$to,
        into = self$args$into,
        unit = self$args$unit
      )
    }
  )
)


#' NormalizeDateTime class
#'
#' NormalizeDateTime - pipeline step for normalizing EXCEED ID
#'
#' @docType class
#' @format An R6 class object.
#' @description NormalizeDateTime
NormalizeDateTime <- R6::R6Class(
  "NormalizeDateTime",
  inherit = exceedapi::Step,

  public = list(
    transform = function(.data, ...) {
      .data %>%
        mutate(
          across(
            !!private$.args$col,
            ~ lubridate::ymd(.)
          )
        )
    }
  )
)

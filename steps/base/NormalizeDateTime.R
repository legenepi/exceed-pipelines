#' NormalizeDateTime - pipeline step for normalizing EXCEED ID
NormalizeDateTime <- R6::R6Class(
  "NormalizeDateTime",
  inherit = Step,

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

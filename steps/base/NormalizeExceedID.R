#' NormalizeExceedID - pipeline step for normalizing EXCEED ID
NormalizeExceedID <- R6::R6Class(
  "NormalizeExceedID",
  inherit = Step,

  public = list(
    transform = function(.data, ...) {
      .data %>%
        mutate(
          exceed_id = dplyr::case_when(
            nchar(stringr::str_trim(exceed_id)) > 0 ~ stringr::str_pad(
              exceed_id, width = 6, pad = 0
            )
          )
        )
    }
  )
)

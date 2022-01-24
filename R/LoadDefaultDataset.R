#' LoadDefaultDataset - load builtin default datasets
LoadDefaultDataset <- R6::R6Class(
  "LoadDefaultDataset",
  inherit = exceedapi::Step,

  public = list(
    transform = function(...) {
      dplyr::as_tibble(self$args$dataset)
    }
  )
)


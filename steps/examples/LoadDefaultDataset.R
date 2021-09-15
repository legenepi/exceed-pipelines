LoadDefaultDataset <- R6::R6Class(
  "LoadDefaultDataset",
  inherit = Step,

  public = list(
    transform = function(...) {
      dplyr::as_tibble(self$args$dataset)
    }
  )
)


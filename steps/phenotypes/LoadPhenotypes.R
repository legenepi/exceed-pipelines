#' LoadPhenotypes - pipeline step for loading phenotype dataset
LoadPhenotypes <- R6::R6Class(
  "LoadPhenotypes",
  inherit = Step,

  private = list(
    .source = "exceed",
    .dataset = "phenotypes",

    get_dataset = function() {
      id <- self$client$datastore(
        source = private$.source,
        dataset = private$.dataset
      ) %>%
        collect() %>%
        pluck("id")

      self$client$datastore(
        source = private$.source,
        dataset = private$.dataset,
        id = id
      ) %>%
        collect()
    }
  ),

  public = list(
    transform = function(...) {
      private$get_dataset()
    }
  )
)

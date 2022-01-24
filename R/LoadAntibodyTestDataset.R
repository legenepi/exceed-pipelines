#' LoadAntibodyTestDataset - pipeline step for loading antibody test datasets
LoadAntibodyTestDataset <- R6::R6Class(
  "LoadAntibodyTestDataset",
  inherit = exceedapi::Step,

  private = list(
    .source = "thriva",
    .dataset = NULL,

    get_dataset = function(dataset) {
      self$client$datastore(
        source = private$.source,
        dataset = dataset,
        quiet = self$quiet
      ) %>%
        collect() %>%
        pluck("id") %>%
        purrr::map(function(id) {
          self$client$datastore(source = private$.source, dataset = dataset, id = id) %>%
            collect()
        }) %>%
        bind_rows()
    }
  ),

  public = list(
    initialize = function(pipeline, dataset, ...) {
      private$.dataset <- paste("antibodytest", dataset, sep = "-")
      super$initialize(pipeline, ...)
    },

    transform = function(...) {
      private$get_dataset(private$.dataset)
    }
  )
)

#' ExportAntibodyTestResults - pipeline step for exporting antibody test results
ExportAntibodyTestResults <- R6::R6Class(
  "ExportAntibodyTestResults",
  inherit = Step,

  private = list(
    prepare_metadata = function() {
      metadata <- readr::read_csv(
        here::here(self$args$metadata),
        show_col_types = FALSE
      ) %>%
        mutate(variable = paste(self$args$prefix, variable, sep = "_"))

      # add any extra fields
      for (field in self$args$parent$config$metadata$fields) {
        metadata <- metadata %>%
          add_row(
            variable = field$name,
            type = field$type,
            description = field$description,
            .before = field$position
          )
      }

      rename_with(metadata, str_to_upper)
    },

    get_fields = function() {
      self$args$fields %>%
        map(~ .$name) %>%
        unlist()
    },

    prepare_dataset = function(fields) {
      dataset <- self$client$pipeline() %>%
        add_step(LoadAntibodyTestResults) %>%
        add_step(MergeUUIDs, domain = "thriva", by = "subjectId") %>%
        select(uuid, fields) %>%
        collect() %>%
        mutate(specimenProcessedDate = lubridate::date(specimenProcessedDate)) %>%
        left_join(self$args$identities, by = "uuid") %>%
        filter(!is.na(uuid) & !is.na(STUDY_ID)) %>%
        relocate(STUDY_ID) %>%
        select(-uuid)

      dataset %>%
        rename_with(function(col) {
          paste(self$args$prefix, col, sep = "_")
        }, .cols = fields)
    }
  ),

  public = list(
    transform = function(...) {
      metadata <- private$prepare_metadata()
      fields <- private$get_fields()
      dataset <- private$prepare_dataset(fields)

      metadata_filename <- self$args$parent$write_csv(
        metadata,
        paste(self$args$name, "metadata", sep = "_")
      )
      dataset_filename <- self$args$parent$write_csv(dataset, self$args$name)

      tibble(
        table = self$args$name,
        variables = n_distinct(metadata$VARIABLE),
        observations = nrow(dataset),
        dataset = dataset_filename,
        metadata = metadata_filename
      )
    }
  )
)

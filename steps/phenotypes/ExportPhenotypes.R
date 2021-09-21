#' ExportPhenotypes - pipeline step for exporting antibody test results
ExportPhenotypes <- R6::R6Class(
  "ExportPhenotypes",
  inherit = Step,

  private = list(
    get_fields_to_exclude = function() {
      fields_exclude <- self$args$fields %>%
        map(~ {
          if (.x$exclude)
            return(.x$name)
        }) %>%
        unlist()
    },

    prepare_metadata = function() {
      metadata <- readr::read_csv(
        here::here(self$args$metadata),
        show_col_types = FALSE
      ) %>%
        filter(!(variable %in% private$get_fields_to_exclude())) %>%
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

    prepare_dataset = function(metadata) {

      floats <- metadata %>%
        filter(TYPE == "DOUBLE") %>%
        pull("VARIABLE")

      dataset <- self$client$pipeline() %>%
        add_step(LoadPhenotypes) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        collect() %>%
        left_join(self$args$identities, by = "uuid") %>%
        filter(!is.na(uuid) & !is.na(STUDY_ID)) %>%
        relocate(STUDY_ID) %>%
        select(-uuid, -private$get_fields_to_exclude())

      dataset %>%
        rename_with(function(col) {
          paste(self$args$prefix, col, sep = "_")
        }, .cols = -c("STUDY_ID")) %>%
        mutate(across(floats, ~ round(as.double(.), digits = 2))) %>%
        select(metadata$VARIABLE)
    }
  ),

  public = list(
    transform = function(...) {
      metadata <- private$prepare_metadata()
      dataset <- private$prepare_dataset(metadata)

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

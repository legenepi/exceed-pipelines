#' ExportOccupations - pipeline step for exporting occupation table
ExportOccupations <- R6::R6Class(
  "ExportOccupations",
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
      fields <- self$args$fields %>%
        map(~ .$name) %>%
        unlist()
    },

    prepare_dataset = function(fields) {
      field_map <- fields %>%
        map(~ paste("user__occupation", ., sep = "__")) %>%
        set_names(fields) %>%
        unlist()

      dataset <- self$client$pipeline() %>%
        add_step(LoadProfiles) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        select(uuid, !!field_map) %>%
        collect() %>%
        mutate(across(fields, ~ na_if(str_replace_all(., "\\}", ""), ""))) %>%
        left_join(self$args$identities, by = "uuid") %>%
        filter(!is.na(uuid) & !is.na(STUDY_ID)) %>%
        group_by(STUDY_ID) %>%
        filter(row_number() == 1)  %>%
        relocate(STUDY_ID) %>%
        select(-uuid)

      dataset %>%
        rename_with(function(col) {
          paste(self$args$prefix, str_replace(col, "occupation__", ""), sep = "_")
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

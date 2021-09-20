#' ExportSurvey - pipeline step for preparing survey exports.
ExportSurvey <- R6::R6Class(
  "ExportSurvey",
  inherit = Step,

  private = list(
    prepare_metadata = function() {
      metadata <- self$client$pipeline() %>%
        add_step(
          LoadSurveyMetadata,
          field_types = private$get_field_types(),
          !!!self$args
        ) %>%
        select(
          field_name,
          field_basename,
          field_type,
          field_label,
          field_value,
          field_value_label
        ) %>%
        collect()

      metadata <- metadata %>%
        select(
          variable = field_name,
          field = field_basename,
          description = field_label,
          type = field_type,
          value = field_value,
          label = field_value_label
        )

      # handle any field overrides from the config
      metadata <- metadata %>%
        mutate(type = private$get_field_type_overrides(variable, type))

      # build field type map
      field_map <- self$args$parent$config$redcap$fields %>%
        map(~ {
          intersect(.x$input, metadata$type)
        }) %>%
        set_names(map(self$args$parent$config$redcap$fields, pluck("type")))

      # map field types to (database compatible) export types
      metadata <- metadata %>%
        mutate(
          type = fct_collapse(type, !!!field_map),
          variable = paste(
            self$args$prefix,
            variable,
            sep = "_"
          )
        )

      # add any extra fields
      for (field in self$args$parent$config$metadata$fields) {
        metadata <- metadata %>%
          add_row(
            variable = field$name,
            description = field$description,
            type = field$type,
            .before = field$position
          )
      }

      rename_with(
        metadata,
        str_to_upper,
        .cols = c("variable", "description", "type", "value", "label")
      )
    },

    prepare_dataset = function(fields) {
      dataset <- self$client$pipeline() %>%
        add_step(!!self$args$exporters$dataset) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        collect() %>%
        select(uuid, timestamp, complete, fields) %>%
        filter(complete == 2, !is.na(uuid)) %>%
        left_join(self$args$identities, by = "uuid") %>%
        filter(!is.na(uuid) & !is.na(STUDY_ID)) %>%
        group_by(STUDY_ID) %>%
        arrange(STUDY_ID, timestamp) %>%
        filter(row_number() == 1)  %>%
        relocate(STUDY_ID) %>%
        select(-c(uuid, timestamp, complete))

      dataset <- dataset %>%
        mutate(across(everything(), function(x) {
          if (is.factor(x))
            as.numeric(x)
          else
            return(x)
        })) %>%
          rename_with(function(col) {
            paste(self$args$prefix, col, sep = "_")
          }, .cols = fields)
    },

    get_survey_fields = function(metadata) {
      metadata %>%
        filter(!is.na(field)) %>%
        distinct(field) %>%
        pull()
    },

    get_field_types = function() {
      self$args$parent$config$redcap$fields %>%
        map(~ {
          if (is.null(.x$exclude) || .x$exclude == FALSE)
            return(.x$input)
        }) %>%
        unlist()
    },

    get_field_type_overrides = function(name, type) {
      if (is.null(self$args$fields))
        return(type)

      field_types <- self$args$fields %>%
        map_dfr(~ tibble(name = .x$name, type = .x$type))

      if (!("type" %in% names(field_types)))
        return(type)

      field_types <- field_types %>%
        deframe %>%
        as.list()

      map2(name, type, function(.x, .y) {
        index = str_which(.x, names(field_types))
        if (length(index))
          return(field_types[[index]])
        else
          return(.y)
      }) %>%
        unlist()
    }
  ),

  public = list(
    transform = function(...) {
      metadata <- private$prepare_metadata()
      fields <- private$get_survey_fields(metadata)
      dataset <- private$prepare_dataset(fields)

      metadata_filename <- self$args$parent$write_csv(
        select(metadata, -field),
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


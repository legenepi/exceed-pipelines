#' ExportTable - generic export of a single table
ExportTable <- R6::R6Class(
  "ExportTable",
  inherit = Step,

  public = list(
    #' get field names
    get_fields = function(exclude = FALSE) {
      map(self$args$fields, ~ {
        field_exclude <- .x$exclude
        if (is.null(field_exclude))
          field_exclude <- FALSE
        if (field_exclude == exclude)
          .x$name
      }) %>%
        unlist()
    },

    #' add any shared metadata from config
    add_shared_metadata = function(metadata) {
      for (field in self$args$parent$config$metadata$fields) {
        metadata <- metadata %>%
          add_row(
            variable = field$name,
            description = field$description,
            type = field$type,
            .before = field$position
          )
      }
      return(metadata)
    },

    add_prefix = function(dataset) {
      dataset %>%
        rename_with(function(col) {
          paste(self$args$prefix, col, sep = "_")
        }, .cols = c(everything(), -STUDY_ID))
    },

    #' field type overrides
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
    },

    #' write metadata to file
    write_metadata = function(metadata, ...) {
      self$args$parent$write_csv(
        metadata %>%
          select(variable, description, type, value, label) %>%
          rename_with(str_to_upper),
        ...
      )
    },

    write_dataset = function(dataset, ...) {
      duplicates <- dataset %>%
        group_by(STUDY_ID) %>%
        tally() %>%
        filter(n > 1)

      if (nrow(duplicates)) {
        cli::cli_alert_danger("exported dataset includes duplicate records")
        print(duplicates)
      }

      self$args$parent$write_csv(dataset, ...)
    },

    write_table = function(metadata, dataset) {
      metadata_filename <- self$write_metadata(
        metadata,
        paste(self$args$name, "metadata", sep = "_")
      )
      dataset_filename <- self$write_dataset(dataset, self$args$name)

      tibble(
        table = self$args$name,
        variables = n_distinct(metadata$variable),
        observations = nrow(dataset),
        dataset = dataset_filename,
        metadata = metadata_filename
      )
    }
  )
)


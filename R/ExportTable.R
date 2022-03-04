#' ExportTable
#'
#' Generic export of a single table.
#'
#' @docType class
#' @format An R6 class object.
#' @description ExportTable class
#' @importFrom R6 R6Class
#' @export
ExportTable <- R6::R6Class(
  "ExportTable",
  inherit = exceedapi::Step,

  public = list(
    # get field names
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

    alert_warning = function(message, success, ...) {
      args <- c(...)
      alert_func <- ifelse(success, cli::cli_alert_success, cli::cli_alert_warning)
      alert_func(message)
      if (!success & !is.null(args))
        alert_func(args)
    },

    alert_danger = function(message, success, ...) {
      args <- c(...)
      alert_func <- ifelse(success, cli::cli_alert_success, cli::cli_alert_danger)
      alert_func(message)
      if (!success & !is.null(args))
        alert_func(args)
    },

    check_study_id_range = function(study_id) {
      fields <- self$args$parent$config$metadata$fields
      fields <- fields %>%
        set_names(map(fields, ~ .$name))

      study_id_range <- unlist(fields$STUDY_ID$range)
      min(study_id) >= min(study_id_range) & max(study_id) <= max(study_id_range)
    },

    # add any shared metadata from config
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
        dplyr::rename_with(function(col) {
          paste(self$args$prefix, col, sep = "_")
        }, .cols = c(everything(), -STUDY_ID))
    },

    # field type overrides
    get_field_type_overrides = function(name, type) {
      if (is.null(self$args$fields))
        return(type)

      field_types <- self$args$fields %>%
        purrr::map_dfr(~ tibble(name = .x$name, type = .x$type))

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

    verify_table = function(metadata, dataset) {
      cli::cli_h3("Verifying table")

      missing_vars <- length(setdiff(metadata$variable, names(dataset)))
      self$alert_danger(
        glue::glue("{missing_vars} missing variables"),
        missing_vars == 0
      )

      factors <- dataset %>%
        select(where(is.factor)) %>%
        ncol()
      self$alert_warning(glue::glue("{factors} factors"), factors == 0)

      character_fields <- metadata %>%
        filter(type == "CHARACTER") %>%
        dplyr::pull(variable) %>%
        unique()
      character_fields_count <- length(character_fields)

      self$alert_warning(
        glue::glue("{character_fields_count} CHARACTER fields in metadata"),
        character_fields_count == 0
      )
      yaml::as.yaml(character_fields) %>%
        cli::cat_line()

      character_fields <- dataset %>%
        select(where(is.character)) %>%
        names()
      character_fields_count <- length(character_fields)

      self$alert_warning(
        glue::glue("{character_fields_count} CHARACTER fields in dataset"),
        character_fields_count == 0
      )
      yaml::as.yaml(character_fields) %>%
        cli::cat_line()

      study_id_var <- "STUDY_ID"
      self$alert_danger(
        glue::glue("{study_id_var} in metadata and dataset"),
        study_id_var %in% names(dataset) & study_id_var %in% metadata$variable
      )

      study_id_dulicates <- dataset %>%
        dplyr::group_by(across(study_id_var)) %>%
        tally() %>%
        filter(n > 1) %>%
        ungroup() %>%
        nrow()

      self$alert_danger(
        glue::glue("{study_id_var} with {study_id_dulicates} duplicates"),
        study_id_dulicates == 0
      )

      study_id <- floor(as.numeric(dataset[[study_id_var]]))
      study_id_range <- paste(range(study_id), collapse = " - ")
      self$alert_danger(
        glue::glue("{study_id_var} range: {study_id_range}"),
        self$check_study_id_range(study_id)
      )

      check_fields <- c(study_id_var)

      if (isTRUE(self$args$check_all_fields)) {
        check_fields <- unique(c(check_fields, names(dataset)))
      }

      purrr::walk(check_fields, function(field) {
        missing_values <- dataset %>%
          filter(if_all(field, ~ is.na(.))) %>%
          mutate(across(study_id_var, ~ floor(as.numeric(.)))) %>%
          select(study_id_var) %>%
          distinct() %>%
          nrow()

        self$alert_danger(
          glue::glue("{field} with {missing_values} NAs"),
          missing_values == 0
        )
      })
    },

    # write metadata to file
    write_metadata = function(metadata, ...) {
      self$args$parent$write_csv(
        metadata %>%
          select(variable, description, type, value, label) %>%
          dplyr::rename_with(str_to_upper),
        ...
      )
    },

    write_dataset = function(dataset, ...) {
      duplicates <- dataset %>%
        dplyr::group_by(STUDY_ID) %>%
        tally() %>%
        filter(n > 1)

      if (nrow(duplicates)) {
        cli::cli_alert_danger("exported dataset includes duplicate records")
        print(duplicates)
      }

      self$args$parent$write_csv(dataset, ...)
    },

    write_table = function(metadata, dataset) {
      self$verify_table(metadata, dataset)

      cli::cli_h3("Saving table")
      metadata_filename <- self$write_metadata(
        metadata,
        paste(self$args$name, "metadata", sep = "_")
      )
      dataset_filename <- self$write_dataset(dataset, self$args$name)

      tibble(
        table = self$args$name,
        variables = dplyr::n_distinct(metadata$variable),
        observations = nrow(dataset),
        dataset = dataset_filename,
        metadata = metadata_filename
      )
    }
  )
)


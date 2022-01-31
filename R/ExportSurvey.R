#' ExportSurvey
#'
#' Export REDCap survey responses
#'
#' @docType class
#' @format An R6 class object.
#' @description ExportSurvey class
#' @importFrom R6 R6Class
ExportSurvey <- R6::R6Class(
  "ExportSurvey",
  inherit = ExportTable,

  public = list(
    transform = function(...) {
      metadata <- self$prepare_metadata()
      dataset <- self$prepare_dataset(metadata)

      self$write_table(metadata, dataset)
    },

    get_field_types = function() {
      self$args$parent$config$redcap$fields %>%
        map(~ {
          if (is.null(.x$exclude) || .x$exclude == FALSE)
            return(.x$input)
        }) %>%
        unlist()
    },

    prepare_metadata = function() {
      metadata <- self$client$pipeline() %>%
        add_step(
          LoadSurveyMetadata,
          field_types = self$get_field_types(),
          fields_include = self$get_fields(exclude = FALSE),
          fields_exclude = self$get_fields(exclude = TRUE),
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
        mutate(type = self$get_field_type_overrides(variable, type))

      # replace exceed with x in field names
      metadata <- metadata %>%
        mutate(variable = str_replace(variable, stringr::regex("exceed", ignore_case = TRUE), "x"))

      # build field type map
      field_map <- self$args$parent$config$redcap$fields %>%
        map(~ {
          intersect(.x$input, metadata$type)
        }) %>%
        set_names(map(self$args$parent$config$redcap$fields, purrr::pluck("type")))

      # map field types to (database compatible) export types
      metadata <- metadata %>%
        mutate(
          type = forcats::fct_collapse(type, !!!field_map),
          variable = paste(
            self$args$prefix,
            variable,
            sep = "_"
          )
        )

      self$add_shared_metadata(metadata)
    },

    prepare_dataset = function(metadata) {
      fields <- metadata %>%
        filter(!is.na(field)) %>%
        dplyr::distinct(field) %>%
        dplyr::pull()

      dataset <- self$client$pipeline() %>%
        add_step(!!self$args$exporter) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        collect() %>%
        select(uuid, timestamp, complete, fields) %>%
        dplyr::rename_with(~ str_replace(., stringr::regex("exceed", ignore_case = TRUE), "x")) %>%
        filter(complete == 2, !is.na(uuid)) %>%
        dplyr::left_join(self$args$identities, by = "uuid") %>%
        filter(!is.na(uuid) & !is.na(STUDY_ID)) %>%
        dplyr::group_by(STUDY_ID) %>%
        dplyr::arrange(STUDY_ID, timestamp) %>%
        filter(dplyr::row_number() == 1)  %>%
        dplyr::ungroup() %>%
        relocate(STUDY_ID) %>%
        select(-c(uuid, timestamp, complete))

      dataset %>%
        mutate(across(where(is.factor), ~ as.numeric(.x))) %>%
        self$add_prefix()
    }
  )
)


#' LoadSurveyMetadata - pipeline step for loading survey metadata from any
#' questionnaire.
LoadSurveyMetadata <- R6::R6Class(
  "LoadSurveyMetadata",
  inherit = Step,

  private = list(
    .survey = NULL,
    .fields = NULL,
    .field_types = NULL,
    .field_exclude = NULL,
    .field_include = NULL,

    #' strip html tags
    strip_tags = function(x) {
      x %>%
        str_replace_all("<.*?>", " ") %>%
        str_squish()
    },

    #' get metadata from a questionnaire
    get_metadata = function(...) {
      self$logger$info("loading metadata survey=%s", private$.survey)

      metadata <- self$client$redcap(project = private$.survey) %>%
        metadata() %>%
        collect()

      if (!is.null(private$.field_types)) {
        if (!is.null(private$.field_include)) {
          metadata <- metadata %>%
            filter(
              field_type %in% private$.field_types |
              str_detect(field_name, paste(private$.field_include, collapse = "|"))
            )
        } else {
          metadata <- metadata %>%
            filter(field_type %in% private$.field_types)
        }
      }

      if (!is.null(private$.field_exclude)) {
        metadata <- metadata %>%
          filter(
            !str_detect(
              field_name, paste(private$.field_exclude, collapse = "|")
            )
          )
      }

      metadata <- metadata %>%
        unnest(field_choices) %>%
        rename(field_value = id, field_value_label = label) %>%
        mutate(
          field_name = case_when(
            field_type == "checkbox" ~ paste(
              field_name, field_value, sep = "___"
            ),
            TRUE ~ field_name
          ),
          field_basename = field_name
        )

      if (!is.null(private$.fields)) {
        metadata <- map2_df(
          names(private$.fields),
          private$.fields,
          private$map_fields,
          metadata
        )
      }

      metadata <- metadata %>%
        mutate(
          field_name = str_to_upper(field_name),
          field_name = str_replace(field_name, regex("_*exceed_*", ignore_case = TRUE), "_"),
          field_name = str_replace(field_name, "_*$", ""),
          field_value = case_when(
            field_type == "checkbox" ~ 1,
            TRUE ~ field_value
          )
        ) %>%
        select(
          field_name,
          field_basename,
          field_label,
          field_value,
          field_value_label
        )

      metadata %>%
        mutate(
          field_label = private$strip_tags(field_label),
          field_value_label = private$strip_tags(field_value_label)
        )
    },

    #' map field attributes
    map_fields = function(name, field, metadata) {
      metadata %>%
        mutate(
          field_label = case_when(
            field_name == name ~ field$label,
            TRUE ~ field_label
          ),
          field_name = case_when(
            field_name == name ~ field$name,
            TRUE ~ field_name
          )
        )
    }
  ),

  public = list(
    initialize = function(
      pipeline,
      survey = NULL,
      fields = NULL,
      field_types = NULL,
      field_exclude = NULL,
      field_include = NULL,
      ...
    ) {
      private$.survey <- survey
      private$.fields <- fields
      private$.field_types <- field_types
      private$.field_exclude <- field_exclude
      private$.field_include <- field_include
      super$initialize(pipeline, ...)
    },

    transform = function(...) {
      private$get_metadata(...)
    }
  )
)

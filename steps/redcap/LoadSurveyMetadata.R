#' LoadSurveyMetadata - pipeline step for loading survey metadata from any
#' questionnaire.
LoadSurveyMetadata <- R6::R6Class(
  "LoadSurveyMetadata",
  inherit = Step,

  private = list(
    #' strip html tags
    strip_tags = function(x) {
      x %>%
        str_replace_all("<.*?>", " ") %>%
        str_squish()
    },

    #' get metadata from a questionnaire
    get_metadata = function() {
      self$logger$info("loading metadata survey=%s", self$args$slug)

      metadata <- self$client$redcap(project = self$args$slug) %>%
        metadata() %>%
        collect()

      if (!is.null(self$args$field_types)) {
        if (!is.null(self$args$fields_include)) {
          metadata <- metadata %>%
            filter(
              field_type %in% self$args$field_types |
              str_detect(
                field_name,
                paste(self$args$fields_include, collapse = "|")
              )
            )
        } else {
          metadata <- metadata %>%
            filter(field_type %in% self$args$field_types)
        }
      }

      if (!is.null(self$args$fields_exclude)) {
        metadata <- metadata %>%
          filter(
            str_detect(
              field_name,
              paste(self$args$fields_exclude, collapse = "|"),
              negate = TRUE
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

      metadata <- metadata %>%
        mutate(
          field_value = case_when(
            field_type == "checkbox" ~ 1,
            TRUE ~ field_value
          )
        ) %>%
        select(
          field_name,
          field_basename,
          field_type,
          field_label,
          field_value,
          field_value_label
        )

      metadata %>%
        mutate(
          field_label = private$strip_tags(field_label),
          field_value_label = private$strip_tags(field_value_label)
        )
    }
  ),

  public = list(
    transform = function(...) {
      private$get_metadata()
    }
  )
)

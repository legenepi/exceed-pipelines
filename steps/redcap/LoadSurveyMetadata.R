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

    #' get metadata from a questionnaire
    get_metadata = function() {
      self$logger$info("loading metadata survey=%s", self$args$slug)

      metadata <- self$client$redcap(project = self$args$slug) %>%
        metadata() %>%
        collect()

      fields_include <- private$get_fields(exclude = FALSE)
      fields_exclude <- private$get_fields(exclude = TRUE)

      if (!is.null(self$args$field_types)) {
        if (!is.null(fields_include)) {
          metadata <- metadata %>%
            filter(
              field_type %in% self$args$field_types |
              str_detect(field_name, paste(fields_include, collapse = "|"))
            )
        } else {
          metadata <- metadata %>%
            filter(field_type %in% self$args$field_types)
        }
      }

      if (!is.null(fields_exclude)) {
        metadata <- metadata %>%
          filter(
            str_detect(
              field_name, paste(fields_exclude, collapse = "|"), negate = TRUE
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

      # TODO: is this even necessary anymore?

      # if (!is.null(self$args$fields)) {
      #   metadata <- map2_df(
      #     names(self$args$fields),
      #     self$args$fields,
      #     private$map_fields,
      #     metadata
      #   )
      # }
      #

      metadata <- metadata %>%
        mutate(
          # field_name = str_to_upper(field_name),
          # field_name = str_replace(field_name, regex("_*exceed", ignore_case = TRUE), "_X"),
          # field_name = str_replace(field_name, "_*$", ""),
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
    transform = function(...) {
      private$get_metadata()
    }
  )
)

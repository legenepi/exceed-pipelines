#' LoadSurveyMetadata
#'
#' Load survey metadata from REDCap questionnaires.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadSurveyMetadata class
#' @importFrom R6 R6Class
LoadSurveyMetadata <- R6::R6Class(
  "LoadSurveyMetadata",
  inherit = exceedapi::Step,

  private = list(
    # strip html tags
    strip_tags = function(x) {
      x %>%
        stringr::str_replace_all("<.*?>", " ") %>%
        stringr::str_squish()
    },

    # get metadata from a questionnaire
    get_metadata = function(slug) {
      self$logger$info("loading metadata survey=%s", slug)

      metadata <- self$client$redcap(project = slug) %>%
        metadata()

      info <- metadata$info
      metadata <- metadata %>%
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
          field_basename = field_name,
          field_name = dplyr::case_when(
            field_type == "checkbox" ~ paste(
              field_name, field_value, sep = "___"
            ),
            TRUE ~ field_name
          )
        )

      metadata <- metadata %>%
        mutate(
          field_value = dplyr::case_when(
            field_type == "checkbox" ~ 1,
            TRUE ~ field_value
          )
        ) %>%
        select(
          field_name,
          field_basename,
          field_type,
          field_label,
          field_note,
          field_value,
          field_value_label
        )

      metadata %>%
        mutate(
          project = slug,
          project_id = info$project_id,
          project_title = info$project_title,
          project_creation_time = info$creation_time,
          field_label = private$strip_tags(field_label),
          field_note = private$strip_tags(field_note),
          field_value_label = private$strip_tags(field_value_label)
        )
    }
  ),

  public = list(
    transform = function(...) {
      pb <- self$progress_bar(total = length(self$args$slug))

      purrr::map_dfr(self$args$slug, function(slug) {
        pb$message(glue::glue("{cli::symbol$bullet} {slug} (metadata)"))
        metadata <- private$get_metadata(slug)
        pb$tick()
        return(metadata)
      })
    }
  )
)

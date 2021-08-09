#' PrepareBreatheExport - pipeline step for preparing BREATHE export.
PrepareBreatheExport <- R6::R6Class(
  "PrepareBreatheExport",
  inherit = GenericExport,

  private = list(
    .config = NULL,
    .domain = "thriva",

    get_identities = function() {
      self$client$identity(domain = "*") %>%
        collect() %>%
        filter(str_detect(domain, private$.domain)) %>%
        select(study_id = pid)
    },

    prepare_metadata = function() {
      metadata <- map_dfr(private$.config$redcap$surveys, private$get_metadata)

      fields <- metadata %>%
        select(survey, field_basename)

      metadata <- metadata %>%
        select(
          variable = field_name,
          description = field_label,
          value = field_value,
          label = field_value_label
        ) %>%
        rename_all(str_to_upper)


      self$summary_append(
        "metadata",
        glue::glue("{rows} x {cols}", rows=nrow(metadata), cols=ncol(metadata))
      )

      filename <- self$make_filename(
        prefix = "exceed_metadata",
        suffix = "csv",
        attribute = "metadata-file"
      )

      list(metadata = metadata, fields = fields, filename = filename)
    },

    get_metadata = function(survey) {
      pipeline <- self$client$pipeline() %>%
        add_step(
          LoadSurveyMetadata,
          survey = survey$slug,
          field_types = private$.config$redcap$vars$types,
          fields = survey$vars,
          field_exclude = survey$exclude,
          field_include = survey$include
        )

      pipeline %>%
        collect() %>%
        mutate(
          field_name = paste(survey$prefix, field_name, sep = "_"),
          survey = survey$name
        )
    },

    get_survey_responses = function(name, step, fields) {
      self$client$pipeline() %>%
        add_step(!!step) %>%
        select(exceed_id, timestamp, complete, fields) %>%
        filter(complete == 2) %>%
        add_step(MergeIdentities, domain = private$.domain, drop_na = FALSE) %>%
        rename(study_id = domain_id) %>%
        collect() %>%
        select(-exceed_id, -timestamp, -complete)
        rename_with(function(col) {
          col <- paste(name, col, sep = "_")
          col %>%
            str_replace(regex("_*exceed_*", ignore_case = TRUE), "_") %>%
            str_replace("_*$", "")
        }, !matches("^study_id$"))
    },

    get_survey_fields = function(metadata, name) {
      metadata %>%
        filter(survey == name) %>%
        distinct(field_basename) %>%
        pull()
    },

    prepare_data = function(metadata) {
      data <- private$get_identities()

      for (survey in private$.config$redcap$surveys) {
        responses <- private$get_survey_responses(
          survey$name,
          survey$step,
          private$get_survey_fields(metadata$fields, survey$name)
        )
        data <- data %>%
          left_join(responses, by = "study_id")
      }

      # data <- data %>%
      #   rename_all(str_to_upper)
      #

      self$summary_append(
        "data",
        glue::glue("{rows} x {cols}", rows=nrow(data), cols=ncol(data))
      )

      filename <- self$make_filename(
        prefix = "exceed_data",
        suffix = "csv",
        attribute = "data-file"
      )

      list(data = data, filename = filename)
    }
  ),

  public = list(
    transform = function(...) {
      private$.config <- yaml::yaml.load_file(
        here::here("projects/breathe/config.yaml")
      )

      metadata <- private$prepare_metadata()
      metadata$metadata %>%
        self$write_csv(metadata$filename)

      data <- private$prepare_data(metadata)
      data$data %>%
        self$write_csv(data$filename)

      self$create_archive(
        private$.output_file,
        files = list(metadata = metadata$filename, data = data$filename)
      )

      arrange(private$.summary, attribute)
    }
  )
)


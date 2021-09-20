#' BreatheExport - pipeline step for preparing BREATHE export.
BreatheExport <- R6::R6Class(
  "BreatheExport",
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
      pb <- progress::progress_bar$new(
        total = length(private$.config$redcap$surveys)
      )
      metadata <- private$.config$redcap$surveys %>%
        map_dfr(function(survey) {
          pb$message(glue::glue("survey metadata: {name}", name = survey$name))
          survey_metadata <- private$get_metadata(survey)
          pb$tick()
          return(survey_metadata)
        })

      fields <- metadata %>%
        select(survey, field_basename)

      metadata <- metadata %>%
        select(
          variable = field_name,
          description = field_label,
          value = field_value,
          label = field_value_label
        )

      purrr::walk(private$.config$metadata$vars, function(v) {
        metadata <- metadata %>%
          add_row(
            variable = v$name,
            description = v$description,
            .before = v$position
          )
      })

      metadata <- metadata %>%
        rename_all(str_to_upper)

      self$summary_append(
        "metadata",
        glue::glue("{rows} x {cols}", rows=nrow(metadata), cols=ncol(metadata))
      )

      filename <- self$make_filename(
        prefix = "exceed_metadata",
        suffix = "csv",
        key = "metadata_file"
      )

      list(metadata = metadata, fields = fields, filename = filename)
    },

    get_metadata = function(survey) {
      self$client$pipeline() %>%
        add_step(
          LoadSurveyMetadata,
          survey = survey$slug,
          field_types = private$.config$redcap$vars$types,
          fields = survey$vars,
          fields_exclude = survey$exclude,
          fields_include = survey$include
        ) %>%
        collect() %>%
        mutate(
          field_name = paste(survey$prefix, field_name, sep = "_"),
          survey = survey$name
        )
    },

    get_survey_responses = function(name, step, fields) {
      self$client$pipeline() %>%
        add_step(!!step, parse_factors = FALSE, parse_survey_fields = TRUE) %>%
        select(exceed_id, timestamp, complete, fields) %>%
        filter(complete == 2) %>%
        add_step(MergeIdentities, domain = private$.domain, drop_na = FALSE) %>%
        rename(study_id = domain_id) %>%
        collect() %>%
        select(-exceed_id, -timestamp, -complete) %>%
        rename_with(function(col) {
          col <- paste(name, col, sep = "_")
          col %>%
            str_replace(regex("_*exceed", ignore_case = TRUE), "_x") %>%
            str_replace("_*$", "")
        }, !matches("^study_id$"))
    },

    get_survey_fields = function(metadata, name) {
      metadata %>%
        filter(survey == name) %>%
        distinct(field_basename) %>%
        pull()
    },

    prepare_data_old = function(metadata) {
      data <- private$get_identities()

      pb <- progress::progress_bar$new(
        total = length(private$.config$redcap$surveys)
      )

      for (survey in private$.config$redcap$surveys) {
        pb$message(glue::glue("survey responses: {name}", name = survey$name))

        responses <- private$get_survey_responses(
          survey$name,
          survey$step,
          private$get_survey_fields(metadata$fields, survey$name)
        )
        data <- data %>%
          left_join(responses, by = "study_id")

        pb$tick()
      }

      sample_size <- 10
      data <- data %>%
        sample_n(sample_size) %>%
        mutate(study_id = sample(seq(270000000, 279999999), sample_size))

      data <- data %>%
        rename_all(str_to_upper)

      self$summary_append(
        "data",
        glue::glue("{rows} x {cols}", rows=nrow(data), cols=ncol(data))
      )

      filename <- self$make_filename(
        prefix = "exceed_data",
        suffix = "csv",
        key = "data_file"
      )

      list(data = data, filename = filename)
    },

    prepare_export = function() {
      identities <- self$client$pipeline() %>%
        add_step(LoadIdentities, domain = "breathe") %>%
        select(uuid, STUDY_ID = pid) %>%
        collect()

      exclusions <- self$client$pipeline() %>%
        add_step(LoadProfiles) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        select(uuid, deceased, scope = basicconsent__withdrawals__scope) %>%
        collect() %>%
        filter(scope >= 2 | deceased == TRUE) %>%
        left_join(identities, by = "uuid")

      identities <- identities %>%
        anti_join(exclusions, by = "uuid")

      self$config$tables %>%
        discard(~ .$skip) %>%
        map_dfr(~ {
          cli::cli_h2("Table: {.x$name}")
          .x$args$name <- .x$name
          self$client$pipeline() %>%
            add_step(
              !!.x$step,
              parent = self,
              identities = identities,
              !!!.x$args
            ) %>%
            collect()
        })
    }
  ),

  public = list(
    transform = function(...) {
      self$load_config(here::here("projects/breathe/config.yaml"))

      return(private$prepare_export())

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

      return(private$.summary)
    }
  )
)


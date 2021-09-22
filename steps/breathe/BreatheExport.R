#' BreatheExport - pipeline step for preparing BREATHE export.
BreatheExport <- R6::R6Class(
  "BreatheExport",
  inherit = GenericExport,

  public = list(
    transform = function(...) {
      cli::cli_h1("BREATHE export")
      self$load_config(here::here("projects/breathe/config.yaml"))

      tables <- self$prepare_export()

      cli::cli_h2("summary")
      tables %>%
        mutate(
          dataset = fs::path_file(dataset),
          metadata = fs::path_file(metadata)
        ) %>%
        select(table, variables, observations) %>%
        print()

      files <- tables %>%
        select(metadata, dataset) %>%
        unlist() %>%
        as.vector()

      self$create_archive(files)

      self$create_manifest(
        files,
        tables,
        template = "projects/breathe/manifest_template.Rmd"
      )

      return(tables)
    },

    prepare_export = function() {
      cli::cli_h2("Identities")
      identities <- self$client$pipeline() %>%
        add_step(LoadIdentities, domain = "breathe") %>%
        select(uuid, STUDY_ID = pid) %>%
        collect()

      cli::cli_h2("Profiles")
      exclusions <- self$client$pipeline() %>%
        add_step(LoadProfiles) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        select(uuid, deceased, scope = basicconsent__withdrawals__scope) %>%
        collect() %>%
        filter(scope >= 2 | deceased == TRUE) %>%
        left_join(identities, by = "uuid")

      identities <- identities %>%
        anti_join(exclusions, by = "uuid")

      cli::cli_h1("Exporting tables")

      self$config$tables %>%
        discard(~ .$skip) %>%
        map_dfr(~ {
          cli::cli_h2(.x$name)
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
  )
)


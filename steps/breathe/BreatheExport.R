#' BreatheExport - pipeline step for preparing BREATHE export.
BreatheExport <- R6::R6Class(
  "BreatheExport",
  inherit = GenericExport,

  public = list(
    transform = function(...) {
      cli::cli_h1("BREATHE export")

      self$show_config()

      tables <- self$prepare_export(domain = "breathe")

      self$create_archive(tables)

      self$create_manifest(tables)

      self$show_summary()

      return(tables)
    }
  )
)


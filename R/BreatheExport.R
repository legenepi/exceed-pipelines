#' BreatheExport
#'
#' Prepare data export for BREATHE.
#'
#' @docType class
#' @format An R6 class object.
#' @description BreatheExport class
#' @importFrom R6 R6Class
BreatheExport <- R6::R6Class(
  "BreatheExport",
  inherit = GenericExport,

  public = list(
    transform = function(...) {
      cli::cli_h1("BREATHE export")

      self$show_config()

      tables <- self$prepare_export(domain = "breathe")

      self$create_archive(tables)

      self$create_manifest(tables, template = self$args$manifest)

      self$create_codebook(tables, template = self$args$codebook)

      self$show_summary()

      return(tables)
    }
  )
)


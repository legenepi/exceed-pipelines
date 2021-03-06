#' LHISExport
#'
#' Prepare data export for LHIS
#'
#' @docType class
#' @format An R6 class object.
#' @description LHISExport class
#' @importFrom R6 R6Class
#' @export
LHISExport <- R6::R6Class(
  "LHISExport",
  inherit = GenericExport,

  public = list(
    transform = function(...) {
      cli::cli_h1("LHIS export")

      self$show_config()

      tables <- self$prepare_export(domain = "lhis")

      self$create_archive(tables)

      self$create_manifest(tables, template = self$args$manifest)

      self$create_report(tables, template = self$args$report)

      self$show_summary()

      return(tables)
    }
  )
)


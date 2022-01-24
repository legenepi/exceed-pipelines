#' CachedCSV
#'
#' Download and cache a CSV file. This class is useful if you need to download
#' CSV files from a remote location.
#'
#' @docType class
#' @format An R6 class object.
#' @description CachedCSV class
#' @importFrom R6 R6Class
CachedCSV <- R6::R6Class(
  "CachedCSV",
  inherit = CachedDownload,

  public = list(
    save = function(key, obj, filename) {
      obj <- readr::read_csv(
        filename,
        show_col_types = self$args$show_col_types
      )
      super$save(key, obj, filename)
    }
  )
)

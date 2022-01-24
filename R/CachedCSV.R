#' CachedCSV - load map boundaryies
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

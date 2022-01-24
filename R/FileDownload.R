#' FileDownload
#'
#' Download a file from a URL.
#'
#' @docType class
#' @format An R6 class object.
#' @description FileDownload class
#' @importFrom R6 R6Class
FileDownload <- R6::R6Class(
  "FileDownload",
  inherit = exceedapi::Step,

  public = list(
    download = function(url) {
      temp_file <- fs::file_temp()
      download.file(url, temp_file, mode = "wb", method='curl')
      return(temp_file)
    }
  )
)

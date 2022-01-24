#' FileDownload - a file downloader step
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

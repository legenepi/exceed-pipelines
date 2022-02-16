#' LoadProfiles
#'
#' Load participant profiles from recruitment database.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadProfiles class
#' @importFrom R6 R6Class
#' @export
LoadProfiles <- R6::R6Class(
  "LoadProfiles",
  inherit = exceedapi::Step,

  private = list(
    get_profiles = function(.collect, ...) {
      self$logger$info("loading profiles")

      profiles <- self$client$profiles() %>%
        .collect(...)

      return(profiles)
    }
  ),

  public = list(
    transform = function(.data, .collect, ...) {
      private$get_profiles(.collect, ...)
    }
  )
)

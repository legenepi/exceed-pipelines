#' LoadProfiles - pipeline step for loading profiles
LoadProfiles <- R6::R6Class(
  "LoadProfiles",
  inherit = Step,

  private = list(
    get_profiles = function(.collect, ...) {
      self$logger$info("loading profiles")

      profiles <- self$client$profiles() %>%
        .collect()

      return(profiles)
    }
  ),

  public = list(
    transform = function(.data, .collect, ...) {
      private$get_profiles(.collect)
    }
  )
)

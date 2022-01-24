#' LoadProfiles - pipeline step for loading profiles
LoadProfiles <- R6::R6Class(
  "LoadProfiles",
  inherit = exceedapi::Step,

  private = list(
    get_profiles = function(.collect, ...) {
      self$logger$info("loading profiles")

      profiles <- self$client$profiles() %>%
        .collect(...)

      postcodes <- self$client$pipeline() %>%
        add_step(LookupPostcodes, by = "primaryaddress__address__postcode") %>%
        select(query, result_postcode) %>%
        .collect(profiles, ...)

      profiles %>%
        left_join(postcodes, by = c("primaryaddress__address__postcode" = "query")) %>%
        select(-primaryaddress__address__postcode) %>%
        rename(primaryaddress__address__postcode = result_postcode)
    }
  ),

  public = list(
    transform = function(.data, .collect, ...) {
      private$get_profiles(.collect, ...)
    }
  )
)

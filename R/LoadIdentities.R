#' LoadIdentities class
#'
#' Load pseudo identities for study participants.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadIdentities class
#' @importFrom R6 R6Class
LoadIdentities <- R6::R6Class(
  "LoadIdentities",
  inherit = exceedapi::Step,

  private = list(
    get_identities = function(.collect, ...) {
      self$logger$info("loading identities domain=%s", private$.domain)

      domains <- self$client$identities() %>%
        src_tbls()

      identities <- domains %>%
        stringr::str_subset(self$args$domain) %>%
        purrr::map(function(domain) {
          self$client$identities(domain = domain) %>%
            .collect()
        }) %>%
        dplyr::bind_rows()
    }
  ),

  public = list(
    transform = function(.data, .collect, ...) {
      private$get_identities(.collect)
    }
  )
)

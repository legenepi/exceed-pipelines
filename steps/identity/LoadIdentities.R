#' LoadIdentities - pipeline step for loading identities
LoadIdentities <- R6::R6Class(
  "LoadIdentities",
  inherit = Step,

  private = list(
    get_identities = function(.exec, ...) {
      self$logger$info("loading identities domain=%s", private$.domain)

      domains <- self$client$identity() %>%
        src_tbls()

      identities <- domains %>%
        str_subset(self$args$domain) %>%
        purrr::map(function(domain) {
          self$client$identity(domain = domain) %>%
            .exec()
        }) %>%
        bind_rows()
    }
  ),

  public = list(
    transform = function(.data, .exec, ...) {
      private$get_identities(.exec)
    }
  )
)

#' MergeUUIDs - pipeline step for merging UUIDs
MergeUUIDs <- R6::R6Class(
  "MergeUUIDs",
  inherit = LoadIdentities,

  public = list(
    transform = function(.data, ...) {
      identities <- super$transform(.data, ...) %>%
        select(-c(id, domain))

      by <- NULL
      by[self$args$by] <- "pid"

      # remove all non-alphanumeric characters
      domain <- str_replace_all(self$args$domain, regex("\\W"), "")

      .data %>%
        left_join(identities, by = by)
    }
  )
)

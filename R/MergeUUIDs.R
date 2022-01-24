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

      .data <- .data %>%
        left_join(identities, by = by)

      # keep distinct by uuid
      if (!is.null(self$args$distinct)) {
        distinct_slice <- self$args$distinct_slice
        if (is.null(distinct_slice))
          distinct_slice <- slice_min

        .data <- .data %>%
          group_by(uuid) %>%
          distinct_slice(self$args$distinct, n = 1, with_ties = FALSE) %>%
          ungroup()
      }

      return(.data)
    }
  )
)

#' MergeUUIDs
#'
#' Merge UUIDs
#'
#' @docType class
#' @format An R6 class object.
#' @description BreatheExport class
#' @importFrom R6 R6Class
#' @export
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
      domain <- stringr::str_replace_all(self$args$domain, stringr::regex("\\W"), "")

      .data <- .data %>%
        dplyr::left_join(identities, by = by)

      # keep distinct by uuid
      if (!is.null(self$args$distinct)) {
        distinct_slice <- self$args$distinct_slice
        if (is.null(distinct_slice))
          slicer <- dplyr::slice_min

        .data <- .data %>%
          dplyr::group_by(uuid) %>%
          slicer(self$args$distinct, n = 1, with_ties = FALSE) %>%
          dplyr::ungroup()
      }

      return(.data)
    }
  )
)

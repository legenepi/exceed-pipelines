#' MergeResearchProfessionalCollectedResponses
#'
#' Merge responses collected by research professionals.
#'
#' @docType class
#' @format An R6 class object.
#' @description MergeResearchProfessionalCollectedResponses class
#' @importFrom R6 R6Class
#' @export
MergeResearchProfessionalCollectedResponses <- R6::R6Class(
  "MergeResearchProfessionalCollectedResponses",
  inherit = LoadResearchProfessionalCollectedResponses,

  private = list(
    coalesce = function(x, y) {
      if (is.na(x))
        return(coalesce(x, y))

      if (x == y)
        return(x)
      else
        return(NA)
    }
  ),

  public = list(
    transform = function(.data, .collect, ...) {

      by <- NULL
      lhs <- ifelse(is.null(self$args$by), "exceed_id", self$args$by)

      stopifnot(lhs %in% names(.data))
      by[lhs] <- "exceed_id"

      rpq <- super$transform(.data, ...) %>%
        select(exceed_id, date_of_birth, sex) %>%
        .collect()

      .data <- .data %>%
        dplyr::left_join(rpq, by = by, suffix = c("", "_rpq"))

      if (is.null(self$args$merge))
        return(.data)

      self$args$merge %>%
        map_chr(~ paste(., "rpq", sep = "_"))

      iwalk(merge_vars, function(.x, .y) {
        .data <<- .data %>%
          rowwise() %>%
          mutate(!!.y := private$coalesce(!!as.symbol(.y), !!as.symbol(.x)))
      })

      .data %>%
        select(-ends_with("_rpq"))
    }
  )
)

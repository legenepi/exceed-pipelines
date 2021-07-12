#' RemoveDuplicateResponses - pipeline step for removing duplicate profiles
RemoveDuplicateResponses <- R6::R6Class(
  "RemoveDuplicateResponses",
  inherit = Step,

  public = list(
    transform = function(.data, ...) {
      stop("not implemented")
      .data
    }
  )
)

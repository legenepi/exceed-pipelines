#' RemoveDuplicateProfiles - pipeline step for removing duplicate profiles
RemoveDuplicateProfiles <- R6::R6Class(
  "RemoveDuplicateProfiles",
  inherit = Step,

  public = list(
    transform = function(.data, ...) {
      stop("not implemented")
      .data
    }
  )
)

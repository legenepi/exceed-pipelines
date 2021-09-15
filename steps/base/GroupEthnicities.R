#' GroupEthnicities - a pipeline step to group ethnicities
GroupEthnicities <- R6::R6Class(
  "GroupEthnicities", inherit = Step,

  private = list(
    group_ethnicities = function(.data, from, into) {
      .data %>%
        mutate(
          across(
            from,
            ~ fct_collapse(
              .,
              White = c("White British", "Irish", "Any other white background"),
              Asian = c("Asian or Asian British", "Indian", "Pakistani", "Bangladeshi", "Any other Asian background"),
              Chinese = c("Chinese"),
              Black = c("Black or Black British", "Black Caribbean"),
              Mixed = c("White and Black Caribbean", "White and Black African", "Any other mixed background"),
              Other = c("Do not know", "Do not know", "Prefer not to answer", "Other ethnic group")
            ),
            .names = into
          )
      )
    }
  ),

  public = list(
    transform = function(.data, ...) {
      private$group_ethnicities(.data, from = self$args$from, into = self$args$into)
    }
  )
)


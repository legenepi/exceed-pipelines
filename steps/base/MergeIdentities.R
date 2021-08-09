#' MergeIdentities - pipeline step for merging identities
MergeIdentities <- R6::R6Class(
  "MergeIdentities",
  inherit = Step,

  private = list(
    .domain = NULL,
    .drop_na = FALSE,
    .keep_uuid = FALSE
  ),

  public = list(
    initialize = function(
      pipeline,
      domain = NULL,
      drop_na = FALSE,
      keep_uuid = FALSE,
      ...
    ) {
      private$.domain <- domain
      private$.drop_na <- drop_na
      private$.keep_uuid <- keep_uuid
      super$initialize(pipeline, ...)
    },

    transform = function(.data, ...) {
      self$logger$info("merge identities domain=%s", private$.domain)

      identities <- exceed$identity(domain = "*") %>%
        collect()

      exceed_ids <- identities %>%
        filter(str_detect(domain, "^exceed")) %>%
        select(uuid, exceed_id = pid)

      domain_ids <- identities %>%
        filter(str_detect(domain, private$.domain)) %>%
        select(uuid, domain_id = pid)

      .data <- .data %>%
        left_join(exceed_ids, by = "exceed_id") %>%
        left_join(domain_ids, by = "uuid")

      if (private$.drop_na)
        .data <- drop_na(.data, domain_id)

      if (!private$.keep_uuid)
        .data <- select(.data, -uuid)

      return(.data)
    }
  )
)

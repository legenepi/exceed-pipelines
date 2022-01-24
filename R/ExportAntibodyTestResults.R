#' ExportAntibodyTestResults
#'
#' Export antibody test results.
#'
#' @docType class
#' @format An R6 class object.
#' @description BreatheExport class
#' @importFrom R6 R6Class
ExportAntibodyTestResults <- R6::R6Class(
  "ExportAntibodyTestResults",
  inherit = ExportTable,

  public = list(
    transform = function(...) {
      metadata <- self$prepare_metadata()
      dataset <- self$prepare_dataset(metadata)

      self$write_table(metadata, dataset)
    },

    prepare_metadata = function() {
      metadata <- readr::read_csv(
        here::here(self$args$metadata),
        show_col_types = FALSE
      ) %>%
        mutate(variable = paste(self$args$prefix, variable, sep = "_"))

      self$add_shared_metadata(metadata)
    },

    prepare_dataset = function(metadata) {
      fields <- self$get_fields()

      dataset <- self$client$pipeline() %>%
        add_step(LoadAntibodyTestResults) %>%
        add_step(MergeUUIDs, domain = "thriva", by = "subjectId") %>%
        select(uuid, fields) %>%
        collect() %>%
        mutate(specimenProcessedDate = lubridate::date(specimenProcessedDate)) %>%
        left_join(self$args$identities, by = "uuid") %>%
        filter(!is.na(uuid) & !is.na(STUDY_ID)) %>%
        relocate(STUDY_ID) %>%
        select(-uuid)

      dataset %>%
        self$add_prefix()
    }
  )
)

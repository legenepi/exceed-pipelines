#' ExportPhenotypes
#'
#' Export phenotypes defined by Leicester GenEpi group.
#'
#' @docType class
#' @format An R6 class object.
#' @description ExportPhenotypes class
#' @importFrom R6 R6Class
#' @export
ExportPhenotypes <- R6::R6Class(
  "ExportPhenotypes",
  inherit = ExportTable,

  public = list(
    transform = function(...) {
      metadata <- self$prepare_metadata()
      dataset <- self$prepare_dataset(metadata)

      self$write_table(metadata, dataset)
    },

    prepare_metadata = function() {
      fields_exclude <- self$get_fields(exclude = TRUE)

      metadata <- readr::read_csv(
        here::here(self$args$metadata),
        show_col_types = FALSE
      ) %>%
        filter(!(variable %in% fields_exclude)) %>%
        mutate(variable = paste(self$args$prefix, variable, sep = "_"))

      self$add_shared_metadata(metadata)
    },

    prepare_dataset = function(metadata) {
      fields_exclude <- self$get_fields(exclude = TRUE)

      dataset <- self$client$pipeline() %>%
        add_step(LoadPhenotypes) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        collect() %>%
        dplyr::left_join(self$args$identities, by = "uuid") %>%
        filter(!is.na(uuid) & !is.na(STUDY_ID)) %>%
        dplyr::group_by(STUDY_ID) %>%
        filter(dplyr::row_number() == 1)  %>%
        dplyr::ungroup() %>%
        relocate(STUDY_ID) %>%
        select(-uuid, -fields_exclude)

      doubles <- metadata %>%
        filter(type == "DOUBLE") %>%
        dplyr::pull("variable")

      dataset %>%
        self$add_prefix() %>%
        mutate(across(doubles, ~ round(as.double(.), digits = 2))) %>%
        select(metadata$variable)
    }
  )
)

#' ExportOccupations
#'
#' Export occupation table
#'
#' @docType class
#' @format An R6 class object.
#' @description ExportOccupations class
#' @importFrom R6 R6Class
ExportOccupations <- R6::R6Class(
  "ExportOccupations",
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

      field_map <- fields %>%
        map(~ paste("user__occupation", ., sep = "__")) %>%
        set_names(fields) %>%
        unlist()

      dataset <- self$client$pipeline() %>%
        add_step(LoadProfiles) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        select(id, uuid, !!field_map) %>%
        collect() %>%
        mutate(across(where(is.logical), as.numeric)) %>%
        mutate(across(fields, ~ na_if(str_replace_all(., "\\}|nan", ""), ""))) %>%
        filter(if_any(c(everything(), -id, -uuid), ~ !is.na(.x))) %>%
        left_join(self$args$identities, by = "uuid") %>%
        filter(!is.na(uuid) & !is.na(STUDY_ID)) %>%
        group_by(STUDY_ID) %>%
        arrange(STUDY_ID, id) %>%
        filter(row_number() == n())  %>%
        ungroup() %>%
        relocate(STUDY_ID) %>%
        select(-c(id, uuid))

      dataset %>%
        rename_with(~ str_replace(., "occupation__", "")) %>%
        self$add_prefix()
    }
  )
)

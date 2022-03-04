#' ExportProfiles
#'
#' Export participant profiles
#'
#' @docType class
#' @format An R6 class object.
#' @description ExportProfiles class
#' @importFrom R6 R6Class
#' @export
ExportProfiles <- R6::R6Class(
  "ExportProfiles",
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
      )

      if (!is.null(self$args$prefix)) {
        metadata <- mutate(
          metadata,
          variable = paste(self$args$prefix, variable, sep = "_")
        )
      }

      self$add_shared_metadata(metadata)
    },

    prepare_dataset = function(metadata) {
      allow_duplicates <- FALSE
      if (!is.null(self$args$allow_duplicates))
        allow_duplicates <- self$args$allow_duplicates

      demographics <- self$client$pipeline() %>%
        add_step(
          LoadDemographicProfiles,
          pseudo_dob_offset = self$args$parent$args$pseudo_dob_offset,
          allow_duplicates = allow_duplicates,
          exclude_withdrawn = self$args$exclude_withdrawn,
          exclude_incomplete_surveys = self$args$exclude_incomplete_surveys,
        ) %>%
        collect()

      profiles <- self$client$pipeline() %>%
        add_step(LoadProfiles) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        collect()

      nhsnumbers <- client$pipeline() %>%
        add_step(LoadNHSNumbers) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        collect() %>%
        group_by(uuid) %>%
        mutate(nhs_no = ifelse(n_distinct(nhs_no) > 1, NA, nhs_no)) %>%
        ungroup() %>%
        select(uuid, nhs_no) %>%
        distinct()

      profiles <- demographics %>%
        left_join(profiles, by = "uuid") %>%
        left_join(nhsnumbers, by = "uuid") %>%
        inner_join(self$args$identities, by = "uuid") %>%
        mutate(
          sex = sex %>%
            fct_recode("M" = "Male", "F" = "Female") %>%
            as.character()
        )

      if (!allow_duplicates) {
        profiles <- profiles %>%
          group_by(uuid) %>%
          group_map(function(.x, .y) {
            nhs_numbers <- discard(.x$nhs_no, is.na)
            if (n_distinct(nhs_numbers) == 1)
              slice_head(.x, n = 1)
            else {
              mutate(.x, nhs_no = NA)
            }
          }) %>%
          bind_rows()
      }

      profiles <- profiles %>%
        select(
          STUDY_ID,
          nhs_number = nhs_no,
          first_name,
          last_name,
          date_of_birth = dob,
          gender = sex,
          postcode = primaryaddress__address__postcode
        ) %>%
        distinct() %>%
        rename_with(str_to_upper)

      if (allow_duplicates) {
        profiles <- profiles %>%
          group_by(STUDY_ID) %>%
          mutate(STUDY_ID = paste(STUDY_ID, row_number(), sep = ".")) %>%
          ungroup()
      }

      return(profiles)
    }
  )
)

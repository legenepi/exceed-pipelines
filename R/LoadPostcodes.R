#' LoadPostcodes
#'
#' Load postcodes to CCG and LSOA mapping from ArcGIS.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadPostcodes class
#' @importFrom R6 R6Class
#' @export
LoadPostcodes <- R6::R6Class(
  "LoadPostcodes",
  inherit = exceedapi::Step,

  private = list(
    urls = list(
      postcodes = "https://www.arcgis.com/sharing/rest/content/items/ce3b53cb8a6c4fee93e5d560d7b1123d/data",
      lsoa_to_ccg = "https://opendata.arcgis.com/api/v3/datasets/c7e5f64bf2a7447c92db212f27093230_0/downloads/data?format=csv&spatialRefId=4326"
    ),

    load_dataset = function(key, .collect) {
      self$client$pipeline(cache = FALSE) %>%
        add_step(
          CachedCSV,
          key = key,
          url = purrr::pluck(private$urls, key),
          show_col_types = self$args$show_col_types
        ) %>%
        .collect() %>%
        dplyr::rename_with(stringr::str_to_lower)
    }
  ),

  public = list(
    transform = function(.data, .collect, ...) {
      .collect <- purrr::partial(.collect, ...)

      postcodes <- private$load_dataset("postcodes", .collect)
      lsoa_to_ccg <- private$load_dataset("lsoa_to_ccg", .collect)

      postcodes <- postcodes %>%
        dplyr::left_join(lsoa_to_ccg, by = "lsoa11cd") %>%
        private$apply_steps(.collect)

      rm(lsoa_to_ccg)
      return(postcodes)
    }
  )
)

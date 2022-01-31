#' LoadONSBoundaries
#'
#' Load CCG, LSOA, and MSOA boundaries from ONS.
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadONSBoundaries class
#' @importFrom R6 R6Class
LoadONSBoundaries <- R6::R6Class(
  "LoadONSBoundaries",
  inherit = CachedDownload,

  private = list(
    dataset = list(
      ccg = "d6acd30ad71f4e14b4de808e58d9bc4c_0",
      lsoa = "e9d10c36ebed4ff3865c4389c2c98827_0",
      msoa = "80223f1d571c405fb2fdf719c7e6da13_0"
    ),

    build_url = function(key) {
      dataset <- purrr::pluck(private$dataset, key)
      glue::glue(
        "https://opendata.arcgis.com/api/v3/datasets/{dataset}/downloads/data?format=shp&spatialRefId=27700"
      )
    }
  ),

  public = list(
    save = function(key, obj, filename) {
      shapefile <- zip::zip_list(filename) %>%
        mutate(suffix = fs::path_ext(filename)) %>%
        filter(suffix == "shp") %>%
        select(filename) %>%
        dplyr::first()

      exdir <- fs::path_temp()
      zip::unzip(filename, exdir = exdir)
      layer <- sf::st_read(fs::path(exdir, shapefile)) %>%
        sf::st_transform("WGS84") %>%
        rename(ons_id = self$args$field, label = self$args$label)
      super$save(key, layer, filename)
    },

    transform = function(...) {
      key <- self$args$key
      self$load(key, private$build_url(key), ...)
    }
  )
)

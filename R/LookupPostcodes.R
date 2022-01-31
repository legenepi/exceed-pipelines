#' LookupPostcodes
#'
#' Lookup postcodes using postcodes.io service
#'
#' @docType class
#' @format An R6 class object.
#' @description LookupPostcodes class
#' @importFrom R6 R6Class
LookupPostcodes <- R6::R6Class(
  "LookupPostcodes",
  inherit = exceedapi::Step,

  private = list(

    make_tibble = function(x) {
      x %>%
        purrr::discard(is.null) %>%
        data.frame() %>%
        tibble::as_tibble()
    },

    lookup_postcodes_batch = function(postcodes) {
      response <- httr::POST(
        "https://api.postcodes.io/postcodes",
        body = list(postcodes = postcodes),
        encode = "json"
      )

      if (httr::http_error(response)) {
        self$logger$error("HTTP GET failed with status = %s", httr::status_code(response))
      }

      httr::stop_for_status(response)

      type <- httr::http_type(response)
      if (type != "application/json") {
        stop(glue::glue("HTTP GET returned unexpected type = {type}"))
      }

      response %>%
        httr::content() %>%
        purrr::pluck("result") %>%
        purrr::map_dfr(~ {
          .x$result$codes <- NULL #private$make_tibble(.x$result$codes)
          .x$result <- purrr::discard(.x$result, is.null)
          private$make_tibble(.x)
        }) %>%
        dplyr::rename_all(~ stringr::str_replace_all(., "\\.", "_"))
    },

    lookup_postcodes = function(.data, batch_size = 100, ...) {
      pb <- self$progress_bar(
        total = nrow(.data),
        format = "  |:bar-| :percent"
      )

      .data %>%
        dplyr::select(self$args$by) %>%
        dplyr::mutate(batch = ceiling(dplyr::row_number() / batch_size)) %>%
        dplyr::group_by(batch) %>%
        dplyr::group_map(function(.x, .y) {
          pb$tick(len = nrow(.x))
          private$lookup_postcodes_batch(postcodes = dplyr::pull(.x, self$args$by))
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::distinct(query, .keep_all = TRUE)
    }
  ),

  public = list(
    transform = function(.data, ...) {
      private$lookup_postcodes(.data)
    }
  )
)


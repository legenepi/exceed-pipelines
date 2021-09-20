#' LookupPostcodes - pipeline step for postcode lookups
LookupPostcodes <- R6::R6Class(
  "LookupPostcodes",
  inherit = Step,

  private = list(

    make_tibble = function(x) {
      x %>%
        discard(is.null) %>%
        data.frame() %>%
        as_tibble()
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
        pluck("result") %>%
        map_dfr(~ {
          .x$result$codes <- NULL #private$make_tibble(.x$result$codes)
          .x$result <- discard(.x$result, is.null)
          private$make_tibble(.x)
        }) %>%
        rename_all(~ str_replace_all(., "\\.", "_"))
    },

    lookup_postcodes = function(postcodes, batch_size = 100, ...) {
      pb <- self$progress_bar(
        total = length(postcodes),
        format = "  |:bar-| :percent"
      )
      tibble(postcode = postcodes) %>%
        mutate(batch = ceiling(row_number() / batch_size)) %>%
        group_by(batch) %>%
        group_map(function(.x, .y) {
          pb$tick(len = nrow(.x))
          private$lookup_postcodes_batch(.x$postcode)
        }) %>%
        bind_rows() %>%
        distinct(query, .keep_all = TRUE)
    }
  ),

  public = list(
    transform = function(...) {
      private$lookup_postcodes(postcodes = self$args$postcodes)
    }
  )
)


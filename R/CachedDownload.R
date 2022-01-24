#' CachedDownload - a step with caching support
CachedDownload <- R6::R6Class(
  "CachedDownload",
  inherit = FileDownload,

  private = list(
    .store = NULL,

    cache_key = function(key, url) {
      config <- list(key = key, url = url)
      digest::digest(config, algo = "md5")
    }
  ),

  active = list(
    store = function() { private$.store } # nolint
  ),

  public = list(
    initialize = function(...) {
      super$initialize(...)

      cache_dir <- NULL
      store <- self$pipeline$cache$store
      if (is.null(store))
        cache_dir <- self$pipeline$config$cache$dir
      else
        cache_dir <- store$info()$dir

      if (!is.null(cache_dir))
        cache_dir <- fs::path(cache_dir, "download")

      private$.store <- cachem::cache_disk(dir = cache_dir)
    },

    load = function(key, url, .refresh_cache = FALSE, ...) {
      key <- private$cache_key(key, url)
      obj <- self$store$get(key)
      if (.refresh_cache | cachem::is.key_missing(obj)) {
        filename <- self$download(url)
        obj <- self$save(key, NULL, filename)
      }
      return(obj)
    },

    save = function(key, obj, filename) {
      if (is.null(obj))
        obj <- file(filename, "rb")
      self$store$set(key, obj)
      return(obj)
    },

    transform = function(...) {
      self$load(self$args$key, self$args$url, ...)
    }
  )
)

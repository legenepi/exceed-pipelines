#' GenericExport - pipeline step for preparing data exports.
GenericExport <- R6::R6Class(
  "GenericExport",
  inherit = Step,

  private = list(
    .config = NULL,
    .encrypt = FALSE,
    .password = NULL,
    .archive = NULL,
    .files = NULL,
    .manifest = NULL,
    .output_dir = NULL,
    .timestamp = format(Sys.time(), "%Y_%m_%d"),

    get_source_path = function() {
      if (rstudioapi::isAvailable()) {
        context <- rstudioapi::getActiveDocumentContext()
        return(context$path)
      } else {
        path <- commandArgs() %>%
          tibble::enframe(name=NULL) %>%
          tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
          dplyr::filter(key == "--file") %>%
          dplyr::pull(value)
        return(path)
      }
    },

    generate_password = function(length = 32) {
      paste(sample(c(0:9, letters, LETTERS), length), collapse="")
    },

    create_encrypted_archive = function(archive, files, password) {
      command <- glue::glue(
        "7z a -t7z -m0=lzma2 -mx=9 -mfb=64 -md=32m -ms=on -mhe=on -p{password} {archive} {files}",
        files = paste(files, collapse = " ")
      )
      system(command)
      system(glue::glue("7z l -p{password} {archive}"))
    },

    generate_checksums = function(file) {
      digest_algos <- list("md5", "sha256")
      checksums <- digest_algos %>%
        set_names() %>%
        map(~ digest::digest(file, algo = .x, file = TRUE))

      return(c(filename = fs::path_file(file), checksums))
    }
  ),

  active = list(
    archive = function() { private$.archive },
    config = function() { private$.config },
    files = function() { private$.files },
    manifest = function() { private$.manifest },
    output_dir = function() { private$.output_dir },
    password = function() { private$.password },
    timestamp = function() { private$.timestamp }
  ),

  public = list(
    initialize = function(pipeline, ...) {
      super$initialize(pipeline, ...)

      suffix <- "zip"
      if (self$args$encrypt) {
        suffix <- "7z"
        private$.password <- private$generate_password()
      }

      private$.output_dir <- here::here(
        fs::path_dir(private$get_source_path()), "output", private$.timestamp
      )

      private$.archive <- self$make_filename(
        prefix = "exceed",
        suffix = suffix
      )

      private$.manifest <- self$make_filename(
        prefix = "exceed_manifest",
        suffix = "txt"
      )
    },

    load_config = function(filename) {
      private$.config <- yaml::yaml.load_file(filename)
    },

    make_filename = function(prefix, suffix) {
      filename <- fs::path(
        self$output_dir,
        paste(
          paste("exceed", prefix, self$timestamp, sep = "_"),
          suffix,
          sep = "."
        )
      )
      fs::path_norm(filename)
    },

    write_csv = function(x, file, ...) {
      if (!fs::is_absolute_path(file)) {
        file <- self$make_filename(file, suffix = "csv")
      }

      if (fs::file_exists(file))
        fs::file_delete(file)

      file %>%
        fs::path_dir() %>%
        fs::dir_create()

      readr::write_csv(x, file, na = "", ...)

      cli::cli_alert_success(file)

      return(file)
    },

    create_archive = function() {
      if (fs::file_exists(self$archive))
        fs::file_delete(self$archive)

      checksums <- purrr::map(files, private$generate_checksums)
      private$create_manifest(checksums)

      files <- files %>%
        unlist() %>%
        as.vector() %>%
        append(self$manifest)

      if (self$args$encrypt) {
        private$create_encrypted_archive()
      } else {
        utils::zip(self$archive, self$files, flags = "--junk-paths")
        utils::unzip(self$archive, list = TRUE) %>%
          print()
      }

      private$generate_checksums(self$archive)
    }
  )
)


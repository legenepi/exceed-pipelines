#' GenericExport - pipeline step for preparing data exports.
GenericExport <- R6::R6Class(
  "GenericExport",
  inherit = Step,

  private = list(
    .encrypt = FALSE,
    .password = NULL,
    .output_dir = NULL,
    .output_file = NULL,
    .summary = tibble::tibble(
      attribute = character(),
      value = character()
    ),
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
      password <- paste(sample(c(0:9, letters, LETTERS), length), collapse="")
      self$summary_append("password", password)
      return(password)
    },

    create_archive_encrypted = function(archive, files, password) {
      command <- glue::glue(
        "7z a -t7z -m0=lzma2 -mx=9 -mfb=64 -md=32m -ms=on -mhe=on -p{password} {archive} {files}",
        files = paste(files, collapse = " ")
      )
      system(command)
    },

    generate_checksums = function(name, file) {
      digest_algos <- c("md5", "sha256")
      digest_algos %>%
        purrr::walk(function(algo) {
          checksum <- digest::digest(file, algo = algo, file = TRUE)
          self$summary_append(glue::glue("{name}-{algo}"), checksum)
        })
    }
  ),

  public = list(
    initialize = function(pipeline, encrypt = FALSE, ...) {
      super$initialize(pipeline, ...)

      private$.encrypt <- encrypt
      self$summary_append("encrypt", private$.encrypt)

      suffix <- "zip"
      if (encrypt) {
        suffix <- "7z"
        private$.password <- private$generate_password()
      }

      private$.output_dir <- here::here(
        fs::path_dir(private$get_source_path()), "output", private$.timestamp
      )
      self$summary_append("output-dir", private$.output_dir)

      private$.output_file <- self$make_filename(
        prefix = "exceed",
        suffix = suffix,
        attribute = "output-file"
      )
    },

    make_filename = function(prefix, suffix, attribute) {
      filename <- fs::path(
        private$.output_dir,
        paste(
          paste(prefix, private$.timestamp, sep = "_"),
          suffix,
          sep = "."
        )
      )
      self$summary_append(attribute = attribute, value = filename)
      return(filename)
    },

    write_csv = function(x, file, ...) {
      if (fs::file_exists(file))
        fs::file_delete(file)

      file %>%
        fs::path_dir() %>%
        fs::dir_create()

      readr::write_csv(x, file, na = "", ...)
    },

    create_archive = function(archive, files) {
      if (fs::file_exists(archive))
        fs::file_delete(archive)

      filenames <- as.vector(unlist(files))

      if (private$.encrypt) {
        private$create_archive_encrypted(archive, filenames, private$.password)
      } else {
        utils::zip(archive, filenames, flags = "--junk-paths")
      }

      # files <- c(output = archive, files)
      purrr::walk2(names(files), files, private$generate_checksums)
    },

    summary_append = function(attribute, value) {
      private$.summary <- private$.summary %>%
        tibble::add_row(attribute = attribute, value = as.character(value))
    }
  )
)


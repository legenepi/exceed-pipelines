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
    .timestamp = format(lubridate::today(), "%Y_%m_%d")
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
    initialize = function(...) {
      super$initialize(...)

      suffix <- "zip"
      if (self$args$encrypt) {
        suffix <- "7z"
        private$.password <- self$generate_password()
      }

      private$.output_dir <- here::here(
        fs::path_dir(self$get_source_path()), "output", private$.timestamp
      )

      private$.archive <- self$make_filename(
        prefix = "file2",
        suffix = suffix
      )

      private$.manifest <- self$make_filename(
        prefix = "manifest",
        suffix = "pdf"
      )
    },

    load_config = function(filename) {
      private$.config <- yaml::yaml.load_file(filename)
    },

    get_version = function() {
      commands <- c(
        "git describe --tags --exact-match 2> /dev/null",
        "git symbolic-ref -q --short HEAD",
        "git rev-parse --short HEAD"
      )
      system(paste(commands, collapse = "||"), intern = TRUE)
    },

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
        self$config$defaults$archiver$encrypted,
        files = paste(files, collapse = " ")
      )
      system(command)
      system(glue::glue("7z l -p{password} {archive}"))
      cli::cat_line(cli::boxx(glue::glue(" {password}")))
    },

    calulate_checksums = function(file) {
      digest_algos <- list("md5", "sha256")
      checksums <- digest_algos %>%
        set_names() %>%
        map(~ digest::digest(file, algo = .x, file = TRUE))

      return(c(filename = fs::path_file(file), checksums))
    },

    make_filename = function(prefix, suffix) {
      timestamp <- self$timestamp
      snapshot <- self$client$snapshot

      if (!is.null(snapshot))
        timestamp <- format(lubridate::ymd(snapshot), "%Y_%m_%d")

      filename <- fs::path(
        self$output_dir,
        paste(
          paste(c("exceed", prefix, self$timestamp), collapse = "_"),
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

    create_archive = function(files) {
      cli::cli_h1("Creating archive")

      if (fs::file_exists(self$archive))
        fs::file_delete(self$archive)

      if (self$args$encrypt) {
        self$create_encrypted_archive(self$archive, files, self$password)
      } else {
        utils::zip(self$archive, files, flags = "--junk-paths")
        utils::unzip(self$archive, list = TRUE) %>%
          print()
      }

      cli::cli_alert_success(fs::path_file(self$archive))
    },

    create_manifest = function(files, tables, template) {
      cli::cli_h1("Creating manifest")

      files <- files %>%
        purrr::map_dfr(self$calulate_checksums)

      rmarkdown::render(
        template,
        output_file = self$manifest,
        output_format = rmarkdown::pdf_document(),
        params = list(
          encrypt = self$args$encrypt,
          version = self$get_version(),
          snapshot = self$client$snapshot,
          archive = self$archive %>%
            self$calulate_checksums() %>%
            as_tibble(),
          tables = tables,
          files = files
        )
      )
      cli::cli_alert_success(fs::path_file(self$manifest))
    }
  )
)


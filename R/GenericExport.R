#' GenericExport
#'
#' Generic export class that includes password-protected compression (optional)
#'
#' @docType class
#' @format An R6 class object.
#' @description GenericExport class
#' @importFrom R6 R6Class
GenericExport <- R6::R6Class(
  "GenericExport",
  inherit = exceedapi::Step,

  private = list(
    .config = NULL,
    .encrypt = FALSE,
    .password = NULL,
    .archive = NULL,
    .files = NULL,
    .codebook = NULL,
    .manifest = NULL,
    .output_dir = NULL,
    .timestamp = format(lubridate::today(), "%Y_%m_%d"),

    get_file_list = function(tables) {
      tables %>%
        select(metadata, dataset) %>%
        unlist() %>%
        as.vector()
    }
  ),

  active = list(
    archive = function() { private$.archive },
    config = function() { private$.config },
    files = function() { private$.files },
    codebook = function() { private$.codebook },
    manifest = function() { private$.manifest },
    output_dir = function() { private$.output_dir },
    password = function() { private$.password },
    timestamp = function() { private$.timestamp }
  ),

  public = list(
    initialize = function(...) {
      private$.config <- yaml::yaml.load_file(
        here::here(self$get_source_path(), "config.yaml")
      )
      super$initialize(...)

      private$.output_dir <- here::here(
        self$get_source_path(),
        "output",
        private$.timestamp
      )

      if (self$args$live)
        private$.output_dir <- paste(private$.output_dir, "live", sep = ".")

      suffix <- "zip"
      if (self$args$encrypt) {
        suffix <- "7z"
        private$.password <- self$generate_password()
      }

      private$.archive <- self$make_filename(prefix = "file2", suffix = suffix)
      private$.codebook <- self$make_filename(prefix = "codebook", suffix = "pdf")
      private$.manifest <- self$make_filename(prefix = "manifest", suffix = "csv")
    },

    get_config = function() {
      list(
        live = self$args$live,
        encrypt = self$args$encrypt,
        version = self$get_version(),
        snapshot = self$client$snapshot,
        output_dir = self$output_dir
      )
    },

    show_config = function() {
      cli::cli_h2("Configuration")

      self$get_config() %>%
        yaml::as.yaml() %>%
        cli::cat_line()
    },

    show_summary = function() {
      cli::cli_h2("Summary")

      summary <- self$get_config()
      c(
        summary,
        list(
          archive = self$archive
        )
      ) %>%
        yaml::as.yaml() %>%
        cli::cat_line()
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
        path <- context$path
      } else {
        path <- commandArgs() %>%
          tibble::enframe(name=NULL) %>%
          tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
          dplyr::filter(key == "--file") %>%
          dplyr::pull(value)
      }
      path %>%
        fs::path_abs() %>%
        fs::path_dir()
    },

    generate_password = function(length = 32) {
      paste(sample(c(0:9, letters, LETTERS), length), collapse="")
    },

    prepare_export = function(domain) {
      cli::cli_h2("Identities")
      identities <- self$client$pipeline() %>%
        add_step(LoadIdentities, domain = domain) %>%
        select(uuid, STUDY_ID = pid) %>%
        collect()

      cli::cli_h2("Profiles")
      exclusions <- self$client$pipeline() %>%
        add_step(LoadProfiles) %>%
        add_step(MergeUUIDs, domain = "exceed", by = "exceed_id") %>%
        select(uuid, deceased, scope = basicconsent__withdrawals__scope) %>%
        collect() %>%
        filter(scope == 3 | deceased == TRUE) %>%
        dplyr::left_join(identities, by = "uuid")

      identities <- identities %>%
        anti_join(exclusions, by = "uuid")

      self$export_tables(self$config$tables, identities = identities)
    },

    export_tables = function(tables, identities) {
      cli::cli_h1("Exporting tables")

      tables <- tables %>%
        purrr::discard(~ .$skip) %>%
        purrr::map_dfr(~ {
          cli::cli_h2(.x$name)
          .x$args$name <- .x$name
          self$client$pipeline() %>%
            add_step(
              !!.x$step,
              parent = self,
              !!!.x$args,
              identities = identities
            ) %>%
            collect()
        })

      cli::cli_h2("Tables")

      tables %>%
        select(table, variables, observations) %>%
        cli::cat_print()

      return(tables)
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

    create_unencrypted_archive = function(archive, files) {
      zip::zip(
        self$archive,
        files = files,
        recurse = FALSE,
        include_directories = FALSE,
        mode = "cherry-pick"
      )
      zip::zip_list(self$archive) %>%
        select(filename, compressed_size) %>%
        print()
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

    create_archive = function(tables) {
      cli::cli_h1("Creating archive")

      files <- private$get_file_list(tables)

      if (fs::file_exists(self$archive))
        fs::file_delete(self$archive)

      if (self$args$encrypt)
        self$create_encrypted_archive(self$archive, files, self$password)
      else
        self$create_unencrypted_archive(self$archive, files)

      cli::cli_alert_success(self$archive)
    },

    create_manifest = function(tables) {
      cli::cli_h1("Creating manifest")

      self$create_manifest_csv(tables)

      self$create_manifest_pdf(tables)
    },

    create_manifest_csv = function(tables) {
      cli::cli_h2("Creating manifest (.csv)")

      tables %>%
        self$write_csv(fs::path_ext_set(self$manifest, "csv"))
    },

    create_manifest_pdf = function(tables) {
      cli::cli_h2("Creating manifest (.pdf)")

      files <- private$get_file_list(tables) %>%
        purrr::map_dfr(self$calulate_checksums)

      self$render_markdown(
        input = here::here(self$get_source_path(), "manifest_template.Rmd"),
        output_file = fs::path_ext_set(self$manifest, "pdf"),
        params = list(
          encrypt = self$args$encrypt,
          version = self$get_version(),
          snapshot = self$client$snapshot,
          archive = self$archive %>%
            self$calulate_checksums() %>%
            tibble::as_tibble(),
          files = files,
          tables = tables
        )
      )
    },

    create_codebook = function(tables) {
      cli::cli_h2("Creating codebook (.pdf)")

      self$render_markdown(
        input = here::here(self$get_source_path(), "codebook_template.Rmd"),
        output_file = self$codebook,
        params = list(
          version = self$get_version(),
          snapshot = self$client$snapshot,
          tables = tables
        )
      )
    },

    render_markdown = function(input, output_file, ...) {
      rmarkdown::render(
        input = input,
        output_file = output_file,
        output_format = rmarkdown::pdf_document(),
        ...
      )
      cli::cli_alert_success(output_file)
    }
  )
)


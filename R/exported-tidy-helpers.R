#' Source Helpers
#'
#' `source_xx()` are helper functions that assist with quickly sourcing the content of a
#' folder with a tidy project structure. Under the hood, these function use `source_folder()`.
#'
#' @name source_helpers
#'
#' @param subdir If required, a subdir can be specified.
#' @inheritDotParams source_folder


#' @rdname source_helpers
#' @export
source_utils <- function(subdir = NULL, ...) {
  source_helper(file.path("R", "utils"), subdir, ...)
}

#' @rdname source_helpers
#' @export
source_r <- function(subdir = NULL, ...) {
  source_helper(file.path("R"), subdir, ...)
}

#' @rdname source_helpers
#' @export
source_model <- function(subdir = NULL, ...) {
  source_helper(file.path("model"), subdir, ...)
}

#' @rdname source_helpers
#' @export
source_math <- function(subdir = NULL, ...) {
  source_helper(file.path("math"), subdir, ...)
}

#' @rdname source_helpers
#' @export
source_functions <- function(subdir = NULL, ...) {
  paths <- c(".", "functions", subdir)
  path <- paste0(paths, collapse = .Platform$file.sep)
  source_folder(path, ...)
}

source_helper <- function(dir, subdir, ...) {
  paths <- c(get_root(), dir, subdir)
  path <- paste0(paths, collapse = .Platform$file.sep)
  source_folder(path, ...)
}




#' Save data to model outputs
#'
#' `save_output()` utilizes the power of a tidy project structure to save a data.frame
#'  as a feather file to the model outputs folder.
#'
#' @param data A data.frame.
#' @param name Name to use for saved file. `.fe` will be added by default.
#' @param subdir An optional subdir.
#'
#' @export
save_outputs <- function(data, name, subdir = NULL) {
  check_type(data, "data.frame")
  check_type(name, "character")
  if (!is.null(subdir)) check_type(subdir, "character")
  name <- rem_ext(basename(name))
  name <- paste0(name, ".fe")
  paths <- c(get_root(), "model", "outputs", subdir)
  base_path <- paste0(paths, collapse = .Platform$file.sep)
  mkdir(base_path)
  filepath <- file.path(base_path, name)
  feather::write_feather(data, filepath)
  cli::cli_alert_success("File saved to {.path {relative_path(filepath)}}.")
}


#' Save data to model inputs
#'
#' `save_inputs()` utilizes the power of a tidy project structure to save a data.frame
#'  to the model inputs folder.
#'
#' @param data A data.frame.
#' @param filename Name to use for saved file. Must include a file extension that is
#' either a delim or feather file.
#' @param subdir An optional subdir.
#'
#' @export
save_inputs <- function(data, filename, subdir = NULL) {
  check_type(data, "data.frame|list")
  check_type(filename, "character")
  if (!is.null(subdir)) check_type(subdir, "character")
  ext <- tools::file_ext(filename)
  if (nchar(ext) == 0) abort_no_file_ext(filename)
  writer <- get_writer(ext)
  paths <- c(get_root(), "model", "inputs", subdir)
  base_path <- paste0(paths, collapse = .Platform$file.sep)
  mkdir(base_path)
  filepath <- file.path(base_path, filename)
  writer(data, filepath)
  cli::cli_alert_success("File saved to {.path {relative_path(filepath)}}.")
}

#' Save text to model inputs
#'
#' `save_inputs_text()` utilizes the power of a tidy project structure to save a
#'  character vector to the model inputs folder.
#'
#' @param text A character vector.
#' @param filename Name to use for saved file. Must include a file extension that is
#' either a delim or feather file.
#' @param subdir An optional subdir.
#'
#' @export
save_inputs_text <- function(text, filename, subdir = NULL) {
  check_type(text, "character")
  check_type(filename, "character")
  if (!is.null(subdir)) check_type(subdir, "character")
  paths <- c(get_root(), "model", "inputs", subdir)
  base_path <- paste0(paths, collapse = .Platform$file.sep)
  mkdir(base_path)
  filepath <- file.path(base_path, name)
  con <- file(filepath)
  writeLines(text, con)
  close(con)
  cli::cli_alert_success("File saved to {.path {relative_path(filepath)}}.")
}

#' Load model outputs
#'
#' `load_output()` and `load_inputs()` utilizes the power of a tidy project structure
#' to load a named file from the model folder.
#'
#' @name model_loaders
#' @param name Name of file to load.
#' @param subdir An optional subdir.
#' @param ... Arguments to pass on to the appropriate file reader.
#'
#'

#' @rdname model_loaders
#' @export
load_outputs <- function(name, subdir = NULL, ...) {
  load_model_file("outputs", name, subdir, ...)
}

#' @rdname model_loaders
#' @export
load_inputs <- function(name, subdir = NULL, ...) {
  load_model_file("inputs", name, subdir, ...)
}

load_model_file <- function(model_dir, name, subdir, ...) {
  check_type(name, "character")
  if (!is.null(subdir)) check_type(subdir, "character")
  paths <- c(get_root(), "model", model_dir, subdir)
  base_path <- paste0(paths, collapse = .Platform$file.sep)
  filepath <- file.path(base_path, name)
  if (!file.exists(filepath)) abort_file_not_found(filepath)
  ext <- tools::file_ext(filepath)
  if (nchar(ext) == 0) abort_no_file_ext(name)
  reader <- get_reader(ext)
  df <- reader(filepath, ...)
  cli::cli_alert_success("Loaded {.path {name}} (Source: Model {model_dir}).")
  df
}


#' Get file path of tidy data
#'
#' Function returns the absolute path to a tidy data file.
#'
#' @param name Name of data.
#' @param reg_type Whether it is stored in raw or tidy.
#'
#' @return Character vector of absolute filepath.
#' @export
get_filepath <- function(name, reg_type) {
  metadata <- get_metadata(name, reg_type)
  source_dir <- get_source_dir(name, reg_type)
  filepath <- file.path(
    get_root(), "data", reg_type, source_dir, paste0(name, ".", metadata$ext)
  )
  filepath
}



#' Use tidy project tests
#'
#' `test()` runs a version of `testthat()` adapted for datr-style tidy projects.
#'
#' @export
test <- function() {
  test_path <- file.path(get_root(), "tests", "run-tests.R")
  if (!file.exists(test_path)) {
    cli::cli_alert_warning("Tests do not seem to be set up in this project.")
    msg <- cli::format_inline("Continuing will set up tests in this project now.")
    if (confirm_action(msg)) {
      use_tests()
    } else {
      stop_quietly()
    }
  } else {
    source(test_path)
  }
}
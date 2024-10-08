#' Provide normalized names and make sure the dir exists
#'
#' @description This provides a temporary directory where the executables can read and write temporaty files. Its path is returned in normalized format \bold{with} system-dependent terminal separator.
#' @param temp_dir NULL, or a path to be used; if NULL, R's [base::tempdir()] is used.
#' @param sep Better ignored; non-default values are passed to [base::normalizePath()].
#' @param sub Extension for defining a sub-directory within the directory defined by [base::tempdir].
#'
#' @return Normalized path with system-dependent terminal separator.
#' @export

tp_tempdir <- function(temp_dir = NULL, sep = NULL, sub = NULL) {
  if (is.null(sep)) {
    sep <- .sep()
  if (is.null(temp_dir))
    temp_dir <- tempdir()
  temp_dir <- normalizePath(temp_dir, sep)
  if (substring(temp_dir, nchar(temp_dir)) != sep) {
    temp_dir <- paste0(temp_dir, sep)
  }
  if (!is.null(sub))
    temp_dir <- paste0(temp_dir, sub, sep)
  if (!dir.exists(temp_dir))
    dir.create(temp_dir)
  temp_dir
  }
}

#' Provide a separator character platform dependent
.sep <- function() {
  if (.Platform$OS.type == "windows")
    "\\"
  else
    "/"
}

#' Provide a enumeration for each model cover by the checker
.model_name <- function() {
  list(
    Basic = "basic",
    Coin = "coin"
  )
}

#' Write input for treeppl
.input <- function(model, data, temp_dir = NULL, project_name = NULL) {
  if (is.null(temp_dir)) {
    temp_dir <- tp_tempdir()
  }
  if (is.null(project_name)) {
    project_name <- "default"
  }

  sep <- .sep()

  temp_file <- paste0(temp_dir, sep, project_name)
  readr::write_file(model, paste0(temp_file, ".tppl"))
  write_json(data, paste0(temp_file, ".json"))
}


#' Platform-dependent treeppl self-contained installation
#' @description
#' `tp_installing_treeppl` will search for the local version tpplc associate
#' with the package. Will download it if it's not detected on the computer.
#'
#' @param download Will download the associate tpplc version in the dir next
#' to your local treepplr installation if not present.
#'
#' @return The path for TreePPL compiler.
#' @export
tp_installing_treeppl <- function(download = TRUE) {
  if (Sys.getenv("TPPLC") != "") {
    tpplc_path <- Sys.getenv("TPPLC")
  } else{
    if (Sys.info()['sysname'] == "Windows") {
      # No self container for Windows, need to install it manually
      "tpplc"
    } else {
      path_treeppl <-
        list.files(path = paste0(.libPaths()[1], "/treeppl/", TPPLC_VERSION),
                   full.names = TRUE)
    }
    # Test if tpplc is already here
    tpplc_path <- paste0("/tmp/treeppl-",TPPLC_VERSION,"/tpplc")
    if(!file.exists(tpplc_path)) {
      if(download && length(path_treeppl) == 0) {
        tag <- tp_fp_fetch()
        path_treeppl <-
          list.files(path = paste0(.libPaths()[1], "/treeppl/", TPPLC_VERSION),
                     full.names = TRUE)
      }
      if (length(path_treeppl) != 0) {
        message("TreePPL initialisation ...please wait...")
        utils::untar(path_treeppl, exdir="/tmp", verbose = FALSE)
        message("TreePPL initialisation : Done")
      }
    }
  }
  tpplc_path
}

# Fetch the associate version of TreePPL if needed
tp_fp_fetch <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    # no self container for Windows, need to install it manually
    "-1"
  } else {
    # Check for Linux
    if (Sys.info()["sysname"] == "Linux") {
      # assets[[2]] because releases are in alphabetical order (1 = Mac, 2 = Linux)
      name <- paste0("treeppl-",TPPLC_VERSION,"-x86_64-linux.tar.gz")
    } else {
      name <- paste0("treeppl-",TPPLC_VERSION,"-aarch64-darwin.tar.gz")
    }

    url <- paste0("https://github.com/treeppl/treeppl/releases/download/v",
                  TPPLC_VERSION,"/",name)
    # local repository
    file_name <- list.files(path = paste0(.libPaths()[1], "/treeppl/",
                                          TPPLC_VERSION),
                            full.names = TRUE)
    # download file if file_name is empty
    if (length(file_name) == 0) {
      # create destination folder if treeppl dir doesn't exist
      dest_folder <- paste0(.libPaths()[1], "/treeppl")
      system(paste("mkdir", dest_folder), ignore.stdout = FALSE,
             ignore.stderr = FALSE)
      # create destination folder if version dir doesn't exist
      version_dir <- paste(dest_folder, TPPLC_VERSION, sep = "/")
      system(paste("mkdir", version_dir), ignore.stdout = TRUE,
             ignore.stderr = TRUE)
      # download
      fn <- paste(version_dir, name, sep = "/")
      curl::curl_download(
        url,
        destfile = fn,
        quiet = FALSE
      )
    }
  }
  TPPLC_VERSION
}

#' Temporary directory for running treeppl
#'
#' @description
#' `tp_tempdir` returns a normalized path for a temporary directory where the
#' executables can read and write temporary files.
#'
#' @param temp_dir NULL, or a path to be used; if NULL, R's [base::tempdir]
#' is used.
#' @param sep Better ignored; non-default values are passed to
#' [base::normalizePath].
#' @param sub Extension for defining a sub-directory within the directory
#' defined by [base::tempdir].
#'
#' @return Normalized path with system-dependent terminal separator.
#' @export

tp_tempdir <- function(temp_dir = NULL,
                       sep = NULL,
                       sub = NULL) {
  if (is.null(sep)) {
    sep <- sep()
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

# Platform-dependent separator character
sep <- function() {
  .Platform$file.sep
}


#' Model names supported by treepplr
#'
#' @description Provides a list of all models in the TreePPL model library.
#'
#' @return A list of model names.
#' @export
tp_model_library <- function() {

  # make sure you get the appropriate version if you have more than one treeppl folder in the tmp
  fd <- list.files("/tmp",
                   pattern = paste0("treeppl-", TPPLC_VERSION),
                   full.names = TRUE)
  # go to the right treeppl folder, whatever it is called
  fd <- list.files(fd, pattern = "treeppl", full.names = TRUE)
  # add the rest of the path
  fd <- paste0(fd, "/lib/mcore/treeppl/models")
  # model names
  mn <- list.files(fd, full.names = TRUE, recursive = TRUE, pattern = "\\.tppl$")
  subcategory <- grepl(".*models/([^/]+)/([^/]+)/([^/]+)\\.tppl$", mn)
  no_sub <- mn[!subcategory]

  # results in a data frame
  rs <- data.frame(
    "category" = sub(".*models/([^/]+)/.*", "\\1", no_sub),
    "model_name" = sub(".*/([^/]+)\\.tppl$", "\\1", no_sub)
  )
  # order by category then by model name
  rs <- rs[order(rs$category, rs$model_name, decreasing = FALSE), ]
  rownames(rs) <- NULL
  rs

}


# Find model for model_name
tp_find_model <- function(model_name) {
  tp_find(model_name, ".tppl")
}

# Find data for model_name
tp_find_data <- function(model_name) {
  tp_find(model_name, ".json")
}

tp_find <- function(model_name, ext) {
  # make sure you get the appropriate version if you have more than one treeppl folder in the tmp
  version <- list.files("/tmp",
                   pattern = paste0("treeppl-", TPPLC_VERSION),
                   full.names = TRUE)

  res <- list.files(version,
                    full.names = TRUE,
                    recursive = TRUE,
                    pattern = paste0(model_name, ext))
}


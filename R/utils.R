tp_rootPath <- function() {
  if (Sys.info()["sysname"] != "Windows") {
    return("/tmp")
  } else {
    return("//wsl.localhost/Debian/tmp")
  }
}

tp_tpplc_call <- function(options) {
  tpplc_path <- tp_installing_treeppl()
  if (Sys.info()["sysname"] != "Windows") {
    cmd_opt <- system2(
      command = tpplc_path,
      args = options,
      env = "LD_LIBRARY_PATH= ",
      stdout = TRUE
    )
  } else {
    cmdLine = paste("-c \"", tpplc_path, options, "\"")
    cmd_opt <- system2(
      command = BASH_PATH,
      args = cmdLine,
      stdout = TRUE)
  }
  cmd_opt
}

#' Platform-dependent treeppl self-contained installation
#' @description
#' `tp_installing_treeppl` will search for the local version tpplc associate
#' with the package. Will download it if it's not detected on the computer.
#'
#' @param download Will download the associate tpplc version in the dir next
#' to your local treepplr installation if not present.
#'
#' @param keep_previous Will download the associate tpplc version in the dir next
#' to your local treepplr installation if not present.
#'
#' @return The path for TreePPL compiler.
#' @export
tp_installing_treeppl <- function(download = TRUE,
                                  keep_previous = FALSE) {
  if (Sys.getenv("TPPLC") != "") {
    tpplc_path <- Sys.getenv("TPPLC")
  } else{
    root_path <- tp_rootPath()
    path_treeppl <-
      list.files(
        path = paste0(.libPaths()[1], "/treeppl/", TPPLC_VERSION),
        full.names = TRUE
      )
    # Test if tpplc is already here
    # Use the directory as proxy for windows to be able to see something
    tpplc_dirPath <- paste0(root_path, "/treeppl-", TPPLC_VERSION)
    if (!dir.exists(tpplc_dirPath)) {
      if (download && length(path_treeppl) == 0) {
        tag <- tp_fp_fetch(keep_previous)
        path_treeppl <-
          list.files(
            path = paste0(.libPaths()[1], "/treeppl/", TPPLC_VERSION),
            full.names = TRUE
          )
      }
      if (length(path_treeppl) != 0) {
        message("TreePPL initialisation ...please wait...")
        if (Sys.info()["sysname"] == "Windows") {
          # WARNING Tim : Here the copy will fail if the tar is already in the Debian/tmp
          # will avoid the tar error if treeppl already here
          # NOT REALLY checking if tpplc is in place but proxy enough for now
          if (file.copy(path_treeppl, root_path)) {
            cmdLine <- paste0("/tmp/", basename(path_treeppl))
            cmdLine <- paste("tar -xf", cmdLine, "-C /tmp")
            cmdLine <- paste("-c \"", cmdLine, "\"")
            system2(BASH_PATH, args = cmdLine)
          }
        } else {
          utils::untar(path_treeppl, exdir = root_path, verbose = FALSE)
        }
        message("TreePPL initialisation : Done")
      }
    }
    tpplc_path <- paste0("/tmp/treeppl-", TPPLC_VERSION, "/tpplc")
  }
  tpplc_path
}

# Fetch the associate version of TreePPL if needed
tp_fp_fetch <- function(keep_previous = FALSE) {
  # Check for Linux
  if (Sys.info()["sysname"] == "Linux" ||
      Sys.info()["sysname"] == "Windows") {
    # assets[[2]] because releases are in alphabetical order (1 = Mac, 2 = Linux)
    name <- paste0("treeppl-", TPPLC_VERSION, "-x86_64-linux.tar.gz")
  } else {
    name <- paste0("treeppl-", TPPLC_VERSION, "-aarch64-darwin.tar.gz")
  }

  url <- paste0(
    "https://github.com/treeppl/treeppl/releases/download/v",
    TPPLC_VERSION,
    "/",
    name
  )
  # local repository
  file_name <- list.files(path = paste0(.libPaths()[1], "/treeppl/", TPPLC_VERSION),
                          full.names = TRUE)
  # download file if file_name is empty
  if (length(file_name) == 0) {
    # create destination folder if treeppl dir doesn't exist
    dest_folder <- paste0(.libPaths()[1], "/treeppl")
    if (!keep_previous) {
      system(
        paste("rm -rf", dest_folder),
        ignore.stdout = FALSE,
        ignore.stderr = FALSE
      )
    }
    system(
      paste("mkdir", dest_folder),
      ignore.stdout = FALSE,
      ignore.stderr = FALSE
    )
    # create destination folder if version dir doesn't exist
    version_dir <- paste(dest_folder, TPPLC_VERSION, sep = "/")
    system(
      paste("mkdir", version_dir),
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    )
    # download
    fn <- paste(version_dir, name, sep = "/")
    curl::curl_download(url, destfile = fn, quiet = FALSE)
  }
  TPPLC_VERSION
}

#' Temporary directory for running treeppl
#'
#' @description
#'
#'
#' @return
#' @export

tp_tempdir <- function() {
  tmp <- paste0(tp_rootPath(), "/treepplTmp/")
  if(!dir.exists(tmp)) {
    dir.create(tmp)
  }
  tmp
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
  root_path <- tp_rootPath()
  # make sure you get the appropriate version if you have more than one treeppl folder in the tmp
  fd <- list.files(root_path,
                   pattern = paste0("treeppl-", TPPLC_VERSION),
                   full.names = TRUE)
  # go to the right treeppl folder, whatever it is called
  fd <- list.files(fd, pattern = "treeppl", full.names = TRUE)
  # add the rest of the path
  fd <- paste0(fd, "/lib/mcore/treeppl/models")
  # model names
  mn <- list.files(fd,
                   full.names = TRUE,
                   recursive = TRUE,
                   pattern = "\\.tppl$")
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

# Function to find the path of model and data files based on a model name and extension
tp_find <- function(model_name, ext) {
  # path to the model library
  version <- unlist(strsplit(Sys.getenv("MCORE_LIBS"), "treeppl="))[2]
  if (!is.na(version)) {
    version <- list.files(version, pattern = "models", full.names = TRUE)
    # path to the required model
    fd <- list.files(
      path = version,
      pattern = paste0(model_name, ext),
      recursive = TRUE,
      full.names = TRUE
    )
  } else {
    root_path <- tp_rootPath()
    fd <- list.files(
      root_path,
      pattern = paste0("treeppl-", TPPLC_VERSION),
      full.names = TRUE
    )
    fd <- list.files(fd, pattern = "treeppl", full.names = TRUE)
    fd <- paste0(fd, "/lib/mcore/treeppl/models")
    # path to the required model
    fd <- list.files(
      path = fd,
      pattern = paste0(model_name, ext),
      recursive = TRUE,
      full.names = TRUE
    )
  }
  return(fd)
}

# Find model for model_name
tp_find_model <- function(model_name) {
  tp_find(model_name, ".tppl")
}

# Find data for model_name
tp_find_data <- function(model_name) {
  tp_find(model_name, ".json")
}

#' Create a flat list
#'
#' @description
#' `tp_list` takes a variable number of arguments and returns a list.
#'
#' @param ... Variadic arguments (see details).
#'
#' @details
#' This function takes a variable number of arguments, so that users can pass as
#' arguments either independent lists, or a single structured
#' list of list (name_arg = value_arg).
#'
#' @return A list.
#' @export
tp_list <- function(...) {
  dotlist <- list(...)

  if (length(dotlist) == 1L && is.list(dotlist[[1]])) {
    dotlist <- dotlist[[1]]
  }

  dotlist
}

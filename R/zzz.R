#The goal of this file is to performing task at the loading of the package

######Version Change ##########
####Use to pull the tag of the last version of TreePPL release on the following function
#repo_info <- gh::gh("GET /repos/treeppl/treeppl/releases")
#version <- repo_info[[1]]$tag_name
##################

#'@export
TPPLC_VERSION <- "0.4"

# WARNING : Not sure how it's work if user doesn't have admin right or
# admin is giving by a different account

.foundBash <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    bashs <- system2('where.exe', args = "bash", stdout = TRUE)
    path <- stringr::str_detect(bashs, Sys.getenv("USERNAME"))
    bashs[path]
  }
}

#'@export
BASH_PATH <- .foundBash()

.onLoad <- function(libname, pkgname){
  tp_installing_treeppl(download =  FALSE)
}

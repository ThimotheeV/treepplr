#The goal of this file is to performing task at the loading of the package

######Version Change ##########
####Use to pull the tag of the last version of TreePPL release on the following function
#repo_info <- gh::gh("GET /repos/treeppl/treeppl/releases")
#version <- repo_info[[1]]$tag_name
##################

.onLoad <- function(libname, pkgname){
  TPPLC_VERSION <<- "0.3"
  tp_installing_treeppl(FALSE)
}

#TODO : Automatic extraction from tpplc --help (sorted)
#The goal is to let the compiler do most of the work
#This list only exist to avoid to recompilate if not needed
tpplcCompileOptions <- c(
  "align",
  "cps",
  "drift",
  "dynamic-delay",
  "incremental-printing",
  "kernel",
  "method",
  "pigeons",
  "pigeons-no-global",
  "pigeons-explore-steps",
  "prune ",
  "resample",
  "static-delay"
)

#' Options that can be passed to TreePPL compiler
#'
#' @returns A data frame with the output from the compiler's help <tpplc --help>
#'
tp_compile_options <- function() {
  # treeppl options
  cmd_opt <- tp_tpplc_call("--help")

  # Preparing the output #

  # find the line containing "Options:"
  x <- which(cmd_opt == "Compile options:")
  # extract everything after that line
  cmd_opt <- cmd_opt[(x + 1):length(cmd_opt)]
  cmd_opt <- trimws(cmd_opt)
  cmd_opt <- strsplit(cmd_opt, " {2,}", perl = TRUE)

  opt_tab <- do.call(rbind, lapply(cmd_opt, function(x) {
    # if there is no description, make it NA
    if (length(x) == 1)
      x <- c(x, NA)
    data.frame(
      argument = x[1],
      description = x[2],
      stringsAsFactors = FALSE
    )
  }))

  # fix arguments (delete everything that comes after the first space)
  opt_tab$argument <- sub(" .*", "", opt_tab$argument)
  return(opt_tab)
}

#Separation between compile and over options (can be runtime or mistake,
#that the compiler to determine)
list_to_options <- function(user_list) {
  options <- list(compile = list(), runtime = list())
  if (length(user_list) != 0) {
    for (name in tpplcCompileOptions) {
      options[["compile"]][[name]] <- user_list[[name]]
    }
    for (name in names(user_list)) {
      if (is.null(options[["compile"]][[name]])) {
        options[["runtime"]][[name]] <- user_list[[name]]
      }
    }
  }
  options
}

#' Convert options to a proper string of flags, e.g., `_` to `-`,
#' adding `--` in the beginning, spaces between things, etc.
options_to_string <- function(options) {
  args_str <- c()
  if (length(options) != 0) {
    vec <- c()
    args_vec <- unlist(options)
    for (i in seq_along(args_vec)) {
      if (!is.logical(args_vec[[i]])) {
        str <- paste0("--", names(args_vec[i]), " ", args_vec[[i]])
      } else {
        if (args_vec[[i]]) {
          str <- paste0("--", names(args_vec[i]))
        }
      }
      vec <- c(vec, str)
    }
    args_str <- paste(vec, collapse = " ")
  }
  args_str
}

#' TreePPL model template
#'
#' @description
#' `tp_modelT` template for TreePPL code carrying all the informations necessary
#' for compiling and running this model efficently

compiled_model_Template <-
  setRefClass(
    "compiled_model_Template",
    fields = list(
      exe_path = "character",
      path = "character",
      compile_options = "list"
    )
  )

#' Compile a TreePPL model and create inference machinery
#'
#' @description
#' `compilation` compile a TreePPL model and create inference machinery to be
#' used by [treepplr::tp_run].
#'
#' @param model One of tree options:
#'   * The full path of the model file that contains the TreePPL code, OR
#'   * A string with the name of a model supported by treepplr
#' (see [treepplr::tp_model_library()]), OR
#'   * A string containing the entire TreePPL code.
#' @param method Inference method to be used. See tp_compile_options()
#' for all supported methods.
#' @param iterations The number of MCMC iterations to be run.
#' @param particles The number of SMC particles to be run.
#' @param dir The directory where you want to save the executable. Default is
#' [base::tempdir()]
#' @param output Complete path to the compiled TreePPL program that will be
#' created. Default is dir/<name of the model object>.exe
#' @param ... See tp_compile_options() for all supported arguments.
#'
#' @return The path for the compiled TreePPL program.

compilation <- function(path, args_str) {
  options(scipen = 999)

  dir_path <- tp_tempdir()

  # output
  output_path <- paste0(dir_path, digest::digest(paste(path,args_str), "sha256"), ".exe")

  options <- paste("--output", output_path, args_str)

  # Preparing the command line program
  command <- paste(path, options)

  # Compile program
  # Empty LD_LIBRARY_PATH from R_env for this command specifically
  # due to conflict with internal env from treeppl self-contained
  res <- tp_tpplc_call(command)
  if (res == 1L) {
    stop("Compilation failed")
  }
  return(output_path)
}

#' Write out a custom model to tp_tempdir()
#'
#' @param model A string containing the entire TreePPL code.
#' @param model_file_name An optional name to the file created
#'
#' @returns The path to the file created
#' @export
#'
tp_write_model <- function(model, model_file_name = "tmp_model_file") {

  path <- paste0(tp_tempdir(), model_file_name, ".tppl")
  cat(model, file = path)

  return(path)
}

#' Create a TreePPL model
#'
#' @description
#' `tp_compile` takes TreePPL model and prepares it to be used by
#' [treepplr::tp_run()].
#'
#' @param model One of tree options:
#'   * The full path of the model file that contains the TreePPL code, OR
#'   * A string with the name of a model supported by treepplr
#' (see [treepplr::tp_model_library()]), OR
#'   * A string containing the entire TreePPL code.
#'
#' @return compiled_model from a compiled_model_Template
#' @export

tp_compile <- function(model, method = "mcmc", ...) {
  if (!assertthat::is.string(model)) {
    stop("Input has to be a string.")
  }

  res <- try(file.exists(model), silent = TRUE)

  # If path exists, it's all good
  if (!is(res, "try-error") && res) {
    model_path <- model
    # If path doesn't exist
  } else {
    # It can be a model name in the library
    res_lib <- try(tp_find_model(model), silent = TRUE)
    # model has the name of a known model
    if (!is(res_lib, "try-error") && length(res_lib) != 0) {
      model_path <- res_lib
      # OR model contains the model
      #### (needs to be verified as an appropriate model later) ####
    } else {
      model_path <- tp_write_model(model)
    }
  }
  m <- new("compiled_model_Template", path = model_path)
  user_list <- append(tp_list(...), list(method = method))
  tmp <- list_to_options(user_list)

  m$compile_options <- tmp[["compile"]]
  full_options = append(tmp[["compile"]], tmp[["runtime"]])
  m$exe_path <- compilation(m$path, options_to_string(full_options))
  return(m)
}

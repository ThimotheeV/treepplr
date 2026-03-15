#' Options that can be passed to TreePPL compiler
#'
#' @returns A data frame with the output from the compiler's help <tpplc --help>
#'
tp_compile_options <- function() {

  # Path to self contained #
  if (Sys.info()["sysname"] == "Windows") {
    # No self container for Windows, need to install it manually
    "tpplc"
  } else if (Sys.info()["sysname"] == "Linux") {
    path_sc <- system.file("treeppl-linux", package = "treepplr")
  } else {
    # Mac OS have lots of different names
    path_sc <- system.file("treeppl-mac", package = "treepplr")
  }

  # check if treeppl is in the tmp
  ft <- list.files("/tmp", full.names = TRUE)
  ff <- ft[grepl("treeppl-", ft)]

  # untar to tmp
  if (length(ff) == 0) {
    utils::untar(
      list.files(
        path = path_sc,
        full.names = TRUE
      ),
      exdir = "/tmp"
    )
  }

  # keep the path (this will also work if the 'if' statement above fails)
  ft <- list.files("/tmp", full.names = TRUE)
  ff <- ft[grepl("treeppl-", ft)]

  # command
  path_cmd <- paste0(ff, "//tpplc")
  # treeppl options
  cmd_opt <- system2(command = path_cmd, args = "--help", stdout = TRUE)

  # Preparing the output #

  # find the line containing "Options:"
  x <- which(cmd_opt == "Options:")
  # extract everything after that line
  cmd_opt <- cmd_opt[(x + 1):length(cmd_opt)]
  cmd_opt <- trimws(cmd_opt)
  cmd_opt <- strsplit(cmd_opt, " {2,}", perl = TRUE)

  opt_tab <- do.call(rbind, lapply(cmd_opt, function(x) {
    # if there is no description, make it NA
    if (length(x) == 1) x <- c(x, NA)
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


#' Compile a TreePPL model and create inference machinery
#'
#' @description
#' `tp_compile` compile a TreePPL model and create inference machinery to be
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
#' @export

tp_compile <- function(model,
                       method,
                       iterations = NULL,
                       particles = NULL,
                       dir = NULL,
                       output = NULL,
                       ...) {

  options(scipen=999)

  if(is.null(dir)){
    dir_path <- tp_tempdir()
  } else {
    dir_path <- dir
  }

  # check path, find or write model
  model_file_name <- tp_model(model)

  musts <- paste("-m", method)

  if(is.null(iterations) & is.null(particles)){
    stop("If using MCMC, please choose number of iterations.
         If using SMC, please choose number of particles.")
  }

  if(!is.null(iterations)){
    musts <- paste(musts, "--particles", iterations)
  }

  if(!is.null(particles)){
    musts <- paste(musts, "--particles", particles)
  }

  # create string with other options to tpplc
  args_list <- list(...)
  args_vec <- unlist(args_list)

  vec <- c()
  for(i in seq_along(args_vec)) {
    str <- paste0("--", names(args_vec[i]), " ", args_vec[[i]])
    vec <- c(vec, str)
  }

  args_str <- paste(vec, collapse = " ")

  # output
  if(is.null(output)){
    output_path <- paste0(dir_path, names(model_file_name), ".exe")
  } else {
    output_path <- output
  }

  options <- paste("--output", output_path, args_str)

  # Preparing the command line program
  tpplc_path <- installing_treeppl()   #### move this? ####
  command <- paste(tpplc_path, model_file_name, musts, options)

  # Compile program
  # Empty LD_LIBRARY_PATH from R_env for this command specifically
  # due to conflict with internal env from treeppl self-contained
  system(paste0("LD_LIBRARY_PATH= MCORE_LIBS= ", command))

  return(output_path)
}



#' Import a TreePPL model
#'
#' @description
#' `tp_model` takes TreePPL code and prepares it to be used by
#' [treepplr::tp_compile()].
#'
#' @param model_input One of tree options:
#'   * The full path of the model file that contains the TreePPL code, OR
#'   * A string with the name of a model supported by treepplr
#' (see [treepplr::tp_model_library()]), OR
#'   * A string containing the entire TreePPL code.
#'
#' @return The path to the TreePPL model file
#' @export
tp_model <- function(model_input) {

  if (!assertthat::is.string(model_input)){
    stop("Input has to be a sring.")
  }

  res <- try(file.exists(model_input), silent = TRUE)

  # If path exists, it's all good
  if (!is(res,"try-error") && res) {
    model_path <- model_input
    # name the model
    if (is.null(names(model_path))){
      names(model_path) <- "custom_model"
    }

    # If path doesn't exist
  } else {
    # It can be a model name in the library
    res_lib <- try(tp_find_model(model_input), silent = TRUE)
    # model_input has the name of a known model
    if (!is(res_lib,"try-error") && length(res_lib) != 0) {
      model_path <- res_lib
      names(model_path) <- model_input
      # OR model_input contains the model
      #### (needs to be verified as an appropriate model later) ####
    } else {
      model_path <- tp_write_model(model_input)
      names(model_path) <- "custom_model"
    }
  }
  return(model_path)
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


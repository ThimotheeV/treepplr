#' Options that can be passed to a TreePPL program
#'
#' @returns A string with the output from the executable's help
#' @export
#'
tp_run_options <- function() {

  #### under development here and in treeppl ####

  # text from treeppl executable --help
  return()

}


#' Run a TreePPL program
#'
#' @description
#' Run TreePPL and return output.
#'
#' @param compiled_model a [base::character] with the full path to the compiled model
#' outputted by [treepplr::tp_compile].
#' @param data a [base::character] with the full path to the data file in TreePPL
#' JSON format (as outputted by [treepplr::tp_data]).
#' @param n_runs When using MCMC, a [base::integer] giving the number of runs to be done.
#' @param n_sweeps When using SMC, a [base::integer] giving the number of SMC sweeps to be done.
#' @param dir a [base::character] with the full path to the directory where you
#' want to save the output. Default is [base::tempdir()].
#' @param out_file_name a [base::character] with the name of the output file in
#' JSON format. Default is "out".
#' @param ... See [treepplr::tp_run_options] for all supported arguments.
#'
#'
#' @return A list of TreePPL output in parsed JSON format.
#' @export
#'
#' @examples
#' \dontrun{
#' # When using SMC
#' # compile model and create SMC inference machinery
#' exe_path <- tp_compile(model = "coin", method = "smc-bpf", particles = 2000)
#'
#' # prepare data
#' data_path <- tp_data(data_input = "coin")
#'
#' # run TreePPL
#' result <- tp_run(exe_path, data_path, n_sweeps = 2)
#'
#'
#' # When using MCMC
#' # compile model and create MCMC inference machinery
#' exe_path <- tp_compile(model = "coin", method = "mcmc-naive", iterations = 2000)
#'
#' # prepare data
#' data_path <- tp_data(data_input = "coin")
#'
#' # run TreePPL
#' result <- tp_run(exe_path, data_path, n_runs = 2)
#' }

tp_run <- function(compiled_model,
                   data,
                   n_runs = 1,
                   n_sweeps = 1,
                   dir = NULL,
                   out_file_name = "out",
                   ...) {

  if(is.null(n_runs) & is.null(n_sweeps)){
    stop("At least one of n_runs and n_sweeps needs to be passed")
  }

  #n_string <- ""
  #if(!is.null(n_runs)){
    #### change to --iterations when it's fixed in treeppl ####
    #n_string <- paste0(n_string, "--sweeps ", n_runs, " ")
  #}

  #if(!is.null(n_sweeps)){
    #n_string <- paste0(n_string, "--sweeps ", n_sweeps, " ")
  #}

  if(is.null(dir)){
    dir_path <- tp_tempdir()
  } else {
    dir_path <- dir
  }

  output_path <- paste0(dir_path, out_file_name, ".json")

  # Empty LD_LIBRARY_PATH from R_env for this command specifically
  # due to conflict with internal env from treeppl self container
  command <- paste("LD_LIBRARY_PATH= MCORE_LIBS=",
                   compiled_model,
                   data,
                   #n_string,
                   paste(">", output_path)
                   )
  system(command)

  # simple parsing
  #### change this? ####
  json_out <- readLines(output_path) |>
    lapply(jsonlite::fromJSON, simplifyVector = FALSE)

  return(json_out)
}




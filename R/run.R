#' Run a TreePPL program
#'
#' @description
#' Run TreePPL and return output.
#'
#' @param compiled_model a [base::character] with the full path to the compiled model
#' outputted by [treepplr::tp_compile].
#' @param data a [base::character] with the full path to the data file in TreePPL
#' JSON format (as outputted by [treepplr::tp_data]).
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
#' result <- tp_run(exe_path, data_path, sweeps = 2)
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
                   dir = NULL,
                   out_file_name = "out",
                   ...) {
  if (is.null(dir)) {
    dir_path <- tp_tempdir()
  } else {
    dir_path <- dir
  }

  output_path <- paste0(dir_path, out_file_name, ".json")

  #If a list have multiple time the same key
  # list[[key]] will return the first key
  # Exemple
  #> lis <- list(method = "mcmc", method = "smc")
  #> lis[["method"]] => "mcmc"
  # So the user list have priority
  options <- list_to_options(tp_list(...))

  if(length(options[["compile"]]) != 0) {
    stop("Can't give compile time options here")
  }

  # Empty LD_LIBRARY_PATH from R_env for this command specifically
  # due to conflict with internal env from treeppl self container
  command <- paste(
    "LD_LIBRARY_PATH= ",
    compiled_model$exe_path,
    data,
    options_to_string(options[["runtime"]]),
    paste(">", output_path)
  )
  tp_tpplc_call(command)

  # simple parsing
  #### change this? ####
  json_out <- readLines(output_path) |>
    lapply(jsonlite::fromJSON, simplifyVector = FALSE)

  return(json_out)
}

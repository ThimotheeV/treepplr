temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

cat(crayon::yellow("\nTest-compile : Compilation.\n"))


test_that("Test-compile_1a : tp_compile SMC", {
  cat("\tTest-compile_1a : tp_compile SMC \n")

  model <- treepplr::tp_model("coin")

  treepplr::tp_compile(model, method = "smc-bpf", particles = 10)

  expect_no_error(readBin(paste0(temp_dir, "coin.exe"), "raw", 10e6))
})


test_that("Test-compile_1b : tp_compile MCMC", {
  cat("\tTest-compile_1b : tp_compile MCMC \n")

  model_right <- treepplr::tp_model("coin")

  treepplr::tp_compile(model_right, method = "mcmc-lightweight", iterations = 10)

  expect_no_error(readBin(paste0(temp_dir, "coin.exe"), "raw", 10e6))

})


test_that("Test-compile_2a : tp_model model name", {
  cat("\tTest-compile_2a : tp_model\n")

  model <- treepplr::tp_model("coin")

  version <- list.files("/tmp",
                        pattern = paste0("treeppl-", TPPLC_VERSION),
                        full.names = TRUE)

  model_right = system(paste0("find ", version," -name coin.tppl"),
               intern = T)

  names(model_right) <- "coin"

  expect_equal(model, model_right)
})


test_that("Test-compile_2b : tp_model model path ", {
  cat("\tTest-compile_2b : tp_model\n")

  version <- list.files("/tmp",
                        pattern = paste0("treeppl-", TPPLC_VERSION),
                        full.names = TRUE)

  model_right = system(paste0("find ", version," -name coin.tppl"),
                       intern = T)
  model <- treepplr::tp_model(model_right)
  names(model_right) <- "custom_model"

  expect_equal(model, model_right)
})


test_that("Test-compile_3a : tp_write path", {
  cat("\tTest-compile_3a : tp_write\n")

  path <- treepplr::tp_write_model("T : bla bla bla")
  expect_true(file.exists(path))

})


test_that("Test-compile_3b : tp_write content", {
  cat("\tTest-compile_3b : tp_write\n")

  content_right <- "M : bla bla bla"
  path <- treepplr::tp_write_model(content_right)

  content <- readLines(path, warn = FALSE)
  expect_equal(content_right, content)

})


test_that("Test-compile_4 : tp_model model string ", {
  cat("\tTest-compile_4 : tp_model\n")

  model_string <- "S : bla bla bla"
  model <- treepplr::tp_model(model_string)
  model_right <- paste0(temp_dir, "tmp_model_file.tppl")
  names(model_right) <- "custom_model"

  expect_equal(model, model_right)
})

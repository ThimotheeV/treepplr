temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)
require(ape)

cat(crayon::yellow("\nTest-json : Test json function for treepplr.\n"))

ex_tree <- data.frame(
  Type = c(
    "Leaf",
    "Leaf",
    "Leaf",
    "Leaf",
    "Leaf",
    "Node",
    "Node",
    "Node",
    "Node"
  ),
  Label = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  Age = c(1, 1, 4, 1.5, 1.5, NA, 5, 2, 2.5),
  Left = c(NA, NA, NA, NA, NA, 7, 1, 3, 4),
  Right = c(NA, NA, NA, NA, NA, 8, 2, 9, 5)
)

testthat::test_that("Test-json_1a : tree_age_cumul", {
  cat("\tTest-json_1a : tree_age_cumul with root-to-tip\n")

  top_down_tree <- data.frame(
    Type = c(
      "Leaf",
      "Leaf",
      "Leaf",
      "Leaf",
      "Leaf",
      "Node",
      "Node",
      "Node",
      "Node"
    ),
    Label = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    Age = c(6, 6, 6, 6, 6, 0.0, 5, 2, 4.5),
    Left = c(NA, NA, NA, NA, NA, 7, 1, 3, 4),
    Right = c(NA, NA, NA, NA, NA, 8, 2, 9, 5)
  )

  res_tree <- treepplr:::tree_age_cumul(ex_tree, 6, age = "root-to-tip")

  testthat::expect_equal(top_down_tree, res_tree)
})

testthat::test_that("Test-json_1b : tree_age_cumul", {
  cat("\tTest-json_1b : tree_age_cumul with tip-to-root\n")

  down_top_tree <- data.frame(
    Type = c(
      "Leaf",
      "Leaf",
      "Leaf",
      "Leaf",
      "Leaf",
      "Node",
      "Node",
      "Node",
      "Node"
    ),
    Label = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    Age = c(0.0, 0.0, 0.0, 0.0, 0.0, 6, 1, 4, 1.5),
    Left = c(NA, NA, NA, NA, NA, 7, 1, 3, 4),
    Right = c(NA, NA, NA, NA, NA, 8, 2, 9, 5)
  )

  res_tree <- treepplr:::tree_age_cumul(ex_tree, 6, age = "tip-to-root")

  testthat::expect_equal(down_top_tree, res_tree)
})

testthat::test_that("Test-json_2 : tp_phylo_to_tppl_tree", {
  cat("\tTest-json_2 : tp_phylo_to_tppl_tree with phylo\n")

  ex_tree_phylo <- ape::read.tree(text = "((1:1,2:1)7:5,(3:4,(4:1.5,5:1.5)9:2.5)8:2)6;")

  res_tree <- treepplr:::tp_phylo_to_tppl_tree(ex_tree_phylo)

  testthat::expect_equal(6, res_tree[[1]])
  testthat::expect_equal(ex_tree, res_tree[[2]])
})

resources <- readRDS(paste0(
  system.file("extdata", package = "treepplr"),
  treepplr:::sep(),
  "Alcedinidae_tre.rds"
))

ref <- jsonlite::fromJSON(paste0(
  system.file("extdata", package = "treepplr"),
  treepplr:::sep(),
  "Alcedinidae_tre.json"
))

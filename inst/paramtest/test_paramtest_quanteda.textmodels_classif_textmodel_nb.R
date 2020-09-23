library(mlr3learners.quanteda.textmodels)

test_that("classif.textmodel_nb", {
  learner = lrn("classif.textmodel_nb")
  fun = quanteda.textmodels::textmodel_nb # replace!
  exclude = c(
    # Examples how to exclude certain parameters. Always comment why a parameter
    # was excluded!
    "formula", # handled via mlr3
    "data", # handled via mlr3
    "weights", # handled via mlr3
    "control", # handled to mboost::boost_control
    "..." # not used
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
    paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})

# example for checking a "control" function of a learner
test_that("classif.textmodel_nb_control", {
  learner = lrn("classif.textmodel_nb")
  fun = quanteda.textmodels::textmodel_nb_control # replace!
  exclude = c(
    "center", # deprecated
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("\nMissing parameters:\n",
    paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})

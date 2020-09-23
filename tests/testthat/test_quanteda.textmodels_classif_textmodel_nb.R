context("classif.textmodel_nb")

test_that("autotest", {
  learner = LearnerClassifNaiveBayesText$new()
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

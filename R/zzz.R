#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 mlr_learners LearnerClassif LearnerRegr
"_PACKAGE"

# nocov start
register_mlr3 = function(libname, pkgname) {
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  x$add("classif.textmodel_nb", LearnerClassifNaiveBayesText)
}

.onLoad = function(libname, pkgname) { # nolint
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(),
    action = "append")
}

.onUnload = function(libpath) { # nolint
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "quanteda.textmodels"],
    action = "replace")
}
# nocov end

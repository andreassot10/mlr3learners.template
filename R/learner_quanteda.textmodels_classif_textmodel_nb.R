#' @title CLassification Naive Bayes for Texts Learner
#' @author andreassot10
#' @name mlr_learners_classif.textmodel_nb
#'
#' @description
#' A [mlr3::Learnerclassif] implementing textmodel_nb from package
#'   \CRANpkg{quanteda.textmodels}.
#' Calls [quanteda.textmodels::textmodel_nb()].
#'
#' @templateVar id classif.textmodel_nb
#' @template section_dictionary_learner
#'
#' @references
#' <optional>
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerClassifNaiveBayesText = R6::R6Class("LearnerClassifNaiveBayesText",
  inherit = LearnerClassif,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(
            id = "smooth", default = 1, lower = -Inf, upper = Inf,
            tags = "train"),
          ParamFct$new(
            id = "prior", default = "uniform",
            levels = c("docfreq", "termfreq", "uniform"),
            special_vals = list(FALSE), tags = "train"),
          ParamFct$new(
            id = "distribution", default = "multinomial",
            levels = c("Bernoulli", "multinomial"),
            special_vals = list(FALSE), tags = "train")
        )
      )

      super$initialize(
        id = "classif.textmodel_nb",
        packages = "quanteda.textmodels",
        feature_types = "character",
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass", "multiclass"),
        man = "mlr3extralearners::mlr_learners_classif.textmodel_nb"
      )
    }
  ),


  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tags = "train")

      formula = task$formula()
      data = task$data()
      y = unlist(data[, 1])
      x =
        quanteda::dfm(
          as.character(unlist(data[, -1])),
          remove = stopwords("english"),
          remove_punct = TRUE,
          remove_symbols = TRUE,
          remove_numbers = TRUE
        )

      mlr3misc::invoke(quanteda.textmodels::textmodel_nb,
        # formula = formula, data = data, .args = pars)
        x = x, y = y, .args = pars)
    },

    .predict = function(task) {

      newdata =
        quanteda::dfm(
          as.character(unlist(task$data()[, -1])),
          remove = stopwords("english"),
          remove_punct = TRUE,
          remove_symbols = TRUE,
          remove_numbers = TRUE
        )
      newdata =
        quanteda::dfm_match(
          newdata,
          features = quanteda::featnames(self$model$x)
        )
      type = ifelse(self$predict_type == "response", "response", "prob")

      pred = mlr3misc::invoke(predict, self$model,
        newdata = newdata,
        # type = type, .args = pars)
        type = type)

      if (self$predict_type == "response") {
        PredictionClassif$new(task = task, response = pred)
      } else {
        PredictionClassif$new(task = task, prob = pred)
      }
    }
  )
)

lrns_dict$add("classif.textmodel_nb", LearnerClassifNaiveBayesText)

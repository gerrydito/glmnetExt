#' This function makes it easier to use the results of cross-validation to make a prediction.
#' @param object fitted 'cv.glmnet.all'.
#' @param newx A Matrix new values for \code{x} at which predictions are to be.
#' @param \dots Not used. Other arguments to predict.
#' @return The object returned depends on the \dots{} argument which is passed.
#' on to the \code{predict} method for \code{glmnet} objects.
#' @examples
#' data("leukemia", package = "varbvs")
#' X <- leukemia$x
#' y <- leukemia$y
#' mod <- cv.glmnet.all(x = X, y = y, family = "binomial", type.measure = "deviance")
#' predict(mod, X)
#' @importFrom stats predict
#' @importFrom glmnet predict.glmnet
#' @rdname predict.cv.glmnet.all
#' @export
predict.cv.glmnet.all <- function(object, newx, ...) {
  predict(object$model, newx, ...)[, 1]
}

#' Print a summary of the results of cross-validation for a glmnet model.
#' @param object fitted 'cv.glmnet.all' object
#' @param \dots Not used. Other arguments for summary.
#' @examples
#' \dontrun{
#' data("leukemia", package = "varbvs")
#' X <- leukemia$x
#' y <- leukemia$y
#' mod <- cv.glmnet.all(x = X, y = y, family = "binomial", type.measure = "deviance")
#' summary(mod)
#' }
#' @import broom
#' @import data.table
#' @import tidytable
#' @rdname summary.cv.glmnet.all
#' @export
summary.cv.glmnet.all <- function(object, ...) {
  .data=NULL
  tidy(object$model) %>%
    mutate.(alpha = object$best_param$alpha) %>%
    relocate.(.data$alpha, .before = .data$lambda) %>%
    select.(-.data$step)
}

#' Does k-fold cross-validation for glmnet, produces a plot, and returns a value for alpha and lambda
#' @param alpha Optional user-supplied a vector of alpha values for which to do k-fold cross-validation; default is NULL, and glmnetExt chooses its own sequence.
#' @param nalpha The number of alpha values - default is 50.
#' @param type.measure loss to use for cross-validation. see \code{cv.glmnet} for more details.
#' @param \dots Other arguments that can be passed to \code{cv.glmnet}
#' @return an object of class \code{"cv.glmnet.all"} is returned, which is a list
#' with the ingredients of the cross-validation fit.
#' \item{model}{a fitted \code{glmnet} object for the full data.}
#' \item{best_param}{value, standard error and confidence interval of alpha and lambda which gives minimum/maximum metric}
#' \item{all_param}{value, standard error and confidence interval of all alpha and lambda }
#' \item{type.measure}{loss to use for cross-validation}
#' @seealso \code{glmnet::cv.glmnet}
#' @examples
#' \dontrun{
#' data("leukemia",package = "varbvs")
#' X=leukemia$x
#' y=leukemia$y
#' mod <- cv.glmnet.all(x=X,y=y,family="binomial",type.measure = "deviance")
#' }
#' @import glmnet
#' @import data.table
#' @import tidytable
#' @import broom
#' @rdname cv.glmnet.all
#' @export
cv.glmnet.all=function(alpha=NULL,nalpha=50, type.measure = c("default","mse",
                                                     "deviance", "class", "auc", "mae","C"),...){

if(!is.null(alpha) && length(alpha) < 2){

  stop("Need more than one value of alpha for cv.glmnet.all")

}

alpha <- seq(0,1,length.out=nalpha)

.data=NULL

alpha_lambda <- map_dfr.(alpha,function(x){
model <- cv.glmnet(alpha=x,
                   type.measure=type.measure,...)

info_model <- glance(model)
lambda_model <- info_model %>%
  pull.("lambda.min")


lambda_metric <- tidy(model) %>%
  filter.(.data$lambda==lambda_model) %>%
  mutate.(alpha=x) %>%
  relocate.(.data$alpha,.before=.data$lambda) %>%
  rename.("metric"="estimate")

return(lambda_metric)
})

if(any(type.measure %in% c("auc","C"))){
  info_model <- alpha_lambda %>%
    slice_max.(.data$metric)
}else{
  info_model <- alpha_lambda %>%
    slice_min.(.data$metric)

}
model <- glmnet(alpha=info_model$alpha,
                lambda = info_model$lambda,
                type.measure=type.measure,
                ...)

result <- list("model"=model,
               "best_param"=info_model,
               "all_param"=alpha_lambda,
               "type.measure"=type.measure
               )
class(result) <- "cv.glmnet.all"
return(result)
}

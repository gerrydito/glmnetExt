estimate_accel_const <- function(x, theta_hat) {
  n <- length(x)
  I <- map_dbl.(seq(n), function(i) {
    xnew <- x[-i]
    theta_jack <- var(xnew, na.rm = TRUE) /
      mean(xnew, na.rm = TRUE)^2 -
      (1 / mean(xnew, na.rm = TRUE))
    I <- (n - 1) * (theta_hat - theta_jack)
    return(I)
  })
  accel_const_hat <- (sum(I^3, na.rm = TRUE) /
    sum(I^2, na.rm = TRUE)^1.5) / 6
  return(accel_const_hat)
}

calc_bca <- function(x, theta_hat, alpha) {
  x <- x %>% unlist(use.names = F)
  names(x) <- NULL
  theta_hat <- theta_hat %>% unlist(use.names = F)

  u <- c(alpha / 2, 1 - alpha / 2)

  mean_x_theta <- mean(ifelse(is.na(x <= theta_hat),
    F,
    x <= theta_hat
  ))
  z0 <- qnorm(mean_x_theta + 1e-10)
  zu <- qnorm(u)
  a <- estimate_accel_const(x, theta_hat)

  u_adjusted <- pnorm(z0 + (z0 + zu) / (1 - a * (z0 + zu)))


  result <- quantile(x,
    u_adjusted,
    na.rm = TRUE
  ) %>%
    set_names(nm = paste0("bsa_", u * 100))
  return(result)
}

imp_variable <- function(x, left, right) {
  criteria <- !between.(0, left, right)
  imp <- abs(x) * criteria
}
#' Compute Bootstrap Confidence Interval and importance measure of glmnet coefficients
#' @param X x matrix of predictor variables.See \code{glmnet} for more details.
#' @param y y response variable. See \code{glmnet} for more details.
#' @param B number of bootstrap replicates. The Default is 500.
#' @param alpha The elasticnet's parameter. See \code{glmnet} for more details.
#' @param lambda The shrinkage's parameter. See \code{glmnet} for more details.
#' @param paralel Use parallel? The default is FALSE.
#' @param workers A positive numeric scalar or a function specifying the maximum number of parallel futures that can be active at the same time before blocking. The default is 2.
#' @param saveResample Saving Bootstrap resample. The default is TRUE.
#' @param conf confidence level with value [0,1]. The default is 0.95.
#' @param ... Other arguments that can be passed to \code{glmnet}
#' @return an object of class \code{"cv.glmnet.all"} is returned, which is a list
#' with the ingredients of the cross-validation fit.
#' \item{model}{a fitted \code{glmnet} object for the full data.}
#' \item{boot_result}{table contains importance measure, bootstrap Percentile CI, bootstrap bsa CI,bootstrap standard deviation and bootstrap mean }
#' \item{boot_resample}{All Bootstrap Resampling data}
#' @references Hansu Kim, Tae Hee Lee (2021) \emph{A robust elastic net via bootstrap method under sampling uncertainty for significance analysis of high-dimensional design problems, Knowledge-Based Systems,Vol. 225}
#' @examples
#' \dontrun{
#' data("leukemia", package = "varbvs")
#' X <- leukemia$x
#' y <- leukemia$y
#' mod <- cv.glmnet.all(
#'   x = X, y = y,
#'   family = "binomial",
#'   type.measure = "deviance"
#' )
#' info_model <- mod$best_param
#' mod_boot <- boot_glmnet(
#'   X = X, y = y,
#'   family = "binomial", alpha = info_model$alpha,
#'   lambda = info_model$lambda, B = 50
#' )
#' }
#' @import glmnet
#' @import data.table
#' @import tidyselect
#' @import tidytable
#' @importFrom purrr array_tree reduce set_names
#' @import broom
#' @import future
#' @import furrr
#' @import glmnet
#' @importFrom  stats sd quantile pnorm qnorm var
#' @importFrom utils globalVariables
#' @rdname boot_glmnet
#' @export
boot_glmnet <- function(X, y, B = 500, alpha, lambda,
                        paralel = FALSE, workers = 2,
                        saveResample = TRUE, conf = 0.95, ...) {
  if (paralel) {
    plan("multisession", workers = workers)
  }

  .data=NULL

  coef_boot <- future_map(seq(B), function(i) {
    index_rep <- sample.int(n = nrow(X), replace = TRUE)
    X_rep <- X[index_rep, ]
    y_rep <- y[index_rep]

    model <- glmnet(x = X_rep, y_rep, lambda = lambda, alpha = alpha, ...)

    coef_model <- tidy(model) %>%
      select.("term", "estimate")
    names(coef_model)[2] <- c(paste0("estimate", i))

    return(coef_model)
  }, .options = furrr_options(seed = 7))

  coef_boot_df <- reduce(.x = coef_boot, .f = full_join., )
  rm(coef_boot)

  coef_boot_mean <- coef_boot_df %>%
    mutate.(mean_boot = rowMeans(across.(vars_select_helpers$where(is.numeric)),
      na.rm = TRUE
    )) %>%
    select.(.data$term, .data$mean_boot)

  coef_boot_sd <- coef_boot_df %>%
    mutate_rowwise.(
      sd_boot = sd(c_across.(vars_select_helpers$where(is.numeric)),
        na.rm = TRUE
      )
    ) %>%
    select.(.data$term, .data$sd_boot)

  coef_boot_perc <- coef_boot_df %>%
    select.(-.data$term) %>%
    array_tree(margin = 1) %>%
    map.(quantile,
      probs = c(
        (1 - conf) / 2,
        1 - ((1 - conf) / 2)
      ),
      na.rm = TRUE
    ) %>%
    reduce(rbind) %>%
    as_tidytable()
  names(coef_boot_perc) <- paste0("perc_", c(
    ((1 - conf) / 2) * 100,
    (1 - ((1 - conf) / 2)) * 100
  ))

  coef_boot_perc <- coef_boot_df %>%
    select.(.data$term) %>%
    bind_cols.(coef_boot_perc)


  model <- glmnet(
    x = X, y = y, lambda = lambda,
    alpha = alpha, ...
  )


  coef_model <- tidy(model) %>%
    select.("term", "estimate")


  coef_boot_bsa <- coef_boot_df %>%
    inner_join.(coef_model, by = "term") %>%
    select.(-.data$term) %>%
    array_tree(margin = 1) %>%
    map2.(
      .y = coef_model %>%
        pull.(.data$estimate),
      .f = calc_bca, alpha = (1 - conf)
    ) %>%
    reduce(rbind) %>%
    as_tidytable()

  coef_boot_bsa <- coef_model %>%
    select.(.data$term) %>%
    bind_cols.(coef_boot_bsa)



  coef_boot_fin <- reduce(
    .x = list(
      coef_boot_mean,
      coef_boot_sd,
      coef_boot_perc,
      coef_boot_bsa
    ),
    .f = inner_join.,
    by = "term"
  )
  rm(coef_boot_bsa, coef_boot_mean, coef_boot_sd, coef_boot_perc)

  coef_boot_fin <- coef_boot_fin %>%
    mutate.(importance = imp_variable(.data$mean_boot, .data$bsa_2.5, .data$bsa_97.5)) %>%
    mutate.(importance = (.data$importance / max(.data$importance)) * 100)

  coef_model_fin <- left_join.(coef_model,
    coef_boot_fin,
    by = "term"
  ) %>%
    arrange.(desc.("importance")) %>%
    select.(
      .data$term, .data$estimate, .data$importance,
      .data$bsa_2.5, .data$bsa_97.5,
      .data$perc_2.5, .data$perc_97.5,
      .data$sd_boot, .data$mean_boot
    )

  if (!saveResample) {
    coef_boot_df <- NULL
  }
  coef_boot_df <- left_join.(coef_model,
    coef_boot_df,
    by = "term"
  )
  final_result <- list(
    model = model,
    boot_result = coef_model_fin,
    boot_resample = coef_boot_df
  )
  class(final_result) <- "boot_glmnet"
  return(final_result)
}

#' Plots the bootstrap result.
#' @param x fitted 'cv.glmnet.all'
#' @param \dots Not used. Other arguments to plot.
#' @examples
#' data("leukemia", package = "varbvs")
#' X <- leukemia$x
#' y <- leukemia$y
#' mod <- cv.glmnet.all(x = X, y = y, family = "binomial", type.measure = "deviance")
#' info_model <- mod$best_param
#' mod_boot <- boot_glmnet(
#'   X = X, y = y,
#'   family = "binomial",
#'   alpha = info_model$alpha,
#'   lambda = info_model$lambda, B = 50
#' )
#' plot(mod_boot)
#' @importFrom ggplot2 geom_vline aes element_text scale_y_continuous scale_x_continuous ggtitle xlab theme scale_color_manual
#' @import ggpubr
#' @import broom
#' @import data.table
#' @import tidytable
#' @rdname plot.boot_glmnet
#' @export
plot.boot_glmnet <- function(x, ...) {
  data_plot <- x$boot_resample %>%
    pivot_longer.(cols = -.data$term, names_to = "terms") %>%
    pivot_wider.(names_from = .data$term, values_from = value)

  coef_model <- tidy(x$model)

  .data=NULL
  bsa_conf <- x$boot_result %>%
    select.(.data$term, contains("bsa"))

  if (nrow(x$boot_result) < 6) {
    selected_n <- nrow(x$boot_result)
  } else {
    selected_n <- 6
  }

  best_predictor <- x$boot_result %>%
    slice.(1:selected_n) %>%
    pull.(.data$term)

  plot_best_predictor <- map.(best_predictor, function(i) {
    bsa_conf_selected <- bsa_conf %>%
      filter.(.data$term == i) %>%
      select.(-.data$term) %>%
      unlist(use.names = FALSE)

    perc_conf_selected <- x$boot_result %>%
      select.(.data$term, contains("perc")) %>%
      filter.(.data$term == i) %>%
      select.(-.data$term) %>%
      unlist(use.names = FALSE)

    coef_selected <- coef_model %>%
      filter.(.data$term == i) %>%
      select.(.data$estimate) %>%
      unlist(use.names = FALSE)

    ggdensity(data_plot,
      x = i,
      fill = "#D6D1E0"
    ) +
      geom_vline(aes(xintercept = bsa_conf_selected[1], color = "bsa"), size = 1.25) +
      geom_vline(aes(xintercept = bsa_conf_selected[2], color = "bsa"), size = 1.25) +
      geom_vline(aes(xintercept = perc_conf_selected[1], color = "perc"), size = 1.25) +
      geom_vline(aes(xintercept = perc_conf_selected[2], color = "perc"), size = 1.25) +
      geom_vline(aes(xintercept = coef_selected[1], color = "estimate"), size = 1.25) +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle(i) +
      xlab("") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_color_manual(
        name = "",
        breaks = c("bsa", "perc", "estimate"),
        values = c("bsa" = "#926ED4", "perc" = "#FAC6AA", "estimate" = "#23C45C")
      ) +
      theme(plot.title = element_text(hjust = 0.5))
  })

  p <- ggarrange(
    plotlist = plot_best_predictor,
    nrow = ceiling(selected_n / 2), ncol = 2,
    common.legend = TRUE
  )
  p
}

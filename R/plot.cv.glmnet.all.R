#' Plots the cross-validation contour.
#' @param x fitted 'cv.glmnet.all'.
#' @param color.temp color template,should be either "temp1","temp2", and "temp3" or vector of colors with length>1. The default is "temp1".
#' @param n.color.grid number of color grid. The default is 30.
#' @param reversescale should color template reverse order? The default is FALSE
#' @param label.font.size font size of label. The default is 10.
#' @param color.label.font color of label. The default is "white".
#' @param ... Not used. Other arguments to plot.
#' @details
#' The labels are shown on the plot are several non-zero coefficients. Labels are only shown for the top five rankings based on a metric.
#' @examples
#' data("leukemia", package = "varbvs")
#' X <- leukemia$x
#' y <- leukemia$y
#' mod1 <- cv.glmnet.all(x = X, y = y, family = "binomial", type.measure = "deviance")
#' plot(mod1)
#' @import plotly
#' @import data.table
#' @import tidytable
#' @import grDevices
#' @export
plot.cv.glmnet.all <- function(x,
                               color.temp = "temp1",
                               n.color.grid = 30,
                               reversescale = FALSE,
                               label.font.size = 10,
                               color.label.font = "white",
                               ...) {
  if (any(x$type.measure %in% c("auc", "C"))) {
    criteria <- -1
  } else {
    criteria <- 1
  }

  if (!any(color.temp %in% paste0("temp", 1:3)) && (length(color.temp) == 1)) {
    stop("color.temp must be temp1,temp2,temp or character color vector with length>1")
  }

  .data=NULL
  label_nzero <- x$all_param %>%
    mutate.(rank = frank(criteria * .data$metric,
      ties.method = "first"
    )) %>%
    mutate.(nzero = case_when.(
      rank < 6 ~ as.character(.data$nzero),
      rank >= 6 ~ ""
    )) %>%
    pull.(.data$nzero)


  color_temp <- list(
    "temp1" = c("#145277", "#83D0CB"),
    "temp2" = c("#010101", "#E95D3C"),
    "temp3" = c("#210CAE", "#4DC9E6")
  )
  if (length(color.temp) > 1) {
    colors0 <- color.temp
  } else {
    colors0 <- color_temp[[color.temp]]
  }
  contour_color <- colorRampPalette(
    colors = colors0,
    interpolate = "linear"
  )




  fig1 <- plot_ly(
    data = x$all_param,
    x = ~ log(lambda),
    y = ~alpha,
    z = ~metric,
    type = "contour",
    colors = contour_color(n = n.color.grid),
    reversescale = reversescale,
    contours = list(
      start = min(x$all_param$metric),
      end = max(x$all_param$metric),
      size = sd(x$all_param$metric)
    )
  )

  fig1 %>%
    add_annotations(
      text = ~label_nzero,
      showarrow = FALSE,
      font = list(
        color = color.label.font,
        size = label.font.size
      )
    ) %>%
    colorbar(title = x$type.measure)
}

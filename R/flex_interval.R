utils::globalVariables(c("IID", "PRS", "Variance", "Lower_Limit", "Upper_Limit"))

#' flex_interval function
#'
#' This function visualizes individual polygenic risk scores (PRS) with corresponding 
#' confidence intervals. It supports customization of reference lines, colors, 
#' themes, and interactivity.
#'
#' @param CI_data Data frame containing columns: `IID`, `PRS`, `Variance`, 
#' `Lower_Limit`, and `Upper_Limit`.
#' @param user_ref_line Numeric or vector. Horizontal reference line(s) to be added. 
#' If `NULL`, defaults to mean PRS. Default is `NULL`.
#' @param user_ref_color Character or vector. Color(s) of reference line(s). 
#' Default is `"red"`.
#' @param user_ref_linetype Character or vector. Linetype(s) for reference line(s). 
#' Default is `"dashed"`.
#' @param user_ref_visible Logical. Whether to display the reference line(s). 
#' Default is `TRUE`.
#' @param user_point_color Character. Color of PRS points. Default is `"blue"`.
#' @param user_point_size Numeric. Size of PRS points. Default is `3`.
#' @param user_errorbar_color Character. Color of confidence interval error bars. 
#' Default is `"black"`.
#' @param user_errorbar_size Numeric. Width of error bars. Default is `0.2`.
#' @param interactive Logical. If `TRUE`, returns an interactive plot using `plotly`. 
#' Default is `TRUE`.
#' @param user_theme A ggplot2 theme object. Default is `theme_minimal()`.
#' @param user_theme_specifications Additional theme specifications. Default is an 
#' empty `theme()` object.
#' @param user_title Character. Title of the plot. Default is `"PRS with Confidence Intervals"`.
#' @param user_x_label Character. X-axis label. Default is `"Individual"`.
#' @param user_y_label Character. Y-axis label. Default is `"Polygenic Risk Score (PRS)"`.
#'
#' @return A ggplot object (static plot) or a plotly object (interactive plot),
#' depending on the value of `interactive`.
#'
#' @import reshape2 
#' @import utils
#' @import gridExtra
#' @import nortest
#' @import ggforce
#' @import cowplot
#' @import grid
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#' library(dplyr)
#' library(nortest)
#' library(ggforce)
#' library(reshape2)
#' library(gridExtra)
#' library(grid)
#' library(cowplot)
#' set.seed(123)
#' example_data <- data.frame(
#'   IID = paste0("ID_", 1:8),
#'   PRS = rnorm(8, 0, 1),
#'   Variance = runif(8, 0.01, 0.1)
#' )
#' example_data$Lower_Limit <- example_data$PRS - 1.96 * sqrt(example_data$Variance)
#' example_data$Upper_Limit <- example_data$PRS + 1.96 * sqrt(example_data$Variance)
#' CI_data <- example_data
#'
#' # Basic 
#' flex_interval(CI_data)
#'
#' # Add custom reference lines
#' flex_interval(CI_data,
#'              user_ref_line = c(1, -1),
#'              user_ref_color = c("darkgreen", "darkred"),
#'              user_ref_linetype = c("dotted", "twodash"))
#'
#' # Static plot with theme customization
#' flex_interval(CI_data, interactive = FALSE,
#'              user_theme_specifications = theme(axis.text.x = element_text(angle = 45, hjust = 1)),
#'              user_point_color = "magenta",
#'              user_errorbar_color = "darkred")
#'
#' # Sorted PRS plot
#' ordered_data <- CI_data[order(-CI_data$PRS),]
#' ordered_data$IID <- factor(ordered_data$IID, levels = ordered_data$IID)
#' flex_interval(ordered_data, interactive = FALSE, user_ref_visible = FALSE)

flex_interval <- function(CI_data,
                          user_ref_line = NULL,
                          user_ref_color = "red",
                          user_ref_linetype = "dashed",
                          user_ref_visible = TRUE,
                          user_point_color = "blue",
                          user_point_size = 3,
                          user_errorbar_color = "black",
                          user_errorbar_size = 0.2,
                          interactive = TRUE,
                          user_theme = theme_minimal(),
                          user_theme_specifications = theme(),  # user-defined theme() additions
                          user_title = "PRS with Confidence Intervals",
                          user_x_label = "Individual",
                          user_y_label = "Polygenic Risk Score (PRS)") {
  
  
  if (user_ref_visible) {
    if (is.null(user_ref_line)) {
      ref_line_vals <- mean(CI_data$PRS, na.rm = TRUE)
    } else {
      ref_line_vals <- user_ref_line
    }
  } else {
    ref_line_vals <- NULL
  }
  
  if (!is.null(ref_line_vals)) {
    n_lines <- length(ref_line_vals)
    if (length(user_ref_color) < n_lines) user_ref_color <- rep(user_ref_color, length.out = n_lines)
    if (length(user_ref_linetype) < n_lines) user_ref_linetype <- rep(user_ref_linetype, length.out = n_lines)
  }
  
  
  p <- ggplot(CI_data, aes(x = IID, y = PRS,
                        text = paste("IID:", IID,
                                     "<br>PRS:", round(PRS, 4),
                                     "<br>Variance:", round(Variance, 4),
                                     "<br>Lower Limit:", round(Lower_Limit, 4),
                                     "<br>Upper Limit:", round(Upper_Limit, 4)))) +
    geom_point(color = user_point_color, size = user_point_size) +
    geom_errorbar(aes(ymin = Lower_Limit, ymax = Upper_Limit),
                  color = user_errorbar_color, width = user_errorbar_size)
  
  if (!is.null(ref_line_vals)) {
    for (i in seq_along(ref_line_vals)) {
      p <- p + geom_hline(yintercept = ref_line_vals[i],
                          linetype = user_ref_linetype[i],
                          color = user_ref_color[i])
    }
  }
  
  p <- p +
    labs(title = user_title, x = user_x_label, y = user_y_label) +
    user_theme +
    user_theme_specifications
  
  if (interactive) {
    print(p)
    return(ggplotly(p, tooltip = "text"))
  } else {
    print(p)
  }
}
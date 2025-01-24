utils::globalVariables(c("Model", "R_squared", "p_value", "lower", "upper"))

#' flex_accuracy function
#'
#' Creates a bar plot of Model prediction accuracies with the option to display 
#' p-values and make the plot interactive using Plotly.
#'
#' @param accuracy_data A data frame containing four columns: 'Model' (factor or
#'  character vector of Model names), 'R_squared' (numeric vector of prediction 
#'  accuracies), 'p_value' (numeric vector of p-values) and 'se' (numeric vector
#'   of standard errors of prediction accuracies).
#' @param conf_level A numeric value between 0 and 1 to indicate level of 
#' confidence; default is 0.95.
#' @param display_CI Logical; if TRUE, returns confidence intervals (CIs).
#' @param interactive Logical; if TRUE, returns an interactive Plotly plot. If 
#' FALSE, returns a static ggplot object.
#' @param user_colors Optional; a vector of colors to use for the bars. If NULL,
#'  default ggplot2 colors are used.
#' @param display_p_values Logical; if TRUE, p-values are displayed on the plot.
#' @param Models_to_display_p_values Optional; a vector of Model names for which
#'  to display p-values. If NULL, p-values are displayed for all Models.
#' @param user_title Character string setting the title of the plot.
#' @param user_x_title Character string for the x-axis title.
#' @param user_y_title Character string for the y-axis title.
#' @param user_legend_title Optional; character string for the legend title. If 
#' NULL, the legend is hidden.
#' @param user_plot_theme ggplot2 theme object to use for the base styling of 
#' the plot.
#' @param user_plot_theme_specs ggplot2 theme object to apply additional 
#' styling.
#' @param user_p_value_prefix Optional; character string to prefix before 
#' p-values in their annotations.
#' @param user_bar_width Numeric value for the width of the bars in the plot.
#' @param geom_text_args List of additional arguments to pass to geom_text for 
#' p-value annotation customization.
#' @param additional_ggplot_args List of additional ggplot objects to add to the
#'  plot.
#'
#' @return A ggplot object if 'interactive' is FALSE; otherwise, an interactive 
#' Plotly plot.
#' @import reshape2 
#' @import utils
#' @import gridExtra
#' @import nortest
#' @import ggforce
#' @import cowplot
#' @import grid
#' @export
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
#' # Usage with default bar width
#' flex_accuracy(example_data3)
#' # Example to show only specific Models' p-values with thicker bars
#' flex_accuracy(example_data3, display_p_values = TRUE, Models_to_display_p_values = 
#' c("Model A", "Model E"), user_bar_width = 0.8)
#' # Optionally add p-value annotations
#' flex_accuracy(example_data3, user_p_value_prefix = "p = ", geom_text_args = 
#' list(data = example_data3, fontface = "bold", hjust = 0.5, color = "darkblue", 
#' size = 3))
#' #adding additional plotting features
#' flex_accuracy(example_data3, user_bar_width = 0.5, display_p_values = TRUE, 
#' Models_to_display_p_values = c("Model A", "Model E"),
#'              geom_text_args = list(data = subset(example_data3, 
#'              Model %in% c("Model A", "Model E")), angle = 60, 
#'              fontface = "bold", hjust = 0.5, color = "darkblue", size = 5), 
#'              additional_ggplot_args = list(ggplot2::scale_y_continuous(limits = c(0, 1.3))))
#' # Adjust the geom_text_args to nudge p-value annotations above the bars
#' flex_accuracy(example_data3,
#'              user_bar_width = 0.5,
#'              display_p_values = TRUE,
#'              Models_to_display_p_values = c("Model A", "Model E"),
#'              geom_text_args = list(angle = 60, fontface = "bold", hjust = 1, 
#'              color = "darkblue", size = 5,
#'                                    nudge_y = 0.2),  
#'              additional_ggplot_args = list(ggplot2::scale_y_continuous(limits = c(0, 1.5))))
flex_accuracy <- function(accuracy_data,
                          conf_level = 0.95,
                          display_CI = TRUE,
                          interactive = TRUE,
                          user_colors = NULL,
                          display_p_values = TRUE,
                          Models_to_display_p_values = NULL,
                          user_title = "Prediction Accuracy Plot",
                          user_x_title = "Model",
                          user_y_title = "Prediction Accuracy",
                          user_legend_title = NULL,
                          user_plot_theme = theme_minimal(),
                          user_plot_theme_specs = theme(
                            legend.title = element_text(size = 10),
                            legend.text = element_text(size = 15),
                            title = element_text(size = 15),
                            axis.text.x = element_text(size = 10),
                            axis.title.x = element_text(size = 10),
                            axis.text.y = element_text(size = 10),
                            axis.title.y = element_text(size = 10)
                          ),
                          user_p_value_prefix = NULL,
                          user_bar_width = 0.8,
                          geom_text_args = list(),
                          additional_ggplot_args = list()) {
  
  # Validate input data
  if (!all(c("Model", "R_squared", "p_value") %in% names(accuracy_data))) {
    stop("accuracy_data must include 'Model', 'R_squared', 'p_value' and 'se' columns")
  }
  
  # Validate 'interactive' is logical and length 1
  if (!is.logical(interactive) || length(interactive) != 1) {
    stop("'interactive' should be a single logical value (TRUE or FALSE)")
  }
  
  # Default color assignment
  if (is.null(user_colors)) {
    if (length(unique(accuracy_data$Model)) > length(scale_fill_discrete())) {
      stop("The number of Models exceeds the available default colors. Please provide user_colors.")
    }
    user_colors <- scale_fill_discrete()  # Use ggplot2 default discrete colors
  } else {
    user_colors <- scale_fill_manual(values = user_colors)
  }
  
  # Calculate confidence intervals
  z <- qnorm((1 + conf_level) / 2)  # Z value for the confidence level
  accuracy_data$lower <- accuracy_data$R_squared - z * accuracy_data$se
  accuracy_data$upper <- accuracy_data$R_squared + z * accuracy_data$se
  
  # Build the plot
  p <- suppressWarnings({ggplot(accuracy_data, aes(x = Model, y = R_squared, fill = Model, 
                                                   text = paste0(user_x_title, ": ", Model, 
                                                                 "<br>", user_y_title, ": ", R_squared, 
                                                                 "<br>p-value: ", format(round(p_value, 3), nsmall = 3))))}) +
       geom_bar(stat = "identity", width = user_bar_width, show.legend = !is.null(user_legend_title)) +
    
    user_colors +
    labs(title = user_title, x = user_x_title, y = user_y_title) +
    user_plot_theme +
    user_plot_theme_specs
  
  # Add confidence intervals if specified
  if (display_CI && "se" %in% names(accuracy_data)) {
    z <- qnorm((1 + conf_level) / 2)  # Z value for the confidence level
    accuracy_data$lower <- accuracy_data$R_squared - z * accuracy_data$se
    accuracy_data$upper <- accuracy_data$R_squared + z * accuracy_data$se
    p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
  }
  
  # Apply additional graphical arguments
  for (arg in additional_ggplot_args) {
    p <- p + arg
  }
  
  # Optionally add p-value annotations
  if (display_p_values) {
    p_value_data <- subset(accuracy_data, is.null(Models_to_display_p_values) | Model %in% Models_to_display_p_values)
    p_value_labels <- paste0(user_p_value_prefix, format(p_value_data$p_value, scientific = TRUE))
    geom_text_defaults <- list(aes(label = p_value_labels, y = R_squared + 0.2), 
                               data = p_value_data, 
                               vjust = 0, 
                               angle = 90,
                               fontface = "bold",
                               color = "black")
    p <- p + do.call(geom_text, modifyList(geom_text_defaults, geom_text_args))
  }
  
  # Add interactivity if specified
  if (interactive) {
    print(p)  # Explicitly print the ggplot object
    return(ggplotly(p, tooltip = "text"))  # Returns the interactive plot
  } else {
    p
  }
}
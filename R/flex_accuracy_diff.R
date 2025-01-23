utils::globalVariables(c("Compare1", "Compare2", "Model", "R_squared", "ypos", "R_squared_1", "R_squared_2", "p_value"))

#' flex_accuracy_diff function
#'
#' This function generates a customizable bar plot to compare prediction 
#' accuracy (e.g. R-squared values) between multiple models. 
#' It displays pairwise comparison lines and corresponding p-values. Interactive
#'  plots can be created using Plotly.
#'
#' @param accuracy_data A data frame containing the models' R-squared values 
#' with two columns: 'Model' and 'R_squared'
#' @param comparison_data A data frame containing pairwise comparisons with 
#' three columns: 'Compare1', 'Compare2' and 'p_value'
#' @param interactive Logical. If 'TRUE', returns an interactive plotly object; 
#' otherwise, returns a static ggplot object. Default is 'TRUE'.
#' @param user_colors Character vector of colors for the bars. If 'NULL', uses 
#' ggplot2 default colors. Default is 'NULL'.
#' @param user_segment_color Character. Color of comparison lines and dashed 
#' segments. Default is "darkblue"'.
#' @param display_p_values Logical. If 'TRUE', adds p-values to the plot for 
#' specified comparisons. Default is 'TRUE'.
#' @param models_to_display_p_values Character vector of model pairs for which 
#' p-values should be displayed, formatted as "Model1 Model2"'. Default is 
#' 'NULL'.
#' @param user_title Character. Title of the plot. Default is "Prediction 
#' Accuracy Comparison Plot"'.
#' @param user_x_title Character. Title of the x-axis. Default is "Model"'.
#' @param user_y_title Character. Title of the y-axis. Default is "Prediction 
#' Accuracy"'.
#' @param user_legend_title Character. Title of the legend. Default is 'NULL'.
#' @param user_plot_theme A ggplot2 theme to be applied to the plot. Default is 
#' 'theme_minimal()'.
#' @param user_plot_theme_specs Additional theme specifications applied after 
#' 'user_plot_theme'. Default is a set of predefined theme customizations.
#' @param user_p_value_prefix Character. Prefix to be added before displaying 
#' p-values. Default is 'NULL'.
#' @param user_bar_width Numeric. Width of the bars. Default is '0.8'.
#' @param geom_text_args List. Additional arguments for 'geom_text', such as 
#' color, font size, or angle. Default is an empty list.
#' @param additional_ggplot_args List. Additional ggplot2 layers or arguments 
#' to be added to the plot. Default is an empty list.
#'
#' @return A ggplot object if 'interactive' is FALSE; otherwise, an interactive 
#' Plotly plot.
#'
#' @import reshape2 
#' @import stats 
#' @import utils
#' @import gridExtra
#' @import nortest
#' @import ggforce
#' @import cowplot
#' @import grid
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
#' accuracy_data <- example_data4
#' comparison_data <- example_data5
#' # Create a static plot
#' flex_accuracy_diff(accuracy_data, comparison_data, interactive = FALSE)
#'
#' # Create an interactive plot
#' flex_accuracy_diff(accuracy_data, comparison_data)
#'
#' # Display specific p-values
#' flex_accuracy_diff(accuracy_data, comparison_data, 
#'   models_to_display_p_values = c("Model A Model B"))
flex_accuracy_diff <- function(accuracy_data, comparison_data,
                                      interactive = TRUE,
                                      user_colors = NULL,
                                      user_segment_color = "darkblue",
                                      display_p_values = TRUE,
                                      models_to_display_p_values = NULL,
                                      user_title = "Prediction Accuracy Comparison Plot",
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
  # Ensure correct factor levels based on accuracy_data Models
  accuracy_data$Model <- factor(accuracy_data$Model, levels = unique(accuracy_data$Model))
  comparison_data$Compare1 <- factor(comparison_data$Compare1, levels = levels(accuracy_data$Model))
  comparison_data$Compare2 <- factor(comparison_data$Compare2, levels = levels(accuracy_data$Model))
  
  # Filter comparisons based on user input
  if (!is.null(models_to_display_p_values)) {
    comparison_data <- subset(comparison_data, paste(Compare1, Compare2) %in% models_to_display_p_values |
                                paste(Compare2, Compare1) %in% models_to_display_p_values)
  }
  
  # Calculate dynamic y positions for comparison lines
  if (nrow(comparison_data) > 0) {
    comparison_data <- merge(comparison_data, accuracy_data, by.x = "Compare1", by.y = "Model")
    comparison_data <- merge(comparison_data, accuracy_data, by.x = "Compare2", by.y = "Model", suffixes = c("_1", "_2"))
    comparison_data$ypos_base <- apply(comparison_data[, c("R_squared_1", "R_squared_2")], 1, max)
    comparison_data <- comparison_data[order(comparison_data$ypos_base), ]
    comparison_data$ypos <- comparison_data$ypos_base + seq(0.2, by = 0.05, length.out = nrow(comparison_data))
  }
  
  # Default color assignment
  if (is.null(user_colors)) {
    if (length(unique(accuracy_data$Model)) > length(scale_fill_discrete())) {
      stop("The number of models exceeds the available default colors. Please provide user_colors.")
    }
    user_colors <- scale_fill_discrete()  # Use ggplot2 default discrete colors
  } else {
    user_colors <- scale_fill_manual(values = user_colors)
  }
  
  # Create the plot
  p <- suppressWarnings({ggplot(accuracy_data, aes(x = Model, y = R_squared, fill = Model, 
                                                   text = paste0(user_x_title, ": ", Model, 
                                                                 "<br>", user_y_title, ": ", R_squared)))}) +
    geom_bar(stat = "identity", width = user_bar_width, show.legend = !is.null(user_legend_title)) +
    user_colors + 
    labs(title = user_title,
         y = user_y_title,
         x = user_x_title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = user_title, x = user_x_title, y = user_y_title) +
    user_plot_theme +
    user_plot_theme_specs
  
  # Add comparison lines and p-values if specified
  if (display_p_values) {
    if (nrow(comparison_data) > 0) {
      p <- p +
        geom_segment(data = comparison_data, aes(x = as.numeric(Compare1), xend = as.numeric(Compare2),
                                                 y = ypos, yend = ypos), inherit.aes = FALSE, color = user_segment_color, size = 0.8) +
        geom_segment(data = comparison_data, aes(x = as.numeric(Compare1), xend = as.numeric(Compare1),
                                                 y = R_squared_1, yend = ypos), inherit.aes = FALSE, color = user_segment_color, linetype = "dashed") +
        geom_segment(data = comparison_data, aes(x = as.numeric(Compare2), xend = as.numeric(Compare2),
                                                 y = R_squared_2, yend = ypos), inherit.aes = FALSE, color = user_segment_color, linetype = "dashed") +
        do.call(geom_text, c(list(data = comparison_data, 
                                  aes(x = (as.numeric(Compare1) + as.numeric(Compare2)) / 2,
                                      y = ypos + 0.02, 
                                      label = paste0(user_p_value_prefix, format(p_value, scientific = TRUE))),
                                  inherit.aes = FALSE), geom_text_args))
    }
  }
  
  # Apply additional graphical arguments
  for (arg in additional_ggplot_args) {
    p <- p + arg
  }
  
  # Add interactivity if specified
  if (interactive) {
    print(p)  # Explicitly print the ggplot object
    return(plotly::ggplotly(p, tooltip = "text"))  # Returns the interactive plot
  } else {
    p
  }
}
utils::globalVariables(c("trait", "R2diff", "lower_limit_R2", "upper_limit_R2", "ymax", "p_value", "R2", "method", "ymin"))

#' flex_accuracy_diff2 function
#'
#' This function generates detailed interactive or static visualizations 
#' comparing the accuracy of two methods across different traits. It uses 
#' ggplot2 and plotly for rendering the plots.
#' 
#' @param accuracy_diff_data A data frame containing nine columns and at least 
#' two rows: `trait` (factor or character vector of trait names), `method` 
#' (factor or character vector of method names (two methods)), `R2` (numeric 
#' vector of accuracy of the methods for a trait), `lower_limit_R2` (numeric 
#' vector of lower limits of accuracy), `upper_limit_R2` (numeric vector of 
#' upper limits of accuracy), `difference_R2` (numeric vector of differences 
#' between the methods' accuracies), `lower_limit_difference_R2` (numeric vector
#'  of lower limits of differences in accuracies), `upper_limit_difference_R2` 
#'  (numeric vector of upper limits of differences in accuracies), and 
#'  `p_value_difference_R2` (numeric vector of p-values for differences in 
#'  accuracies).
#' @param user_colors A character vector of length 2 specifying colors for 
#' the two methods in the bar plot.
#' @param interactive Logical, if `TRUE` the function returns an interactive 
#' plotly plot, otherwise it returns a ggplot object.
#' @param user_geom_bar A ggplot2 geom_bar object for customizing the bar plot 
#' appearance. Accepts parameters like stat, position, and width to control the 
#' visual properties of the bars.
#' @param user_geom_point A ggplot2 geom_point object for customizing the point 
#' plot appearance. This includes settings for color and size of points which 
#' represent the differences in accuracy.
#' @param user_accuracy_geom_errorbar A ggplot2 geom_errorbar object for 
#' customizing the error bars in the accuracy plot. This involves setting 
#' parameters like color, width, and position to visually modify how error 
#' bars are displayed.
#' @param user_accuracy_diff_geom_errorbar A ggplot2 geom_errorbar object for 
#' customizing the error bars in the accuracy difference plot. Similar to 
#' user_accuracy_geom_errorbar, but typically used to emphasize differences 
#' between methods.
#' @param user_geom_text A ggplot2 geom_text object for adding text annotations 
#' to the plots. This can include parameters for positioning, size, and the 
#' label content, often used to display statistical significance or other 
#' annotations.
#' @param user_ylim_accuracy A ggplot2 ylim function call for setting the y-axis
#'  limits in the accuracy plot. This helps in controlling the scale of the plot
#'   to better fit the data presentation.
#' @param user_ylim_accuracy_difference A ggplot2 ylim function call for setting
#'  the y-axis limits in the accuracy difference plot. Useful for maintaining 
#'  consistent visual scales across related plots.
#' @param user_accuracy_labs A ggplot2 labs function call for setting labels and
#'  titles in the accuracy plot. This includes parameters to set the x-axis 
#'  label, y-axis label, and the main title of the plot.
#' @param user_accuracy_diff_labs A ggplot2 labs function call for setting 
#' labels and titles in the accuracy difference plot. Useful for distinguishing 
#' between different plots and providing clear, informative titles and labels.
#' @param user_accuracy_theme A ggplot2 theme object for applying styling themes
#'  to the accuracy plot. This parameter can be used to apply a predefined theme
#'   or customize aspects like text, background, and grid lines.
#' @param user_accuracy_diff_theme A ggplot2 theme object for applying styling 
#' themes to the accuracy difference plot. Allows for consistent or contrasting 
#' styles between different types of visualizations in the package.
#' @param user_accuracy_theme_specs Additional ggplot2 theme modifications 
#' specifically for the accuracy plot. This can involve finer control over 
#' elements like legend position and plot margins.
#' @param user_accuracy_diff_theme_specs Additional ggplot2 theme modifications 
#' specifically for the accuracy difference plot. Tailored to enhance or modify 
#' specific aspects of the plot's appearance beyond the base theme settings.

#' @return Either a ggplot or plotly object depending on the `interactive` 
#' argument.
#' @import reshape2 
#' @import stats 
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
#' flex_accuracy_diff2(example_data6)
flex_accuracy_diff2 <- function(accuracy_diff_data,
                                user_colors = c("lightblue", "lightpink"),
                                interactive = TRUE,
                                user_geom_bar = geom_bar(stat = "identity", position = position_dodge(0.5), width = 0.4),
                                user_geom_point = geom_point(aes(x = trait, y = R2diff), color = "black", size = 2),
                                user_accuracy_geom_errorbar = geom_errorbar(aes(ymin = lower_limit_R2, ymax = upper_limit_R2), color = "black", width = 0.1, position = position_dodge(0.5)),
                                user_accuracy_diff_geom_errorbar = geom_errorbar(width = 0.05, color = "black"),
                                user_geom_text = geom_text(aes(x = trait, y = ymax+0.1, label = p_value), hjust = 0.5, size = 3.5),
                                user_ylim_accuracy = ylim(0,1),
                                user_ylim_accuracy_difference = ylim(0,1),
                                user_accuracy_labs = labs(x = "", y = "R2", title = "R2 values"),
                                user_accuracy_diff_labs = labs(x = "", y = "Difference", title = "Difference between R2"),
                                user_accuracy_theme = theme_minimal(),
                                user_accuracy_diff_theme = theme_minimal(),
                                user_accuracy_theme_specs = theme(legend.position = c(0.85,0.85)),
                                user_accuracy_diff_theme_specs = theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = unit(c(1, 1, 1, 1), "lines"))) {
  # Check if the color vector has exactly three elements
  if (length(user_colors) != 2) {
    stop("Please provide exactly two user_colors.")
  }
  
  # Create a plot for R^2 values
  p1 <- ggplot(accuracy_diff_data, aes(x = trait, y = R2, fill = method)) +
    user_geom_bar +
    scale_fill_manual(values = user_colors) +
    user_accuracy_geom_errorbar +
    user_accuracy_theme +
    user_accuracy_theme_specs + 
    user_accuracy_labs +
    user_ylim_accuracy 
  
  # Create a data frame for the difference plots
  diff_data <- data.frame(trait = accuracy_diff_data$trait, x = 1, ymin = accuracy_diff_data$lower_limit_difference_R2, ymax = accuracy_diff_data$upper_limit_difference_R2, R2diff = accuracy_diff_data$difference_R2, p_value = accuracy_diff_data$p_value_difference_R2)
  
  # Plot for Difference of R^2
  p2 <- ggplot(diff_data, aes(x = trait, ymin = ymin, ymax = ymax)) +
    user_geom_point + 
    user_accuracy_diff_geom_errorbar +  # Adjusted error bar width here
    user_geom_text +
    user_accuracy_diff_theme +
    user_accuracy_diff_theme_specs +
    user_accuracy_diff_labs +
    user_ylim_accuracy_difference
  
  # Combine plots using grid.arrange
  final_plot <- grid.arrange(p2, p1, ncol = 1, heights = c(1, 3))
  
  # Add interactivity if specified
  if (interactive) {
    print(final_plot)  # Print the ggplot object
    plotly_p1 <- plotly::ggplotly(p1 + labs(title = NULL))
    plotly_p2 <- plotly::ggplotly(p2 + labs(title = NULL))
    
    combined_plot <- subplot(plotly_p2, plotly_p1, nrows = 2, heights = c(0.25, 0.75), shareX = TRUE, titleX = TRUE, titleY = TRUE)
    combined_plot <- layout(combined_plot, 
                            annotations = list(
                              list(text = user_accuracy_diff_labs$title, x = 0.01, y = 1, xref = 'paper', yref = 'paper', showarrow = FALSE),
                              list(text = user_accuracy_labs$title, x = 0.01, y = 0.75, xref = 'paper', yref = 'paper', showarrow = FALSE)
                            ))
    return(combined_plot)
  } else {
    return(final_plot)  # Return the ggplot object
  }
}
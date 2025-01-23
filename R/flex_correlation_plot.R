#' flex_correlation_plot function
#'
#' This function creates a correlation heatmap using ggplot2 and plotly, allowing 
#' for various customizations. It supports rendering both static and interactive
#'  correlation heatmaps of either the full, lower, or upper triangular matrix.
#' 
#' @import reshape2 
#' @import stats 
#' @import utils
#' @import gridExtra
#' @import nortest
#' @import ggforce
#' @import cowplot
#' @import grid
#' 
#' @export
#' 
#' @param correlation_matrix A square matrix representing correlation coefficients 
#' with column names identical to row names.
#' @param user_colors A vector of three colors representing the gradient of the 
#' heatmap for negative, neutral, and positive correlations respectively. 
#' Default is c("darkred", "white", "steelblue4").
#' @param display_names Logical, whether to display variable names on the axes. 
#' Default is TRUE.
#' @param interactive Logical, indicating if the output should be an interactive
#'  plotly object. Default is TRUE.
#' @param user_lower_limit The minimum value of the color gradient. Default is -1.
#' @param user_upper_limit The maximum value of the color gradient. Default is 1.
#' @param user_mid_point The midpoint value of the color gradient where the neutral 
#' color is centered. Default is 0.
#' @param user_plotly_x_name The name to be used for the x-axis in the plotly plot. 
#' Default is "VAR_A".
#' @param user_plotly_y_name The name to be used for the y-axis in the plotly plot. 
#' Default is "VAR_B".
#' @param user_plotly_value_name The name to be used for the values in the plotly 
#' tooltip. Default is "r".
#' @param user_title Title of the heatmap. Default is "Correlation Plot".
#' @param user_x_title Custom x-axis title. If NULL, defaults to column names of
#'  the matrix.
#' @param user_y_title Custom y-axis title. If NULL, defaults to row names of the
#'  matrix.
#' @param user_legend_title Title for the legend. Default is "Correlation".
#' @param matrix_type Specifies whether to plot the full matrix, the lower triangular
#'  part, or the upper triangular part. Default options are "full", "lower", "upper".
#' @param user_plot_theme ggplot2 theme object for base theming of the plot. Default
#'  is theme_minimal().
#' @param user_plot_theme_specs Additional ggplot2 theme specifications to apply 
#' on top of `user_plot_theme`.
#' @param user_zoom_range Optional numeric vector specifying the indices of the 
#' matrix to zoom into; this disables interactivity.
#'
#' @return A ggplot object if `interactive = FALSE`, otherwise a plotly interactive plot.
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
#' # Generate a symmetric correlation matrix
#' correlation_matrix <- example_data9
#' # Default full matrix heatmap
#' flex_correlation_plot(correlation_matrix)
#'
#' # Lower triangular heatmap
#' flex_correlation_plot(correlation_matrix, matrix_type = "lower")
#'
#' # Customized plot with different theme and colors
#' flex_correlation_plot(
#'   correlation_matrix = correlation_matrix,
#'   user_colors = c("gold2", "lightgrey", "darkblue"),
#'   user_title = "Custom Correlation Plot",
#'   user_x_title = "Variables",
#'   user_y_title = "Variables",
#'   user_legend_title = "Correlation Coefficient",
#'   user_plot_theme = theme_classic(),
#'   user_plot_theme_specs = theme(
#'     legend.title = element_text(size = 12),
#'     legend.text = element_text(size = 10),
#'     title = element_text(size = 16),
#'     axis.title.x = element_text(size = 12),
#'     axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
#'     axis.text.y = element_text(size = 12),
#'     axis.title.y = element_text(size = 12),
#'     legend.position = "bottom")
#' )
flex_correlation_plot <- function(correlation_matrix, 
                                   user_colors = c("darkred", "white", "steelblue4"), 
                                   display_names = TRUE,
                                   interactive = TRUE,
                                   user_lower_limit = -1,
                                   user_upper_limit = 1,
                                   user_mid_point = 0,
                                   user_plotly_x_name = "VAR_A",
                                   user_plotly_y_name = "VAR_B",
                                   user_plotly_value_name = "r",
                                   user_title = "Correlation Plot",
                                   user_x_title = NULL,
                                   user_y_title = NULL,
                                   user_legend_title = "Correlation",
                                   matrix_type = "full",
                                   user_plot_theme = theme_minimal(),
                                   user_plot_theme_specs = theme(
                                     legend.title = element_text(size = 10),
                                     legend.text = element_text(size = 10),
                                     title = element_text(size = 15),
                                     axis.text.x = element_text(size = 10),
                                     axis.title.x = element_text(size = 10),
                                     axis.text.y = element_text(size = 10),
                                     axis.title.y = element_text(size = 10)), 
                                   user_zoom_range = NULL) {
  # Check if the color vector has exactly three elements
  if (length(user_colors) != 3) {
    stop("Please provide exactly three user_colors.")
  }
  
  # Validate matrix_type
  if (!matrix_type %in% c("full", "lower", "upper")) {
    stop("matrix_type must be one of 'full', 'lower', or 'upper'.")
  }
  
  # Subset the matrix based on the matrix_type
  if (matrix_type == "lower") {
    correlation_matrix[upper.tri(correlation_matrix)] <- NA
  } else if (matrix_type == "upper") {
    correlation_matrix[lower.tri(correlation_matrix)] <- NA
  }
  
  # Melt the matrix for ggplot
  correlation_melt <- reshape2::melt(as.matrix(correlation_matrix), na.rm = TRUE) # Remove NA values from melted data
  
  # Assign column names
  names(correlation_melt) <- c(user_plotly_x_name, user_plotly_y_name, user_plotly_value_name)
  correlation_melt[[user_plotly_value_name]] <- as.numeric(correlation_melt[[user_plotly_value_name]])  # Ensure it's numeric
  
  var_names <- rownames(correlation_matrix) 
  
  # Create the base plot
  p <- ggplot(correlation_melt, aes(x = .data[[user_plotly_x_name]], y = .data[[user_plotly_y_name]], fill = .data[[user_plotly_value_name]])) +
    geom_tile() +
    scale_fill_gradient2(low = user_colors[1], mid = user_colors[2], high = user_colors[3], midpoint = user_mid_point, limits = c(user_lower_limit, user_upper_limit)) +
    labs(title = user_title, x = user_x_title, y = user_y_title, fill = user_legend_title) +
    scale_x_discrete(labels = colnames(correlation_matrix)) +
    scale_y_discrete(labels = rownames(correlation_matrix)) +
    user_plot_theme +
    user_plot_theme_specs
  
  # Customize axis positioning based on matrix_type
  if (matrix_type == "lower" && display_names) {
    p <- p +
      scale_x_discrete(labels = rownames(correlation_matrix)) +
      scale_y_discrete(labels = rownames(correlation_matrix), position = "right") + # Position y-axis to the right
      theme(axis.text.y.right = element_text(size = 10),
            axis.title.y.right = element_text(size = 12))
  } else if (matrix_type == "upper" && display_names) {
    p <- p +
      scale_x_discrete(labels = colnames(correlation_matrix), position = "top") + # Position x-axis on top
      scale_y_discrete(labels = rownames(correlation_matrix)) +
      theme(axis.text.x.top = element_text(size = 10, angle = 45, hjust = 0, vjust = -0.1),
            axis.title.x.top = element_text(size = 12))
  }
  
  if (!is.null(user_zoom_range)) {
    interactive = FALSE
    zoomed_cor <- correlation_melt[correlation_melt[[user_plotly_x_name]] %in% var_names[user_zoom_range] & 
                                     correlation_melt[[user_plotly_y_name]] %in% var_names[user_zoom_range], ]
    p_zoom <- ggplot(zoomed_cor, aes(x = .data[[user_plotly_x_name]], y = .data[[user_plotly_y_name]], fill = .data[[user_plotly_value_name]])) +
      geom_tile(color = "black", size = 0.3) +
      scale_fill_gradient2(low = user_colors[1], mid = user_colors[2], high = user_colors[3],
                           midpoint = user_mid_point, limits = c(user_lower_limit, user_upper_limit)) +
      labs(title = "") +
      scale_x_discrete(labels = var_names[user_zoom_range]) +
      scale_y_discrete(labels = var_names[user_zoom_range]) +
      user_plot_theme +
      theme(legend.position = "none", 
            axis.text.x = element_text(angle = 45, hjust = 1), 
            axis.text.y = element_text(hjust = 1),
            axis.title = element_blank()) 
    
    p <- gridExtra::grid.arrange(p, p_zoom, ncol = 2)
  }
  
  # Add interactivity if specified
  if (interactive) {
    print(p)  # Print the ggplot object
    return(plotly::ggplotly(p))  # Return the interactive plot
  } else {
    return(p)  # Return the ggplot object
  }
}
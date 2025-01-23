utils::globalVariables(c("Variable", "Value", "lower", "upper", "ymin", "middle", "ymax", "lower_fence", "upper_fence"))

#' flex_boxplot function
#'
#' This function creates a flexible, optionally interactive boxplot which can
#' be customized with different themes, colors, and additional annotations. It
#' supports both traditional and interactive (Plotly) outputs.
#'
#' @param distribution_data A dataframe where each column represents a data 
#' distribution (e.g. population) to be plotted with minimum of a single column.
#' @param interactive Logical; if TRUE, the output is an interactive Plotly
#'   graph. Defaults to TRUE.
#' @param user_colors A vector of colors to be used for the boxplots. If NULL,
#'   a default set of colors is used.
#' @param user_title The main title of the plot. Defaults to "Boxplot".
#' @param user_x_title Custom title for the x-axis. If NULL, defaults to the
#'   column names of distribution_data.
#' @param user_y_title Custom title for the y-axis. If NULL, defaults to "Values".
#' @param user_legend_title Title for the legend. Use NA to hide the legend.
#'   Defaults to NA.
#' @param user_plot_theme A ggplot2 theme object to customize the appearance of
#'   the plot. Defaults to theme_minimal().
#' @param user_plot_theme_specs Additional ggplot2 theme specifications.
#' @param annotate_stats Logical; if TRUE, adds text annotations for basic
#'   statistics (min, Q1, median, Q3, max) to the plot.
#' @param annotate_outliers Logical; if TRUE, adds text annotations for outliers
#'   to the plot.
#' @param annotate_stats_text_size Numeric; text size for statistics annotations.
#'   Defaults to 3.5.
#' @param annotate_outliers_text_size Numeric; text size for outliers annotations.
#'   Defaults to 3.
#' @param annotate_stats_text_color Character; text color for statistics annotations.
#'   Defaults to "black".
#' @param annotate_outliers_text_color Character; text color for outliers annotations.
#'   Defaults to "darkred".
#' @param annotate_outliers_text_vjust Numeric; vertical adjustment for outliers
#'   text annotations.
#' @param annotate_stats_text_vjust Numeric; vertical adjustment for statistics
#'   text annotations.
#' @param annotate_outliers_text_hjust Numeric; horizontal adjustment for outliers
#'   text annotations.
#' @param annotate_stats_text_hjust Numeric; horizontal adjustment for statistics
#'   text annotations.
#' 
#' @return Depending on the value of \code{interactive}, returns either a Plotly
#'   object and/or a ggplot object.
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
#'@export
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
#' # Basic interactive boxplot
#' flex_boxplot(example_data8)
#' 
#' # Boxplot with custom colors, themes and titles
#'flex_boxplot(example_data8, user_plot_theme = ggplot2::theme_gray(), 
#'             user_colors = c("#6A3D9A", "#FF7F00", "gold1"), 
#'             user_title = "Customized Boxplot",
#'           user_x_title = "Multiple Populations",
#'           user_y_title = "Values",
#'           user_legend_title = "Multiple Populations",
#'           user_plot_theme_specs = ggplot2::theme(
#'             legend.title = ggplot2::element_text(size = 10),
#'             legend.text = ggplot2::element_text(size = 10),
#'             title = ggplot2::element_text(size = 20),
#'             axis.text.x = ggplot2::element_blank(),
#'             axis.title.x = ggplot2::element_text(size = 15),
#'             axis.text.y = ggplot2::element_text(size = 12),
#'             axis.title.y = ggplot2::element_text(size = 15)
#'           ))
flex_boxplot <- function(
    distribution_data, 
    interactive = TRUE,
    user_colors = NULL, 
    user_title = "Boxplot",
    user_x_title = NULL,
    user_y_title = NULL,
    user_legend_title = NA,
    user_plot_theme = theme_minimal(),
    user_plot_theme_specs = theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      title = element_text(size = 15),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 10)
    ),
    annotate_stats = FALSE,
    annotate_outliers = FALSE,
    annotate_stats_text_size = 3.5, 
    annotate_outliers_text_size = 3, 
    annotate_stats_text_color = "black",
    annotate_outliers_text_color = "darkred",
    annotate_outliers_text_vjust = -1, 
    annotate_stats_text_vjust = 1.5,
    annotate_outliers_text_hjust = -0.5, 
    annotate_stats_text_hjust = 1
) {
  # Validate input
  if (!is.data.frame(distribution_data)) {
    stop("Input must be a dataframe")
  }
  
  # Default color palette
  default_colors <- c(
    "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1",
    "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6", "#FDBF6F", "gray70", "khaki2"
  )
  
  # Use user-defined colors if provided, otherwise default
  colors <- if (!is.null(user_colors)) user_colors else default_colors[1:min(length(default_colors), ncol(distribution_data))]
  
  # Melt data to long format, explicitly specifying no id variables
  data_long <- melt(distribution_data, id.vars = NULL) # Set id.vars to NULL explicitly
  colnames(data_long) <- c("Variable", "Value")
  
  # Create the boxplot
  p <- ggplot(data_long, aes(x = Variable, y = Value, fill = Variable)) +
    geom_boxplot() +
    scale_fill_manual(values = colors) +
    labs(
      title = user_title,
      x = user_x_title,
      y = user_y_title,
      fill = user_legend_title
    ) +
    user_plot_theme +
    user_plot_theme_specs
  
  if (annotate_stats || annotate_outliers) {
    interactive <- FALSE
    stats <- data_long %>%
      group_by(Variable) %>%
      summarise(
        ymin = min(Value),
        lower = quantile(Value, 0.25),
        middle = median(Value),
        upper = quantile(Value, 0.75),
        ymax = max(Value),
        lower_fence = lower - 1.5 * IQR(Value),
        upper_fence = upper + 1.5 * IQR(Value)
      )
    
    if (annotate_stats) {
      p <- p + geom_text(data = stats, aes(x = Variable, y = ymin, label = paste("Min:", round(ymin, 4))), color = annotate_stats_text_color, size = annotate_stats_text_size, vjust = annotate_stats_text_vjust, hjust = annotate_stats_text_hjust) +
        geom_text(data = stats, aes(x = Variable, y = lower, label = paste("Q1:", round(lower, 4))), color = annotate_stats_text_color, size = annotate_stats_text_size, vjust = annotate_stats_text_vjust, hjust = annotate_stats_text_hjust) +
        geom_text(data = stats, aes(x = Variable, y = middle, label = paste("Median:", round(middle, 4))), color = annotate_stats_text_color, size = annotate_stats_text_size, vjust = annotate_stats_text_vjust, hjust = annotate_stats_text_hjust) +
        geom_text(data = stats, aes(x = Variable, y = upper, label = paste("Q3:", round(upper, 4))), color = annotate_stats_text_color, size = annotate_stats_text_size, vjust = annotate_stats_text_vjust, hjust = annotate_stats_text_hjust) +
        geom_text(data = stats, aes(x = Variable, y = ymax, label = paste("Max:", round(ymax, 4))), color = annotate_stats_text_color, size = annotate_stats_text_size, vjust = annotate_stats_text_vjust, hjust = annotate_stats_text_hjust)
    }
    
    if (annotate_outliers) {
      outliers <- data_long %>%
        left_join(stats, by = "Variable") %>%
        filter(Value < lower_fence | Value > upper_fence)
      
      p <- p +
        geom_point(data = outliers, aes(x = Variable, y = Value), shape = 19, size = 3) +
        geom_text(data = outliers, aes(x = Variable, y = Value, label = round(Value, 4)), hjust = annotate_outliers_text_hjust, vjust = annotate_outliers_text_vjust, color = annotate_outliers_text_color, size = annotate_outliers_text_size)
    }
  }
  
  # Adjust legend display
  if (is.na(user_legend_title)) {
    p <- p + theme(legend.position = "none")
  }
  
  # Add interactivity if specified
  if (interactive) {
    print(p)
    return(plotly::ggplotly(p))
  } else {
    p
  }
}
utils::globalVariables(c("value", "..count..", "distribution", "..density..", "Mean", "Median"))

#' flex_distribution function
#'
#' The `flex_distribution` function provides a flexible way to visualize data 
#' distributions using histograms and density plots. It can be used to create 
#' both basic and interactive plots with additional features such as density 
#' curves, reference lines, and summary statistics annotations. The function 
#' supports custom colors and various themes.
#'
#' @param distribution_data A dataframe where each column represents a data 
#' distribution (e.g. population) to be plotted with minimum of a single column.
#' @param interactive Logical, if TRUE (default), the plot will be interactive 
#' using Plotly, otherwise a ggplot object is returned.
#' @param user_colors A vector of colors for the plots. If NULL, a set of default
#'  colors is used.
#' @param plot_type The type of plot to create, with options "histogram" and/or 
#' "density".
#' @param add_density Logical, if TRUE, adds a density curve to the plot.
#' @param reference_line Specifies the type of reference line to add; options 
#' include "mean", "median", or FALSE for no line.
#' @param show_summary Logical, if TRUE, displays summary statistics on the plot.
#' @param user_title Title of the plot.
#' @param user_x_title Custom X-axis title. If NULL, the name of the variable is
#'  used.
#' @param user_y_title Custom Y-axis title, default is "Frequency".
#' @param user_legend_title Title for the legend. Can be NA to exclude the legend title.
#' @param user_plot_theme ggplot2 theme object for customizing the appearance of the plot.
#' @param user_plot_theme_specs Further ggplot2 theme specifications.
#' @param binwidth The width of the bins for the histogram (optional).
#' @param bins The number of bins for the histogram (optional).
#'
#' @details The function is designed to be flexible and allows extensive customization 
#' of plot aesthetics and functionality. Density plots are added as an additional 
#' layer when \code{add_density} is TRUE, and the reference line can be specified by the user.
#'
#' @return An object of class 'ggplot' or 'plotly' depending on the 'interactive' parameter.
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
#' # Histogram with custom colors
#' custom_colors <- c("red", "blue", "green")
#' flex_distribution(example_data8, user_colors = custom_colors)
#'
#' # Density plot
#' flex_distribution(
#'   example_data8, 
#'   plot_type = "density", 
#'   user_colors = custom_colors,
#'   user_y_title = "Density"
#' )
#' 
#' #' # Basic histogram with density curves and summary statistics
#' flex_distribution(
#'   example_data8, 
#'   add_density = TRUE, 
#'   show_summary = TRUE,
#'   user_title = "Distribution with Densities and Summary",
#'   user_y_title = "Density"
#' )
#'
#' # Histogram with custom binwidth
#' flex_distribution(
#'   example_data8, 
#'   binwidth = 0.5,
#'   user_title = "Distribution with Custom Binwidth"
#' )
#'
#' # Histogram with a specified number of bins
#' flex_distribution(
#'   example_data8, 
#'   bins = 50,
#'   user_title = "Distribution with Custom Bin Count"
#' )
#'
#' @import reshape2 
#' @import utils
#' @import gridExtra
#' @import nortest
#' @import ggforce
#' @import cowplot
#' @import grid
#' @export
flex_distribution <- function(
    distribution_data, 
    interactive = TRUE,
    user_colors = NULL, 
    plot_type = "histogram", 
    add_density = FALSE, 
    reference_line = "mean", 
    show_summary = FALSE, 
    user_title = "Distribution Plot",
    user_x_title = NULL,
    user_y_title = "Frequency",
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
    binwidth = NULL,
    bins = NULL
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
  colors <- if (!is.null(user_colors)) {
    user_colors
  } else {
    default_colors
  }
  
  # Limit colors to the number of columns
  unique_vars <- length(distribution_data)
  if (length(colors) < unique_vars) {
    stop("Not enough colors provided for the number of unique columns.")
  }
  
  # Melt data to long format with explicit column specification
  data_long <- reshape2::melt(distribution_data, measure.vars = names(distribution_data))
  colnames(data_long) <- c("distribution", "value")
  
  # Base plot with explicit naming
  p <- ggplot(data_long, aes(x = value, y = ..count.., fill = distribution)) +
    user_plot_theme +
    user_plot_theme_specs +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(
      title = user_title, 
      x = user_x_title, 
      y = user_y_title
    )
  
  # Plot type selection
  if (plot_type == "histogram") {
    # Histogram with flexible bin control
    hist_params <- list(
      position = "identity", 
      alpha = 0.6
    )
    
    # Prioritize binwidth if provided
    if (!is.null(binwidth)) {
      hist_params$binwidth <- binwidth
    } 
    # Use bins if provided
    else if (!is.null(bins)) {
      hist_params$bins <- bins
    }
    # Use default calculation if neither is provided
    else {
      # Sturges' rule for default bin calculation
      hist_params$bins <- max(30, round(1 + 3.322 * log10(nrow(distribution_data))))
    }
    
    # Add histogram with calculated or user-specified bins
    p <- p + do.call(geom_histogram, hist_params)
  }
  
  # Density curve option
  if (add_density) {
    p <- ggplot(data_long, aes(x = value, y = ..density..)) +
      user_plot_theme +
      user_plot_theme_specs +
      labs(
        title = user_title, 
        x = user_x_title, 
        y = user_y_title
      )
    
    # Plot type selection
    if (plot_type == "histogram") {
      # Histogram with flexible bin control
      hist_params <- list(
        position = "identity", 
        alpha = 0.4,
        aes(fill = distribution)
      )
      
      # Prioritize binwidth if provided
      if (!is.null(binwidth)) {
        hist_params$binwidth <- binwidth
      } 
      # Use bins if provided
      else if (!is.null(bins)) {
        hist_params$bins <- bins
      }
      # Use default calculation if neither is provided
      else {
        # Sturges' rule for default bin calculation
        hist_params$bins <- max(30, round(1 + 3.322 * log10(nrow(distribution_data))))
      }
      
      # Add histogram with calculated or user-specified bins
      p <- p + do.call(geom_histogram, hist_params)
    }
    p <- p +
      geom_density(
        aes(color = distribution), 
        alpha = 1, 
        lwd = 1,
        position = "identity",
        show.legend = FALSE
      ) +     
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
  }
  
  if (plot_type == "density") {
    p <- ggplot(data_long, aes(x = value, y = ..density.., fill = distribution)) +
      user_plot_theme +
      user_plot_theme_specs +
      labs(
        title = user_title, 
        x = user_x_title, 
        y = user_y_title
      ) +
      geom_density(
        aes(color = distribution), 
        alpha = 0.5, 
        position = "identity")+
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
  }
  
  # Reference line
  summary_stats <- data_long %>%
    group_by(distribution) %>%
    summarise(
      Mean = mean(value, na.rm = TRUE),
      Median = median(value, na.rm = TRUE),
      `St_Dev` = sd(value, na.rm = TRUE),
      `Shapiro_p` = shapiro.test(value)$p.value
    )
  
  # Add reference line based on user specification
  if (reference_line == "mean") {
    p <- p + 
      geom_vline(
        data = summary_stats, 
        aes(xintercept = Mean, color = distribution),
        linetype = "dashed",
        size = 0.6,
        show.legend = FALSE)
  } else if (reference_line == "median") {
    p <- p + 
      geom_vline(
        data = summary_stats, 
        aes(xintercept = Median, color = distribution),
        linetype = "dashed",
        size = 0.6,
        show.legend = FALSE)
  } else{
    p <- p
  }
  
  # Add summary statistics
  if (show_summary) {
    # Create a nicely formatted summary table
    summary_text <- capture.output(
      print(
        summary_stats %>%
          mutate(across(where(is.numeric), ~round(., 4))) %>%
          as.data.frame(),
        row.names = FALSE
      )
    )
    
    p <- p + 
      annotate(
        "text", 
        x = Inf, 
        y = Inf, 
        label = paste(summary_text, collapse = "\n"), 
        hjust = 1, 
        vjust = 1, 
        family = "mono",  # Use monospaced font for alignment
        size = 4,
        lineheight = 0.9
      )
  }
  # Remove legend if plotting only a single distribution
  if(ncol(distribution_data) == 1){
    p <- p + theme(legend.position = "none")
  }
  # Add interactivity if specified
  if (interactive) {
    print(p)
    return(plotly::ggplotly(p))
  } else {
    return(p)
  }
}
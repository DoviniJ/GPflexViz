utils::globalVariables(c("distance", "R2", "CHR_A", "SNP_A", "CHR_B", "SNP_B"))

#' flex_LD_decay function
#'
#' This function generates a linkage disequilibrium (LD) decay plot based on input LD data. 
#' It supports both static and interactive visualization with customizable aesthetics, themes, 
#' and optional smoothing curves.
#'
#' @param ld_data Data frame containing LD information.
#' @param interactive Logical. If `TRUE`, returns an interactive plot using `plotly`. 
#' Default is `TRUE`.
#' @param user_colors Character vector of colors for chromosomes. If `NULL`, default 
#' colors are used. Default is `NULL`.
#' @param user_title Character. Title of the plot. Default is `"LD Decay Plot"`.
#' @param user_x_title Character. X-axis label. Default is `"Distance (Mb)"`.
#' @param user_y_title Character. Y-axis label. Default is `"R-squared (LD)"`.
#' @param user_legend_title Character. Legend title. Default is `"Chromosome"`.
#' @param user_plot_theme A ggplot2 theme object. Default is `theme_minimal()`.
#' @param user_plot_theme_specs Additional theme specifications applied to the plot. 
#' Default is a theme object with custom font sizes.
#' @param user_base Numeric. Scaling factor for distance (e.g., 1e6 for Mb). Default is `1e6`.
#' @param user_smoothing Character. Smoothing method to use in `geom_smooth`. Default is `"loess"`.
#' @param add_smoothing Logical. If `TRUE`, adds a smoothing curve to the plot. Default is `FALSE`.
#' @param add_smoothing_per_chromosome Logical. If `TRUE`, adds separate smoothing 
#' curves for each chromosome. Default is `FALSE`.
#' @param ... Additional arguments passed to `geom_smooth` (e.g., `span`, `se`, etc.).
#'
#' @return A ggplot object (static plot) or a plotly object (interactive plot), 
#' depending on the value of `interactive`.
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
#' # Load example data
#' snp_ld_data <- example_data7
#'
#' # Basic plot
#' flex_LD_decay(snp_ld_data)
#'
#' # Add a general smoothing curve
#' flex_LD_decay(snp_ld_data, add_smoothing = TRUE)
#'
#' # Add smoothing per chromosome
#' flex_LD_decay(snp_ld_data, add_smoothing = TRUE, add_smoothing_per_chromosome = TRUE)
#'
#' # Customize smoothing parameters
#' flex_LD_decay(snp_ld_data, add_smoothing = TRUE, span = 0.5, se = FALSE)
#'
#' # Use generalized additive model (GAM) for smoothing
#' flex_LD_decay(snp_ld_data, user_smoothing = "gam", add_smoothing = TRUE, 
#' add_smoothing_per_chromosome = TRUE)
flex_LD_decay <- function(ld_data, 
                                   interactive = TRUE,
                                   user_colors = NULL, 
                                   user_title = "LD Decay Plot",
                                   user_x_title = "Distance (Mb)",
                                   user_y_title = "R-squared (LD)",
                                   user_legend_title = "Chromosome",
                                   user_plot_theme = theme_minimal(),
                                   user_plot_theme_specs = theme(
                                     legend.title = element_text(size = 10),
                                     legend.text = element_text(size = 10),
                                     title = element_text(size = 15),
                                     axis.text.x = element_text(size = 10),
                                     axis.title.x = element_text(size = 10),
                                     axis.text.y = element_text(size = 10),
                                     axis.title.y = element_text(size = 10)
                                   ),
                                   user_base = 1e6, 
                                   user_smoothing = "loess",
                                   add_smoothing = FALSE, 
                                   add_smoothing_per_chromosome = FALSE,
                                   ...) {
  # Default color palette (22 colors)
  default_colors <- c(
    "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1",
    "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6", "#FDBF6F", "gray70", "khaki2",
    "maroon", "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise", "green1", "yellow4"
  )
  
  colors <- if (!is.null(user_colors)) user_colors else default_colors
  unique_chr <- unique(ld_data$CHR_A)
  if (length(colors) < length(unique_chr)) {
    stop("Not enough colors provided for the number of unique chromosomes.")
  }
  colors <- colors[1:length(unique_chr)]
  chr_colors <- setNames(colors, unique_chr)
  
  ld_data$distance <- abs(ld_data$BP_A - ld_data$BP_B) / user_base  # Convert distances to megabases (Mb)
  
  # Initialize the plot
  p <- ggplot(ld_data, aes(x = distance, y = R2, color = factor(CHR_A))) +
       suppressWarnings({geom_point(alpha = 0.6, size = 1.5, aes(x = distance, y = R2,
                                                               text = paste0("<br>CHR_A: ", as.character(CHR_A),
                                                                             "<br>SNP_A: ", as.character(SNP_A),
                                                                             "<br>CHR_B: ", as.character(CHR_B),
                                                                             "<br>SNP_B: ", as.character(SNP_B),
                                                                             "<br>Distance: ", distance,
                                                                             "<br>LD: ", R2)))}) +
    scale_color_manual(values = chr_colors) +
    labs(title = user_title, x = user_x_title, y = user_y_title, color = user_legend_title) +
    user_plot_theme +
    user_plot_theme_specs
  
  # Add smoothing curve if requested
  if (add_smoothing) {
    if (add_smoothing_per_chromosome) {
      # Add a smoothing curve for each chromosome
      p <- p + 
        suppressWarnings({geom_smooth(method = user_smoothing, aes(group = CHR_A), ... , se = FALSE)})
    } else {
      # Add a general smoothing curve for all data
      p <- p + 
        suppressWarnings({geom_smooth(method = user_smoothing, ..., color = "black", linetype = "dashed")})
    }
  }
  
  # Add interactivity if specified
  if (interactive) {
    print(p)  # Explicitly print the ggplot object
    return(plotly::ggplotly(p, tooltip = "text"))  # Returns the interactive plot
  } else {
    p
  }
  
  # Return the plot
  print(p)
}
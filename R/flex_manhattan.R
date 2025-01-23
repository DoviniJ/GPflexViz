utils::globalVariables(c("POS", "P_VALUE", "CHR", "SNP", "xmin", "xmax", "ymin", "ymax"))

#' flex_manhattan function
#'
#' This function creates a flexible Manhattan plot, which is useful for visualizing 
#' GWAS results.
#' The plot can be generated as either a static or an interactive plot. The function allows
#' customization of color schemes, titles, axis labels, and annotation features.
#'
#' @param gwas_data A data frame containing columns for SNP ID ('SNP'), chromosome ('CHR'), 
#' position ('POS'), and p-values ('P_VALUE').
#' @param interactive Logical; if TRUE, returns an interactive plotly plot, otherwise 
#' a ggplot2 plot.
#' @param user_colors A vector of colors for the chromosomes. If NULL, a default set 
#' of 22 colors is used.
#' @param user_y_cutoff A numeric value for the y-axis cutoff, used to draw a horizontal line.
#' @param user_y_cutoff_color Color of the y-axis cutoff line.
#' @param user_title The main title of the plot.
#' @param user_x_title The label for the x-axis.
#' @param user_y_title The label for the y-axis.
#' @param user_legend_title The title for the legend. If NULL, no legend is displayed.
#' @param user_plot_theme ggplot2 theme object for styling the plot background and fonts.
#' @param user_plot_theme_specs Additional ggplot2 theme specifications for custom styling.
#' @param annotate_data A vector of SNP identifiers for which annotations are to be made.
#' @param annotate_column The column name from `gwas_data` used for matching `annotate_data`.
#' @param annotate_labels Logical; if TRUE, annotations are added to the plot.
#' @param zoom_on_annotations Logical; if TRUE, the plot will zoom in on annotated SNPs.
#' @param zoom_margin Numeric; defines the margin around the zoomed area as a proportion 
#' of the range of the data.
#' @param ... Additional arguments to be passed to ggplot2 plotting functions.
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
#' flex_manhattan(example_data1)
#'
#'# Creating a non-interactive plot (using ggplot2 only)
#' flex_manhattan(example_data1, interactive = FALSE)
#'
#'# Using a custom color palette for chromosomes
#' flex_manhattan(example_data1, user_colors = rep(c("dodgerblue2", "gold1"), 
#' length.out = max(unique(example_data1$CHR))))
#'
#'# Adjusting plot themes
#' flex_manhattan(example_data1, user_plot_theme = theme_dark())
#'
#'# Modifying legend and title sizes and positions using custom theme specifications
#' flex_manhattan(example_data1, 
#'                user_plot_theme_specs = theme(
#'                  legend.title = element_text(size = 10),
#'                  legend.text = element_text(size = 8),
#'                  title = element_text(size = 20),
#'                  axis.text.x = element_text(size = 12),
#'                  axis.title.x = element_blank(),
#'                  axis.text.y = element_text(size = 12),
#'                  axis.title.y = element_text(size = 15)
#'                ))
#'
#'# Changing the y-axis cutoff to highlight significant p-values differently
#' flex_manhattan(example_data1, 
#'                user_y_cutoff = c(5, 4),  # Multiple cutoffs
#'                user_y_cutoff_color = c("darkred", "darkgreen"))  # Corresponding colors
#'
#'# Adding annotations for specific SNPs
#' annotated_snps <- c("SNP_2550", "SNP_4829", "SNP_8296")
#' flex_manhattan(example_data1, annotate_data = annotated_snps, annotate_labels = TRUE, 
#' interactive = FALSE)
#'
#'# Filtering data to display only a specific chromosome
#' flex_manhattan(subset(example_data1, CHR == 3))
#'
#'# Title, axis labels, and legend customization
#' flex_manhattan(example_data1,
#'                user_legend_title = "Chromosome",
#'                user_title = "Manhattan Plot for GWAS Results",
#'                user_x_title = "Genomic Position (bp)",
#'                user_y_title = "-log10(p-value)",
#'                user_plot_theme_specs = theme(
#'                  legend.text = element_text(size = 20),
#'                  plot.title = element_text(
#'                    hjust = 0.5,
#'                    vjust = 1,
#'                    size = 14,
#'                    face = "bold"
#'                  )))
#'
#'# Example of removing the legend entirely
#' flex_manhattan(example_data1, 
#'                user_legend_title = NULL, 
#'                user_plot_theme_specs = theme(legend.position = "none"))
#'
#'# Example of placing the legend at the bottom with a black outline
#' flex_manhattan(example_data1, 
#'                user_plot_theme_specs = theme(
#'                  legend.position = "bottom",
#'                  legend.box.background = element_rect(colour = "black")
#'                ))
#'                
#' @importFrom ggplot2 aes geom_point geom_text geom_hline geom_rect scale_colour_manual 
#' labs theme theme_minimal theme_dark element_text element_rect
#' @import reshape2 
#' @import stats 
#' @import utils
#' @import gridExtra
#' @import nortest
#' @import ggforce
#' @import cowplot
#' @import grid
#' @export
flex_manhattan <- function(gwas_data, 
                           interactive = TRUE, 
                           user_colors = NULL, 
                           user_y_cutoff = -log10(5e-8),
                           user_y_cutoff_color = "red",
                           user_title = "Manhattan Plot",
                           user_x_title = "Position",
                           user_y_title = "-log10(p-value)",
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
                           annotate_data = NULL, 
                           annotate_column = 'SNP',
                           annotate_labels = FALSE, 
                           zoom_on_annotations = FALSE,
                           zoom_margin = 1,
                           ...
) {
  
  # Add checks for input validity  
  if (!"SNP" %in% names(gwas_data) || !"P_VALUE" %in% names(gwas_data)) {
    stop("gwas_data must include 'SNP' and 'P_VALUE' columns")
  }
  if (!is.logical(interactive)) {
    warning("interactive should be a logical value; setting to default TRUE.")
    interactive <- TRUE
  }
  
  # Default color palette (22 colors)
  default_colors <- c(
    "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1",
    "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6", "#FDBF6F", "gray70", "khaki2",
    "maroon", "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise", "green1", "yellow4"
  )
  
  # Use user-defined colors if provided, otherwise default to the 22 colors
  colors <- if (!is.null(user_colors)) {
    user_colors
  } else {
    default_colors
  }
  
  # Limit colors to the number of unique chromosomes
  unique_chr <- length(unique(gwas_data$CHR))
  if (length(colors) < unique_chr) {
    stop("Not enough colors provided for the number of unique chromosomes.")
  }
  
  gwas_data$POS <- as.numeric(gwas_data$POS)
  gwas_data$CHR <- as.factor(gwas_data$CHR)
  gwas_data$P_VALUE <- as.numeric(gwas_data$P_VALUE)
  
  # Create the plot
  p <- ggplot(gwas_data, aes(x = POS, y = -log10(P_VALUE), color = CHR)) +
    suppressWarnings({geom_point(alpha = 0.75, size = 1.5, aes(x = POS, y = -log10(P_VALUE),
                                                               text = paste0("SNP: ", SNP, 
                                                                             "<br>Chr: ", as.character(CHR), 
                                                                             "<br>Pos: ", format(POS, big.mark = ","), 
                                                                             "<br>-log10(p-value): ", format(round(-log10(P_VALUE), 3), nsmall = 3))))}) +
    scale_colour_manual(
      values = colors[1:unique_chr], 
      name = if (!is.null(user_legend_title)) user_legend_title else NULL
    ) +
    geom_hline(yintercept = user_y_cutoff, linetype = "dashed", color = user_y_cutoff_color) +
    labs(
      title = user_title,
      x = user_x_title,
      y = user_y_title
    ) +
    user_plot_theme +
    user_plot_theme_specs
  
  # Annotation handling
  if (!is.null(annotate_data) && annotate_labels) {
    interactive <- FALSE
    annotated_data <- gwas_data[gwas_data[[annotate_column]] %in% annotate_data,]
    p <- p + geom_text(data = annotated_data, aes(label = SNP), hjust = -0.1, vjust = 0, size = 3, color = "black")
  }
  
  # Add zoom-in feature if enabled
  zoom_box <- NULL
  if (zoom_on_annotations && !is.null(annotate_data)) {
    interactive <- FALSE
    annotated_data <- gwas_data[gwas_data[[annotate_column]] %in% annotate_data,]
    if (nrow(annotated_data) > 0) {  # Ensure there is data to zoom into
      x_min <- min(annotated_data$POS)
      x_max <- max(annotated_data$POS)
      x_range <- x_max - x_min
      y_min <- min(-log10(annotated_data$P_VALUE))
      y_max <- max(-log10(annotated_data$P_VALUE))
      y_range <- y_max - y_min
      
      # Apply zoom with margin
      p <- p + facet_zoom(
        xlim = c(x_min - zoom_margin * x_range, x_max + zoom_margin * x_range),
        ylim = c(y_min - zoom_margin * y_range, y_max + zoom_margin * y_range)
      )
      
      # Define the zoom box
      zoom_box <- data.frame(xmin = x_min - zoom_margin * x_range,
                             xmax = x_max + zoom_margin * x_range,
                             ymin = y_min - zoom_margin * y_range,
                             ymax = y_max + zoom_margin * y_range)
    }
  }
  
  # Draw bounding box if zoom box is defined
  if (!is.null(zoom_box)) {
    p <- p + geom_rect(data = zoom_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       inherit.aes = FALSE, fill = NA, color = "darkgray", linetype = "solid", size = 1)
  }
  
  # Add interactivity if specified
  if (interactive) {
    print(p)  # Explicitly print the ggplot object
    return(plotly::ggplotly(p, tooltip = "text"))  # Returns the interactive plot
  } else {
    p
  }
}
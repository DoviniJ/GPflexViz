utils::globalVariables(c("P_VALUE", "Expected", "Observed", "SNP", "CHR", "POS", "xmin", "xmax", "ymin", "ymax"))

#' flex_qqplot function
#'
#' This function generates a customizable quantile-quantile (QQ) plot for GWAS data, 
#' allowing for interactive exploration, zooming on specific SNPs, and inclusion
#' of a Kolmogorov-Smirnov test result annotation.
#'
#' @param gwas_data A data frame containing columns for SNP ID ('SNP’), chromosome ('CHR'), 
#' position ('POS'), and p-values ('P_VALUE').
#' @param interactive Logical, whether to return an interactive plot (TRUE) or static ggplot (FALSE).
#' @param display_ks Logical, whether to display the Kolmogorov-Smirnov (KS) test result.
#' @param user_colors Character vector of length 2 specifying the colors for points and reference line.
#' @param user_title The title of the plot.
#' @param user_x_title The title for the x-axis.
#' @param user_y_title The title for the y-axis.
#' @param user_plot_theme A ggplot2 theme object to style the plot.
#' @param user_plot_theme_specs Additional ggplot2 theme specifications to override in `user_plot_theme`.
#' @param annotate_data A vector of SNP identifiers to annotate in the plot.
#' @param annotate_column The name of the column in `gwas_data` to use for matching `annotate_data`.
#' @param annotate_labels Logical, whether to label the annotated points.
#' @param zoom_on_annotations Logical, whether to create a zoomed version of the plot focusing on annotated SNPs.
#' @param zoom_margin Numeric, the margin size around zoomed points.
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
#'flex_qqplot(example_data1, user_colors = c("black", "darkorange"))
#'flex_qqplot(example_data1, display_ks = TRUE, interactive = FALSE)
#'annotated_snps <- c("SNP_1", "SNP_2", "SNP_3")
#'flex_qqplot(example_data1, annotate_data = annotated_snps, annotate_labels = TRUE)
#'flex_qqplot(example_data1, annotate_data = annotated_snps, annotate_labels = TRUE, 
#'display_ks = TRUE)
#'flex_qqplot(example_data1, annotate_data = annotated_snps, annotate_labels = TRUE, 
#'zoom_on_annotations = TRUE, zoom_margin = 0.3, user_plot_theme = theme_bw())
#' @import reshape2 
#' @import stats 
#' @import utils
#' @import gridExtra
#' @import nortest
#' @import ggforce
#' @import cowplot
#' @import grid
#' @export
flex_qqplot <- function(gwas_data,
                        interactive = TRUE,
                        display_ks = FALSE,
                        user_colors = c("dodgerblue", "darkorange"),  # Observed and expected colors
                        user_title = "QQ Plot",
                        user_x_title = "Theoretical Quantiles",
                        user_y_title = "Observed Quantiles",
                        user_plot_theme = theme_minimal(),
                        user_plot_theme_specs = theme(
                          title = element_text(size = 15),
                          axis.text = element_text(size = 10),
                          axis.title = element_text(size = 10)
                        ),
                        annotate_data = NULL,
                        annotate_column = 'SNP',
                        annotate_labels = FALSE,
                        zoom_on_annotations = FALSE,
                        zoom_margin = 1,
                        ...) {
  
  # Add checks for input validity
  if (!"SNP" %in% names(gwas_data) || !"P_VALUE" %in% names(gwas_data)) {
    stop("gwas_data must include 'SNP' and 'P_VALUE' columns")
  }
  if (!is.logical(interactive)) {
    warning("interactive should be a logical value; setting to default TRUE.")
    interactive <- TRUE
  }
  if (length(user_colors) != 2) {
    stop("user_colors must be a vector of length 2")
  }
  
  # Calculate expected p-values under the null distribution
  expected_quantiles <- ppoints(nrow(gwas_data))
  gwas_data <- gwas_data %>%
    arrange(P_VALUE)
  observed_quantiles <- -log10(gwas_data$P_VALUE)
  
  # Prepare the data frame for plotting
  plot_data <- data.frame(
    SNP = gwas_data$SNP, 
    CHR = gwas_data$CHR, 
    POS = gwas_data$POS, 
    P_VALUE = gwas_data$P_VALUE,
    Observed = observed_quantiles, 
    Expected = -log10(expected_quantiles)
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Expected, y = Observed)) +
    suppressWarnings({geom_point(aes(text = paste0("SNP: ", SNP, 
                                 "<br>Chr: ", as.character(CHR), 
                                 "<br>Pos: ", format(POS, big.mark = ","), 
                                 "<br>-log10(p-value): ", format(round(-log10(P_VALUE), 3), nsmall = 3))), 
               color = user_colors[1], alpha = 0.75, size = 1.5)}) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = user_colors[2], size = 1) +
    labs(
      title = user_title,
      x = user_x_title,
      y = user_y_title
    ) +
    user_plot_theme +
    user_plot_theme_specs
  
  
  # Display KS test results if requested
  # Perform Kolmogorov-Smirnov test
  ks_test_result <- ks.test(plot_data$Observed, plot_data$Expected)
  if (display_ks) {
    p <- p + annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 1.5, 
                      label = sprintf("KS Test p-value: %.5f", ks_test_result$p.value),
                      size = 4, color = "darkred", parse = FALSE, fontface = "bold")
  }
  
  
  # Annotation handling
  if (!is.null(annotate_data) && annotate_labels) {
    interactive <- FALSE
    annotated_data <- plot_data[plot_data[[annotate_column]] %in% annotate_data,]
    p <- p + geom_text(data = annotated_data, aes(label = SNP), hjust = -0.1, vjust = 0, size = 3, color = "black")
  }
  
  # Add zoom-in feature if enabled
  zoom_box <- NULL
  if (zoom_on_annotations && !is.null(annotate_data)) {
    interactive = FALSE
    annotated_data <- plot_data[plot_data[[annotate_column]] %in% annotate_data,]
    if (nrow(annotated_data) > 0) {
      x_min <- min(annotated_data$Expected)
      x_max <- max(annotated_data$Expected)
      y_min <- min(annotated_data$Observed)
      y_max <- max(annotated_data$Observed)
      x_range <- x_max - x_min
      y_range <- y_max - y_min
      
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
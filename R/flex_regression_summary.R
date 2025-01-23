utils::globalVariables(c("p_value", "x_var", "y_var", "estimate", "p_interval"))

#' flex_regression_summary function
#'
#' This function creates a flexible regression summary plot with customization options for 
#' interactivity, p-value intervals, legend sizes, color gradients, and more. It supports both 
#' ggplot2 and plotly outputs for static and interactive visualizations, respectively.
#'
#' @param regression_data A dataframe containing the variables `x_var`, `y_var`, `estimate`, 
#' and `p_value` which represent the horizontal axis component, vertical axis component, 
#' regression coefficient estimates, and their corresponding p-values, respectively.
#' @param interactive Logical, if TRUE returns an interactive plotly object, otherwise returns a ggplot object.
#' @param user_size_breaks Factor levels for breaking down the p-value into intervals. Default is
#' predefined intervals at p < 0.01, 0.05, 0.1, and 1.
#' @param user_legend_break_sizes A numeric vector indicating the sizes of points in the legend, corresponding
#' to the p-value intervals.
#' @param user_geom_tile Custom ggplot2 geom_tile layer, allows customization of tile appearance.
#' @param user_geom_point Custom ggplot2 geom_point layer, allows customization of point markers.
#' @param user_gradient_bar Custom ggplot2 scale_fill_gradientn for coloring the tiles based on the estimates.
#' @param user_geom_text List of custom ggplot2 geom_text layers for adding text annotations based on significance.
#' @param user_theme Custom ggplot2 theme, allows overriding the default minimal theme.
#' @param user_theme_specs Custom modifications to the default or user-specified theme.
#' @param user_labs Custom ggplot2 labs function for setting axis and legend titles.
#' @param user_tooltip A list containing tooltip strings for interactive plots.
#' @param user_size_legend_guides Custom ggplot2 guides for the size aesthetic in the legend.
#' 
#' @return A ggplot or plotly object depending on the `interactive` argument.
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
#' @rawNamespace import(dplyr, except = c(combine, lag, filter))
#' @rawNamespace import(ggplot2, except = last_plot)
#' @rawNamespace import(plotly, except = filter)
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
#' # Default usage
#' flex_regression_summary(example_data10)
#'
#' # Custom p-value intervals and annotations
#' flex_regression_summary(example_data10, user_size_breaks = cut(example_data10$p_value,
#'                        breaks = c(0, 0.05, 1), labels = c("< 0.05", "1"), include.lowest = TRUE),
#'                         user_legend_break_sizes = c(12, 8), 
#'                         user_geom_text = list(geom_text(data = subset(example_data10, 
#'                                               (p_value <= 0.05)),
#'                                 aes(label = "sig"),
#'                                 size = 4, vjust = 0.5, hjust = 0.5,
#'                                 color = "black",
#'                                 fontface = "italic")))
#'
#' # Custom formatting and gradient colors
#' flex_regression_summary(example_data10, interactive = FALSE, 
#' user_geom_point = geom_point(shape = 25, 
#' color = "white", stroke = 0),
#'                        user_gradient_bar = scale_fill_gradientn(colours = c("purple4", 
#'                        "white", "green4")))
flex_regression_summary <- function(regression_data,
                         interactive = TRUE,
                         user_size_breaks = cut(regression_data$p_value,
                                                breaks = c(0, 0.01, 0.05, 0.1, 1),
                                                labels = c("< 0.01", "0.05", "0.1", "1"),
                                                include.lowest = TRUE),
                         user_legend_break_sizes = c(12,10,8,7),
                         user_geom_tile = geom_tile(color = "white", fill = "white", size = 5),
                         user_geom_point = geom_point(shape = 22, color = "white", stroke = 0),
                         user_gradient_bar = scale_fill_gradientn(colours = c("red4", "white", "steelblue4"),
                                                              limit = c(-max(abs(regression_data$estimate))-0.05, max(abs(regression_data$estimate))+0.05),
                                                              guide = guide_colorbar(title = "Estimate",
                                                                                     label.position = "right",
                                                                                     barwidth = 1,
                                                                                     barheight = 5,
                                                                                     title.position = "top",
                                                                                     order = 1)),
                         user_geom_text = list(geom_text(data = subset(regression_data, (p_value <= 0.01)),
                                                    aes(label = "**"),
                                                    size = 6, vjust = 0.5, hjust = 0.5,
                                                    show.legend = FALSE,
                                                    color = "white",
                                                    fontface = "bold"), 
                                          geom_text(data = subset(regression_data, (p_value <= 0.05 & p_value > 0.01)),
                                                     aes(label = "*"),
                                                     size = 6, vjust = 0.5, hjust = 0.5,
                                                     show.legend = FALSE,
                                                     color = "white",
                                                     fontface = "bold")),
                         user_theme = theme_minimal(),
                         user_theme_specs = theme(axis.text.x = element_text(angle = 45,
                                                                             hjust = 1,
                                                                             vjust = 1,
                                                                             size = 15,
                                                                             colour = "black"),
                                                  axis.text.y = element_text(size=15),
                                                  axis.title.y = element_text(size = 15),
                                                  axis.title.x = element_text(size = 15),
                                                  text = element_text(family = "serif"),
                                                  legend.text = element_text(size = 15),
                                                  legend.title = element_text(size = 15),
                                                  legend.key.width = unit(0.01, "cm"),
                                                  legend.key.height = unit(0.01, "cm"),
                                                  legend.box = "horizontal",
                                                  legend.spacing.y = unit(0.5, "cm"),
                                                  legend.position = "right",
                                                  legend.box.margin = margin(b=15, t=-20, l=5, r=5),
                                                  legend.box.just = ("bottom"),
                                                  plot.margin = margin(b=5, t=20, l=5, r=5)),
                         user_labs = labs(x = "x variable", y = "y variable", fill = "Estimate", size = "p value"),
                         user_tooltip = list("Estimate: ", "p-value: "),
                         user_size_legend_guides = guides(size = guide_legend(reverse = F,
                                                    label.position = "right",
                                                    title.position = "top",
                                                    override.aes = list(colour="grey",
                                                                        shape=15,
                                                                        fill="white",
                                                                        size=user_legend_break_sizes-3)))){
  # Create a new column with p-value intervals
  regression_data$p_interval <- user_size_breaks
  
  # Define your custom legend values and sizes
  legend_data <- data.frame(
    p_value_interval = sort(unique(regression_data$p_interval)),
    size = user_legend_break_sizes
  )
  
  # Create the plot
  p <- ggplot(data = regression_data, aes(x = x_var, y = y_var, fill = estimate, size = p_interval, 
                                       text = paste0(user_tooltip[[1]], estimate, 
                                                     "<br>", user_tooltip[[2]], p_value))) +
              user_geom_tile +
              user_geom_point +
              geom_hline(yintercept = seq_along(unique(regression_data$y_var)) + 0.5, color = "grey") +
              geom_vline(xintercept = seq_along(unique(regression_data$x_var)) + 0.5, color = "grey") +
              geom_vline(xintercept = 0.5, color = "grey")+
              geom_hline(yintercept = 0.5, color = "grey")+
              user_geom_text +
              user_gradient_bar +
              scale_size_manual(values = legend_data$size,
                                breaks = legend_data$p_value_interval,
                                labels = legend_data$p_value_interval,
                                guide = guide_legend(title = "p value",
                                                     label.position = "right",
                                                     title.position = "top")) +
              user_theme +
              user_theme_specs +
              user_labs +
              user_size_legend_guides
  
  # Add interactivity if specified
  if (interactive) {
    print(p)  # Print the ggplot object
    # Create interactive plotly plot with custom tooltip
    plotly_fig <- plotly::ggplotly(p + theme(legend.position = "none"), tooltip = "text")
    
    #Customize hovertemplate for better readability
    plotly_fig <- plotly_fig %>%
      layout(hoverlabel = list(
        bgcolor = "white",
        font_size = 12,
        bordercolor = "black",
        borderwidth = 1
      ))
    
    return(plotly_fig)  # Return the interactive plot
  } else {
    return(p)  # Return the ggplot object
  }
}
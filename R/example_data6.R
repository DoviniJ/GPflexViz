#' accuracy_diff_data
#' This contains prediction accuracy related information for detailed model comparison (2 methods at a time). 
#' @format A dataframe with 6 rows and 9 columns
#' \describe{
#'  \item{trait}{Trait name}
#'  \item{method}{Method name (there can be only 2 types)}
#'  \item{R2}{Prediction accuracy}
#'  \item{lower_limit_R2}{Lower limit of prediction accuracy}
#'  \item{upper_limit_R2}{Upper limit of prediction accuracy}
#'  \item{lower_limit_difference_R2}{Lower limit of prediction accuracy difference (common to the 2 methods)}
#'  \item{upper_limit_difference_R2}{Upper limit of prediction accuracy difference (common to the 2 methods)}
#'  \item{p_value_difference_R2}{p value of diffence between prediction accuracy (common to the 2 methods)}
#' }
"example_data6"
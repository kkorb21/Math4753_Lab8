#' Plot the CDF of B ~ Gamma(2,1)
#'
#' Generates a ggplot2 line plot of the cumulative distribution function
#' for the variable B, which follows a Gamma distribution with shape = 2
#' and rate = 1. The CDF is given by F_B(b) = 1 - exp(-b) * (b + 1).
#'
#' @name plot_B_cdf
#'
#' @param b_max Numeric. Upper bound for the horizontal axis of the plot (default: 10).
#' @param resolution Integer. Number of points sampled to draw the CDF curve (default: 200).
#'
#' @returns A ggplot2 object representing the CDF plot.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#'
#' @examples
#' plot_B_cdf()

utils::globalVariables(c("b", "F_B"))

plot_B_cdf <- function(b_max = 10, resolution = 200) {

  # Define the range of b values
  b_vals <- seq(0, b_max, length.out = resolution)

  # Define the CDF function
  F_B <- function(b) {
    1 - exp(-b) * (b + 1)
  }

  # Create a data frame for plotting
  df <- data.frame(
    b = b_vals,
    F_B = F_B(b_vals)
  )

  # Plot using ggplot2
  ggplot(df, aes(x = b, y = F_B)) +
    geom_line(color = "darkred", size = 1.2) +
    labs(title = "CDF of B ~ Gamma(2,1)",
         x = "b", y = "F_B(b)") +
    theme_minimal()
}

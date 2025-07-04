#' Plot Marginal Densities of g(a) and g(b)
#'
#' Generates separate `ggplot2` visualizations for the marginal densities
#' of g(a) and g(b), where a ~ Uniform(0, 1) and b ~ Gamma(2, 1).
#'
#' @name plot_joint_marginals
#' @param a_resolution Integer. Number of points to use for the uniform density g(a) over [0, 1].
#' @param b_max Numeric. Maximum value of b to plot for the gamma-like density g(b).
#' @param b_resolution Integer. Number of points to sample along b-axis for plotting g(b).
#'
#' @returns A named list containing two `ggplot` objects:
#'   \describe{
#'     \item{ga_plot}{Plot of g(a) on [0, 1]}
#'     \item{gb_plot}{Plot of g(b) on [0, bmax]}
#'   }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal ylim
#'
#' @examples
#' plots <- plot_joint_marginals()
#' print(plots$ga_plot)
#' print(plots$gb_plot)

utils::globalVariables(c("a", "b", "g_a", "g_b", "ylim"))

plot_joint_marginals <- function(a_resolution = 100, b_max = 10, b_resolution = 200) {

  # g(a): Uniform(0,1)
  ga_df <- data.frame(
    a = seq(0, 1, length.out = a_resolution),
    g_a = rep(1, a_resolution)
  )

  ga_plot <- ggplot(ga_df, aes(x = a, y = g_a)) +
    geom_line(color = "steelblue", size = 1.2) +
    labs(title = "Marginal Density g(a)", x = "a", y = "g(a)") +
    ylim(0, 1.1) +
    theme_minimal()

  # g(b): Gamma(2,1) → b * exp(-b)
  gb_df <- data.frame(
    b = seq(0, b_max, length.out = b_resolution)
  )
  gb_df$g_b <- gb_df$b * exp(-gb_df$b)

  gb_plot <- ggplot(gb_df, aes(x = b, y = g_b)) +
    geom_line(color = "darkgreen", size = 1.2) +
    labs(title = "Marginal Density g(b)", x = "b", y = "g(b)") +
    theme_minimal()

  # Return both ggplot objects in a list
  list(ga_plot = ga_plot, gb_plot = gb_plot)
}

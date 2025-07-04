#' Simulate B using density-based optimization and summarize results
#'
#' Simulates values of B by numerically solving an objective function using the
#' w-F theorem approach (e.g., density matching). Compares histogram to the
#' theoretical Gamma(2,1) density.
#'
#' @name rmyF
#' @param iter Integer. Number of values to simulate (default: 10000).
#'
#' @return A named list containing:
#' \describe{
#'   \item{q}{Quantiles of simulated B: 25\%, 50\%, 97.5\%}
#'   \item{meanB}{Mean of simulated B}
#'   \item{varB}{Sample variance of B}
#'   \item{varBtheory}{Theoretical variance = 2}
#' }
#'
#' @importFrom stats dgamma optimise
#' @importFrom graphics hist curve
#' @importFrom stats runif
#' @export
#'
#' @examples
#' result <- rmyF(iter = 10000)
#' print(result)

utils::globalVariables("x")

rmyF <- function(iter = 10000) {

  # Generate uniform random numbers (inverse transform approach)
  u <- runif(iter)

  # Define inverse CDF via numerical optimisation
  inverse_gamma <- function(p) {
    obj <- function(x) abs(p - (1 - exp(-x) * (x + 1)))  # CDF of Gamma(2,1)
    optimise(obj, interval = c(0, 20))$minimum
  }

  # Simulate values of B by inverting its CDF numerically
  B <- vapply(u, inverse_gamma, numeric(1))

  # Summaries
  q <- quantile(B, probs = c(0.25, 0.50, 0.975))
  meanB <- mean(B)
  varB <- var(B)
  varBtheory <- 2

  # Visual: histogram and theoretical density
  hist(B, probability = TRUE, main = "Histogram of y",
       xlab = "B", border = "white", breaks = 50)
  curve(dgamma(x, shape = 2, rate = 1), add = TRUE, col = "blue", lwd = 2)

  return(list(
    q = q,
    meanB = meanB,
    varB = varB,
    varBtheory = varBtheory
  ))
}

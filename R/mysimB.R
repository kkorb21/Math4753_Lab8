#' Simulate B = X + Y and summarize statistics
#'
#' This function simulates values of B = X + Y, where X and Y are independent
#' Exp(1) variables, and returns summary statistics with a visual comparison
#' to the theoretical Gamma(2,1) distribution.
#'
#' @name mysimB
#' @param iter Integer. Number of simulations to perform (default: 1000).
#'
#' @return A named list containing:
#' \describe{
#'   \item{q}{Quantiles of simulated B: 25\%, 50\%, 97.5\%}
#'   \item{meanB}{Mean of simulated B values}
#'   \item{varB}{Sample variance of B}
#'   \item{varBtheory}{Theoretical variance of B = 2}
#' }
#'
#' @importFrom graphics hist curve
#' @importFrom stats quantile rexp var dgamma
#' @export
#'
#' @examples
#' result <- mysimB(iter = 1000)
#' print(result)

utils::globalVariables("x")

mysimB <- function(iter = 1000) {
  # Simulate X and Y ~ Exp(1)
  X <- rexp(iter, rate = 1)
  Y <- rexp(iter, rate = 1)
  B <- X + Y

  # Compute statistics
  q <- quantile(B, probs = c(0.25, 0.50, 0.975))
  meanB <- mean(B)
  varB <- var(B)
  varBtheory <- 2  # Variance for Gamma(2,1)

  # Plot histogram with theoretical Gamma(2,1) overlay
  hist(B, probability = TRUE, main = "Histogram of Bsim",
       xlab = "Bsim", border = "white", breaks = 40)
  curve(dgamma(x, shape = 2, rate = 1), add = TRUE,
        col = "blue", lwd = 2, from = 0, to = max(B))

  # Return summary
  return(list(
    q = q,
    meanB = meanB,
    varB = varB,
    varBtheory = varBtheory
  ))
}

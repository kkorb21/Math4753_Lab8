#' Simulate A = X / (X + Y) and summarize statistics
#'
#' This function simulates values of A, computes summary statistics, and plots
#' the histogram of A with the theoretical density of a Uniform(0, 1) distribution.
#'
#' @param iter Integer. Number of simulations to perform (default: 1000).
#'
#' @return A named list containing:
#' \describe{
#'   \item{q}{Quantiles: 25\%, 50\%, 97.5\%}
#'   \item{meanA}{Mean of simulated A values}
#'   \item{varA}{Sample variance of A}
#'   \item{varAtheory}{Theoretical variance of A (Uniform(0,1): 1/12)}
#' }
#'
#' @importFrom graphics hist lines
#' @importFrom stats quantile rexp var
#' @export
#'
#' @examples
#' result <- mysimA(iter = 1000)
#' print(result)

mysimA <- function(iter = 1000) {
  # Simulate X and Y ~ Exp(1)
  X <- rexp(iter, rate = 1)
  Y <- rexp(iter, rate = 1)
  A <- X / (X + Y)

  # Summary statistics
  q <- quantile(A, probs = c(0.25, 0.50, 0.975))
  meanA <- mean(A)
  varA <- var(A)
  varAtheory <- 1 / 12  # Variance of Uniform(0,1)

  # Plot histogram + theoretical density
  hist(A, probability = TRUE, main = "Histogram of A",
       xlab = "A", col = "gray", border = "white")
  lines(x = c(0, 1), y = rep(1, 2), col = "blue", lwd = 2)

  # Return result
  return(list(
    q = q,
    meanA = meanA,
    varA = varA,
    varAtheory = varAtheory
  ))
}

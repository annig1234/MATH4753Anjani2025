#' myncurve Curve and Probability
#'
#' @param mu Mean
#' @param sigma Standard deviation
#' @param a Value of upper limit of probability
#'
#'
#' @export
#'
#'
myncurve <- function(mu,sigma,a) {
  curve(dnorm(x, mean=mu, sd=sigma), xlim= c(mu -3 * sigma, mu + 3 * sigma),
        col="black", lwd=2, xlab="X", ylab="Density",
        main="Normal Distribution Curve")
  x_curv <- seq(mu - 3 * sigma, a, length = 1000)
  y_curv <- dnorm(x_curv, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, x_curv, a), c(0,y_curv,0), col="red")
  prob <- pnorm(a, mean=mu, sd=sigma)
  prob <- round(prob, 4)
  list(mu = mu, sigma = sigma, a = a, area = prob)
}

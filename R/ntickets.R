#' ntickets Number of tickets to be sold based on probability plane is overbooked
#'
#' @param N Number of seat in flight
#' @param gamma probability that plane is overbooked
#' @param p probabiility of a "show"
#'
#' @return A list with nd, nc, N, gamma, and p
#'
#' @export
#'
#'
#'
#'
ntickets <- function(N, gamma, p) {

  # Discrete distribution objective function
  disc <- function(n) {
    return(1 - gamma - stats::pbinom(N, size = n, prob = p)) #use binomial
  }

  # Continuous (normal approx) objective function
  cont <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    return(1 - gamma - stats::pnorm(N + 0.5, mean = mu, sd = sigma))
  }


  # Discrete
  n_value <- N:(N + 30) #steps in which we want to find n
  disc_value <- numeric(length(n_value)) # create vector

  for (i in seq_along(n_value)) {
    disc_value[i] <- disc(n_value[i]) #for loop to find n
  }

  nd_index <- which.min(abs(disc_value)) #smallest n
  nd <- n_value[nd_index]

  # Plot discrete case
  graphics::layout(matrix(1:2, nrow = 2, ncol = 1))
  plot(n_value, disc_value, type = "o", col = "blue", pch = 16,
       main = paste0("Objective Vs n to find optimal tickets sold\n(", nd, ") gamma= ", gamma, " N=", N, " discrete"),
       xlab = "n",
       ylab = "Objective")
  graphics::abline(h = 0, col = "red")
  graphics::abline(v = nd, col = "red", lwd = 2)


  # Continuous case
  nc <- stats::uniroot(cont, lower = N, upper = N + 30)$root #to find n when f(n) = 0
  n_cont <- seq(N, N + 30, by = 0.1) # using steps of 0.1 from N to N +30
  cont_value <- numeric(length(n_cont))
  for (i in seq_along(n_cont)) {
    cont_value[i] <- cont(n_cont[i])
  }

  # Plot continuous case
  plot(n_cont, cont_value, type = "l", col = "black",
       main = paste0("Objective Vs n to find optimal tickets sold\n(", nc, ") gamma= ", gamma, " N=", N, " continuous"),
       xlab = "n",
       ylab = "Objective")
  graphics::abline(h = 0, col = "blue")
  graphics::abline(v = nc, col = "blue", lwd = 2)

  #print the list of variables:
  return(list(
    nd = nd, nc = nc, N = N,p = p, gamma = gamma))
}


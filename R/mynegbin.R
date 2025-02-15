#' Negative Binomial function
#'
#' @param y A random numeric value
#' @param r Number of elements
#' @param p Probability
#'
#' @returns probability using negative binomial
#' @export
#'
#'
mynegbin <- function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}

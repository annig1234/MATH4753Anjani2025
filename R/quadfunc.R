#' Quadratic function
#'
#' @param x A quantitative vector
#' @param coef coefficients
#' @param xk A numeric value
#'
#' @returns An intercept value
#' @export
#'
#'
quadfunc <- function(x,coef,xk){
  coef[1] + coef[2] * (x) + coef[3] * (x - xk) * (x - xk > 0)
}

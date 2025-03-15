library(purrr)


#task 1
myg <- function(x){
  1/500*(25-x^2)
}
l <- integrate(myg, -5,5)
c = 1/l$value
c

#task 2
dtrain <- function(x){
  ifelse( x > -5 & x < 5, (3/500)*(25-x^2), 0) # complete!!
}

dtrain(0)

curve(dtrain, xlim=c(-10,10))

#task 3
ptrain <- function(q){
  ifelse(q <= -5,
         0,
         ifelse(q > -5 & q < 5,
                1/500*(75*q - q^3 + 250),
                1))

}

ptrain(0)
curve(ptrain, xlim=c(-10,10))

#task 4

qtrain <- function(p){
  ifelse(p < 0 | p > 1,stop("p must be between 0 and 1"), "")

  myroot <- function(p) {
    k <- function(x){
      p -  1/500*(75*x - x^3 + 250)
    }
    l <- stats::uniroot(k, interval = c(-5,5)) # complete
    l$root
  }

  purrr::map_vec(.x = p, .f = myroot) # You must read the doc for this
}

qtrain(0.75)
qtrain(p = c(0.25,0.5,0.75))

#task 5

rtrain <- function(n){
  r <- runif(n) # complete!!
  qtrain(r)
}

rtrain(10000)

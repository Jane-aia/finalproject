#' mix normal distribution
#'
#' This function generate n observations from a mixture of two normal distributions
#' @param n: the number of observations
#' @param p: the probability that an observations is drawn from a normal distribution(mu1,sd1)
#' @param mu1: the mean of a normal distribution
#' @param mu2: the mean of another normal distribution
#' @param sd1: the standard deviation of a normal distribution
#' @param sd2: the standard deviation of another normal distribution
#' @keywords mixture normal distribution
#' @import LaplacesDemon
#' @export
#' @examples mix.nor.dis(10,.2,4,2,-1,1)
#'

mix.nor.dis <- function(n, p, mu1, mu2, sd1, sd2){

  y1 <- rnorm(n,mean=mu1, sd = sd1)
  y0 <- rnorm(n,mean=mu2, sd = sd2)
  flag <- LaplacesDemon::rbern(n,prob=p)
  y <- y1*flag + y0*(1-flag)
  return(y)
}

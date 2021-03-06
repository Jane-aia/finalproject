% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mix.nor.dis.R
\name{mix.nor.dis}
\alias{mix.nor.dis}
\title{mix normal distribution}
\usage{
mix.nor.dis(n, p, mu1, mu2, sd1, sd2)
}
\arguments{
\item{n:}{the number of observations}

\item{p:}{the probability that an observations is drawn from a normal distribution(mu1,sd1)}

\item{mu1:}{the mean of a normal distribution}

\item{mu2:}{the mean of another normal distribution}

\item{sd1:}{the standard deviation of a normal distribution}

\item{sd2:}{the standard deviation of another normal distribution}
}
\description{
This function generate n observations from a mixture of two normal distributions
}
\examples{
mix.nor.dis(10,.2,4,2,-1,1)

}

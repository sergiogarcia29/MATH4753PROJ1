% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myNRML.R
\name{myNRML}
\alias{myNRML}
\title{myNRML (My newton raphson maximum likelihood)}
\usage{
myNRML(x0, delta = 0.001, llik, xrange, parameter = "param")
}
\arguments{
\item{x0}{start value to start finding the inflexion point of the function}

\item{delta}{step ofthe estimation (the smaller this is, the accurate will be the answer)}

\item{llik}{log function of the data of interest}

\item{xrange}{range of the log lik function, the interval where the maximum should be found}

\item{parameter}{name of the parameter of interest}
}
\value{
returns a graph of the loglik function and a graph of the netwon raphson algorithm of the derivative. Also returns a table with the steps of the approximation until finding the maximum
}
\description{
myNRML (My newton raphson maximum likelihood)
}
\examples{
myNRML(x0=1,delta=0.000001,llik=function(x) log(dpois(4,x)*dpois(6,x)*dpois(7,x)*dpois(6,x)*dpois(5,x)),xrange=c(0,20),parameter="lambda" )
Will give you the point estimator of the parameter lambda of a random sample y=4,6,7,6,5 which has a poisson distribution
}

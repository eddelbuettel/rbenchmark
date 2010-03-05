\name{rbenchmark}
\alias{rbenchmark}
\docType{package}
\title{rbenchmark provides a simple routine for benchmarking R code.}
\description{
\code{rbenchmark} is inspired by the Perl module Benchmark, and is intended to facilitate benchmarking of arbitrary R code.

The library consists of just one function, benchmark, which is a simple wrapper around system.time.

Given a specification of the benchmarking process (counts of replications, evaluation environment) and an arbitrary number of expressions, benchmark evaluates each of the expressions in the specified environment, replicating the evaluation as many times as specified, and returning the results conveniently wrapped into a data frame. 
}
\details{
\tabular{ll}{
Package: \tab benchmark\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2010-03-05\cr
License: GPL-2
LazyLoad: \tab yes\cr
}



}
\author{
Wacek Kusnierczyk

Maintainer: Wacek Kusnierczyk <waku@idi.ntnu.no>
}
\keyword{ package }
\examples{

library(rbenchmark)
benchmark(1:10^8, replications=5)

}

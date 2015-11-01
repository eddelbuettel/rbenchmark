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
Version: \tab 1.0.0\cr
Date: \tab 2012-08-30\cr
License: GPL-2
LazyLoad: \tab yes\cr
}



}
\author{
Wacek Kusnierczyk

Maintainer: Wacek Kusnierczyk <waku@idi.ntnu.no>

Contributors: Dirk Eddelbuettel <edd@debian.org>, Berend Hasselman <bhh@xs4all.nl>
}
\keyword{ package }
\examples{

library(rbenchmark)

# Example 1
# Benchmarking the allocation of one 10^6-element numeric vector,
# by default replicated 100 times
benchmark(1:10^6)

# simple test functions used in subsequent examples
random.array = function(rows, cols, dist=rnorm) 
                  array(dist(rows*cols), c(rows, cols))
random.replicate = function(rows, cols, dist=rnorm)
                      replicate(cols, dist(rows))

# Example 2
# Benchmarking an expression multiple times with the same replication count,
# output with selected columns only
benchmark(replications=rep(100, 3),
          random.array(100, 100),
          random.array(100, 100),
          columns=c('test', 'elapsed', 'replications'))

# Example 3
# Benchmarking two named expressions with three different replication
# counts, output sorted by test name and replication count,
# with additional column added after the benchmark
within(benchmark(rep=random.replicate(100, 100),
                 arr=random.array(100, 100),
                 replications=10^(1:3),
                 columns=c('test', 'replications', 'elapsed'),
                 order=c('test', 'replications')),
       { average = elapsed/replications })

# Example 4
# Benchmarking a list of arbitrary predefined expressions
tests = list(rep=expression(random.replicate(100, 100)), 
             arr=expression(random.array(100, 100)))
do.call(benchmark,
        c(tests, list(replications=100,
                      columns=c('test', 'elapsed', 'replications'),
                      order='elapsed')))

}

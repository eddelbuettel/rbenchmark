#!/usr/bin/r

# loading benchmark directly from googlecode
source('http://rbenchmark.googlecode.com/svn/trunk/benchmark.r')

# benchmark the allocation of one 10^6-element numeric vector, replicated 100 times
result = benchmark(1:10^6)
print(result)

# benchmark the application of two functions with like functionality but different implementation
means.rep = function(n, m) mean(replicate(n, rnorm(m)))
means.pat = function(n, m) colMeans(array(rnorm(n*m), c(m, n)))
result = benchmark(
   rep=means.rep(100, 100), 
   pat=means.pat(100, 100), 
   replications=10^(1:3),
   order=c('replications', 'elapsed'))
print(result)
   
# six benchmarks for means.pat(100, 100), each with 100 replications
means.pat = function(n, m) colMeans(array(rnorm(n*m), c(m, n)))
result = within(
   benchmark(
      replications=rep(100, 3), 
      means.pat(100, 100), 
      means.pat(100, 100), 
      columns=c('test', 'elapsed', 'replications')),
   {average=elapsed/replications})
print(result)

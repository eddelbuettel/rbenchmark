library(rbenchmark)

# Example 1
# Benchmark the allocation of one 10^6-element numeric vector,
# replicated 100 times
benchmark(1:10^6)

# Example 2
# A call to benchmark with two named expressions and three replication
# counts, output sorted by the replication counts and then by the elapsed time:
means.rep = function(n, m) mean(replicate(n, rnorm(m)))
means.pat = function(n, m) colMeans(array(rnorm(n*m), c(m, n)))
benchmark(rep=means.rep(100, 100),
          pat=means.pat(100, 100),
          replications=10^(1:3),
          order=c('replications', 'elapsed'))

# Example 3
# A call to benchmark with duplicate expressions and replication counts,
# output with selected columns, additional column computed afterwards:
means.pat = function(n, m) colMeans(array(rnorm(n*m), c(m, n)))
within(benchmark(replications=rep(100, 3),
                 means.pat(100, 100),
                 means.pat(100, 100),
                 columns=c('test', 'elapsed', 'replications')), 
       {average=elapsed/replications})

# Example 4
# A call to benchmark with a list of arbitrary predefined expressions:
means.rep = function(n, m) mean(replicate(n, rnorm(m)))
means.pat = function(n, m) colMeans(array(rnorm(n*m), c(m, n)))
tests = list(rep=expression(means.rep(100, 100)), pat=expression(means.pat(100, 100)))
do.call(benchmark,
        c(tests, list(replications=100,
                      columns=c('test', 'elapsed', 'replications'),
                      order='elapsed')))



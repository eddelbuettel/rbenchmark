
## rbenchmark: Benchmarking Routine for R

[![ci](https://github.com/eddelbuettel/rbenchmark/actions/workflows/r-ci.yaml/badge.svg)](https://github.com/eddelbuettel/rbenchmark/actions/workflows/r-ci.yaml)
[![License](https://eddelbuettel.github.io/badges/GPL2+.svg)](https://www.r-project.org/Licenses/GPL-2)
[![CRAN](https://www.r-pkg.org/badges/version/rbenchmark)](https://cran.r-project.org/package=rbenchmark)
[![r-universe](https://eddelbuettel.r-universe.dev/badges/rbenchmark)](https://eddelbuettel.r-universe.dev/rbenchmark)
[![Dependencies](https://tinyverse.netlify.app/badge/rbenchmark)](https://cran.r-project.org/package=rbenchmark)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rbenchmark?color=brightgreen)](https://www.r-pkg.org:443/pkg/rbenchmark)
[![Last Commit](https://img.shields.io/github/last-commit/eddelbuettel/rbenchmark)](https://github.com/eddelbuettel/rbenchmark)

### About

Benchmarking of arbitrary R code, inspired by similar functionality in a Perl module, is implemented
via a simple wrapper around system.time().  Given a specification of the benchmarking process
(counts of replications, evaluation environment) and an arbitrary number of expressions, the
function evaluates each of the expressions in the specified environment, replicating the evaluation
as many times as specified, and returning the results conveniently wrapped into a data frame.
 
### Installation

The package is on CRAN and can be installed via

```r
> install.packages("rbenchmark")
```

The [r-univere page][r-univere page] has source and binary builds from the repository version which
may differ from the CRAN version and shows how to install them. Lastly, one can always rely on
`remotes::install_github()`:

```r
> remotes::install_github("eddelbuettel/rbenchmark")
```

### Author

Wacek Kusnierczyk created the package; Dirk Eddelbuettel and Berend Hasselman provided additional pieces.

Dirk Eddelbuettel adopted the package in 2026 (having already filled in during 2012).

### License

GPL (>= 2)


[r-univere page]: https://eddelbuettel.r-universe.dev/rbenchmark

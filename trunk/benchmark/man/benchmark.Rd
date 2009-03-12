\name{benchmark}
\alias{benchmark}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
benchmark(..., columns = c("test", "replications", "user.self", "sys.self", "elapsed", "user.child", "sys.child"), order = "test", replications = 100, environment = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
  \item{columns}{ ~~Describe \code{columns} here~~ }
  \item{order}{ ~~Describe \code{order} here~~ }
  \item{replications}{ ~~Describe \code{replications} here~~ }
  \item{environment}{ ~~Describe \code{environment} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(
      ..., 
      columns=c('test', 'replications', 'user.self', 'sys.self', 'elapsed', 'user.child', 'sys.child'),
      order='test',
      replications=100,
      environment=parent.frame()) {
   arguments = match.call()[-1]
   parameters = names(arguments)
   if (is.null(parameters))
      parameters = as.character(arguments)
   else {
      keep = ! parameters \%in\% c('columns', 'order', 'replications', 'environment')
      arguments = arguments[keep]
      parameters = parameters[keep] }
   n = list(tests=length(arguments), replications=length(replications))
   replications = rep(replications, n$tests)
   labels = rep(ifelse(parameters=='', as.character(arguments), parameters), each=n$replications)
   values = list(...)
   for (i in 1:n$tests)
      if (is.expression(values[[i]])) 
         arguments[i] = values[i]
   tests = rep(arguments, each=n$replications)
   result = data.frame(
      row.names=NULL,
      test=labels,
      replications=as.integer(replications),
      t(mapply(
         function(test, replications) 
            system.time(replicate(replications, { eval(test, environment); NULL })),
         tests,
         replications)))
   result[do.call(base::order, result[order]), columns, drop=FALSE] }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

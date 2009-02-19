#!/usr/bin/r

# a simple routine for benchmarking an arbitrary number of expressions
benchmark = function(
      ..., 
      columns=c('test', 'replications', 'user.self', 'sys.self', 'elapsed', 'user.child', 'sys.child'),
      replications=100,
      environment=parent.frame()) {
   arguments = match.call()[-1]
   parameters = names(arguments)
   if (is.null(parameters))
      parameters = as.character(arguments)
   else {
      keep = ! parameters %in% c('columns', 'replications', 'environment')
      arguments = arguments[keep]
      parameters = parameters[keep] }
   n = list(tests=length(arguments), replications=length(replications))
   replications = rep(replications, n$tests)
   labels = rep(ifelse(parameters=='', as.character(arguments), parameters), each=n$replications)
   tests = rep(arguments, each=n$replications)
   data.frame(
      row.names=NULL,
      test=labels,
      replications=as.integer(replications),
      t(mapply(
         function(test, replications) 
            system.time(replicate(replications, { eval(test, environment); NULL })),
         tests,
         replications)))[, columns, drop=FALSE] }

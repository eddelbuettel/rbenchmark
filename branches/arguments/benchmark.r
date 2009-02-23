#!/usr/bin/r

# a simple routine for benchmarking an arbitrary number of expressions
benchmark = function(
      ..., 
      columns=c('test', 'replications', 'arguments', 'user.self', 'sys.self', 'elapsed', 'user.child', 'sys.child'),
      order='test',
      replications=100,
      environment=parent.frame(),
      arguments=NULL) {

   args = match.call()[-1]
   pars = names(args)
   if (is.null(pars))
      pars = as.character(args)
   else {
      keep = ! pars %in% c('columns', 'order', 'replications', 'environment', 'arguments')
      args = args[keep]
      pars = pars[keep] 
      labels = ifelse(pars=='', as.character(args), pars) }
      
   n = list(
      tests=length(args),
      replications=length(replications), 
      arguments=if(!is.null(arguments)) length(arguments) else 1)
      
   if (!is.null(arguments))
      arguments = rep(as.list(arguments), n$tests*n$replications)
   replications = rep(rep(replications, each=n$arguments), n$tests)
   labels = rep(labels, each=n$replications*n$arguments)
   tests = rep(args, each=n$replications*n$arguments)
      
   result = data.frame(
      row.names=NULL,
      test=labels,
      replications=as.integer(replications),
      arguments=rep(1:n$arguments, n$tests*n$replications),
      if (is.null(arguments))
         t(mapply(
            function(test, replications)
               system.time(replicate(replications, { eval(test, envir=environment); NULL })) - 
               system.time(replicate(replications, { eval(NULL, envir=environment); NULL })),
            tests,
            replications))
      else {
         ignore = function(...) NULL
         t(mapply(
            function(test, replications, arguments) {
               arguments = as.list(arguments)
               system.time(replicate(replications, { do.call(test, arguments, envir=environment); NULL })) -
               system.time(replicate(replications, { do.call(ignore, arguments, envir=environment); NULL })) }, 
            tests,
            replications,
            arguments)) } )
   result[do.call(base::order, result[order]), columns, drop=FALSE] }

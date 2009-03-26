`benchmark` <-
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
      keep = ! parameters %in% c('columns', 'order', 'replications', 'environment')
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
   if (is.null(order))
      order = 1:length(tests)
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


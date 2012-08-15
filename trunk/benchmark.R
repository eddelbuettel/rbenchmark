`benchmark` <-
function(
      ..., 
      columns=c('test', 'replications', 'elapsed', 'relative', 'user.self', 'sys.self', 'user.child', 'sys.child'),
      order='test',
      replications=100,
      environment=parent.frame(),
      relative='elapsed') {
   arguments = match.call()[-1]
   parameters = names(arguments)
   if (is.null(parameters))
      parameters = as.character(arguments)
   else {
      keep = ! parameters %in% c('columns', 'order', 'replications', 'environment', 'relative')
      arguments = arguments[keep]
      parameters = parameters[keep] }
   n = list(tests=length(arguments), replications=length(replications))
   replications = rep(replications, n$tests)
   labels = rep(ifelse(parameters=='', as.character(arguments), parameters), each=n$replications)
   values = list(...)
   for (i in 1:n$tests)
      if (is.expression(values[[i]])) 
         arguments[i] = values[i]

   # this no longer works in R-2.15.1 patched and R-devel
   # since rep can now only handle vector
   # tests = rep(arguments, each=n$replications)

   # so we have to copy arguments in an ugly way 
   # if replications is > 1 extend tests by
   # copying each argument entry to tests  n$replications consecutive times
   # this extends tests
   # initialize tests with arguments with everything R needs
   tests = arguments
   if (n$replications > 1) {
       z = 1
       for (k in 1:length(arguments)) {
           tests[z:(z+n$replications-1)] = arguments[k]
           z = z+n$replications } }
   result = data.frame(
      row.names=NULL,
      test=labels,
      replications=as.integer(replications),
      t(mapply(
         function(test, replications) 
            system.time(replicate(replications, { eval(test, environment); NULL })),
         tests,
         replications)))
   if ('relative' %in% columns)
      result['relative'] = local({
         timing = result[relative]
         if (min(timing) != 0) round(timing/min(timing), 3)
	     else NA })
   if (is.null(order))
      result[,columns,drop=FALSE]
   else
      result[do.call(base::order, result[order]), columns, drop=FALSE] }


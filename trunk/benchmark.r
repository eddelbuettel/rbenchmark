#!/usr/bin/r

peval = function(arg) {
   cat('*** ', deparse(substitute(arg)), ' ***', fill=TRUE)
   print(arg) }

benchmark = function(
      ..., 
      columns=c('test', 'replications', 'user.self', 'sys.self', 'elapsed', 'user.child', 'sys.child'),
      replicate=100,
      environment=parent.frame()) {
   arguments = match.call()[-1]
   parameters = names(arguments)
   if (is.null(parameters))
      parameters = as.character(arguments)
   else {
      positions = ! parameters %in% c('columns', 'replicate', 'environment')
      arguments = arguments[positions]
      parameters = parameters[positions] }
   result = cbind(
      test=rep(ifelse(parameters=='', as.character(arguments), parameters), each=length(replicate)),
      as.data.frame(
         do.call(rbind, 
            lapply(arguments, 
               function(argument)
                  do.call(rbind,
                     lapply(replicate,
                        function(count)
                           c(replications=count,
                              system.time(replicate(count, { eval(argument, environment); NULL })))))))))
   result[, columns, drop=FALSE] }

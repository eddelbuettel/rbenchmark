#!/usr/bin/r

benchmark = function(
      ..., 
      columns=c('replications', 'user.self', 'sys.self', 'elapsed', 'user.child', 'sys.child'),
      replications=100,
      environment=parent.frame()) {
   arguments = match.call()[-1]
   parameters = names(arguments)
   if (is.null(parameters))
      parameters = as.character(arguments)
   else {
      positions = ! parameters %in% c('columns', 'replications', 'environment')
      arguments = arguments[positions]
      parameters = parameters[positions] }
   result = 
      do.call(rbind, 
         lapply(arguments, 
            function(argument)
               do.call(rbind,
                  lapply(replications,
                     function(replication)
                        c(replications=replication, 
                           system.time(replicate(replication, eval(argument, environment))))))))
   rownames(result) = 
      rep(ifelse(parameters=='', as.character(arguments), parameters), each=length(replications))
   result[, columns, drop=FALSE] }

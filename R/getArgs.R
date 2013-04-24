getArgs <- function(args)
{
  if (length(args)>0)
  {
    isqgraph <- sapply(args,function(x)"qgraph"%in%class(x))
    argLists <- c(lapply(args[isqgraph],'[[','Arguments'),lapply(args[isqgraph],'[','layout'))
    args <- args[!isqgraph]
    newArgs <- lapply(argLists,getArgs)
    for (l in newArgs) args <- c(args,l[!names(l)%in%names(args)])
  }
  return(args)
}
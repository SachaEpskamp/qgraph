getArgs <- function(args)
{
  if (length(args)>0)
  {
    isqgraph <- sapply(args,function(x)"qgraph"%in%class(x))
    argLists <- args[isqgraph]
    args <- args[!isqgraph]
    newArgs <- lapply(argLists,getArgs)
    for (l in newArgs) args <- c(args,l[!names(l)%in%names(args)])
  }
  return(args)
}
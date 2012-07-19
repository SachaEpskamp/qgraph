
print.qgraph <- function(x,...)
{
	out <- cbind(x$qgraphEdgelist$from,"\t",ifelse(x$qgraphEdgelist$directed,ifelse(x$qgraphEdgelist$bidir,"<->","-->"),"---"),"\t",x$qgraphEdgelist$to,"\t",round(x$qgraphEdgelist$weight,2),"\n")
	cat(c("From\t\tTo\tWeight\n"))
	apply(out,1,cat)
}

plot.qgraph <- function(x,...) qgraph(x,...)

summary.qgraph <- function(object, ...)
{
	cat("Number of edges:\t",length(object$qgraphEdgelist$from),"\n","Number of directed edges:\t",sum(object$qgraphEdgelist$directed),"\n","Number of unique weights:\t",length(unique(object$qgraphEdgelist$weight)),"\n\n")
}
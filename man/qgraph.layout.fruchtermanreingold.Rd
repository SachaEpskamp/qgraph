\encoding{UTF8}
\name{qgraph.layout.fruchtermanreingold}
\alias{qgraph.layout.fruchtermanreingold}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph.layout.fruchtermanreingold}
\description{
This is a wrapper for the function that returns the x and y coordinates of the graph based on the Fruchterman 
Reingold algorithm (Fruchterman & Reingold, 1991), which was ported from the SNA package (Butts, 2010).
This function is used in \code{\link{qgraph}} and is not designed to be used separately. See details for
using constraints in this layout.}
\usage{
qgraph.layout.fruchtermanreingold(edgelist, weights=NULL, vcount=NULL,
	niter=NULL, max.delta=NULL, area=NULL, cool.exp=NULL, repulse.rad=NULL,
	init=NULL, groups=NULL, rotation=NULL, layout.control=0.5, constraints=NULL, 
	round = TRUE, digits = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{edgelist}{A matrix with on each row the nodes at the start and the node at the end of each edge.}
  \item{weights}{A vector containing the edge weights.}
  \item{vcount}{The number of nodes.}
  \item{niter}{Number of iterations, default is 500.}
  \item{max.delta}{Maximum displacement, default is equal to the number of nodes.}
  \item{area}{The area of the plot, default is the square of the number of nodes.}
  \item{cool.exp}{Cooling exponent, default is 1.5.}
  \item{repulse.rad}{Repulse radius, defaults to the cube of the number of nodes.}
  \item{init}{Matrix with two columns and a row for each node containing the initial X and Y positions.}
  \item{groups}{See \code{\link{qgraph}}}
  \item{rotation}{See \code{\link{qgraph}}}
  \item{layout.control}{See \code{\link{qgraph}}}
  \item{constraints}{A constraints matrix with two columns and a row for each node containing a NA if the node is free or a fixed value for one of the coordinates.}
   \item{round}{Logical indicating if the initial input should be rounded}
   \item{digits}{Number of digits to round initial input and displacement in the algorithm to. Defaults to 5. This helps prevent floating point disrepancies between different operating systems.}
}
\details{
All arguments for this function can be passed from \code{\link{qgraph}} to this function by using the 'layout.par' argument, 
which must be a list containing the arguments. This can be used to constrain the layout in two ways:
}
\section{Hard constraints}{
By using the 'constraints' argument the X and Y positions of each node can be fixed to a certain value. The 'constraint'
argument must be given a matrix with two columns and a row for each node. An NA means that that coordinate for that
node is free, and a value means it is fixed to that value.}

\section{Soft constraints}{
Soft constraining can be done by varying the 'max.delta' argument. This can be a single number, but also a 
vector containing the maximum displacement per step for each node. The default value is the number of nodes,
so by setting this to a lower value for some nodes the node won't move so much. Use this in combination with
the 'init' argument to make sure nodes don't move too much from their initial setup. This can be useful when
adding a new node to an existing network and if you don't want the network to completely change.}


\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.



Carter T. Butts <buttsc@uci.edu> (2010). sna: Tools for Social Network Analysis. R package version 2.2-0. http://CRAN.R-project.org/package=sna

Fruchterman, T. & Reingold, E. (1991). Graph drawing by force-directed placement. Software - Pract. Exp. 21, 1129?1164.
}
\author{
Sacha Epskamp (mail@sachaepskamp.com)
}
\seealso{
\code{\link{qgraph}}
}
\examples{
\dontrun{
# This example makes a multipage PDF that contains images
# Of a building network using soft constraints.

# Each step one node is added with one edge. The max.delta
# decreases the longer nodes are present in the network.

pdf("Soft Constraints.pdf",width=10,height=5)

adj=adjO=matrix(0,nrow=3,ncol=3)
adj[upper.tri(adj)]=1
Q=qgraph(adj,vsize=3,height=5,width=10,layout="spring",
	esize=1,filetype='',directed=T)
cons=Q$layout
for (i in 1:20)
{
	x=nrow(adj)
	adjN=matrix(0,nrow=x+1,ncol=x+1)
	adjN[1:x,1:x]=adj
	consN=matrix(NA,nrow=x+1,ncol=2)
	consN[1:x,]=cons[1:x,]
	layout.par=list(init=rbind(cons,c(0,0)),
	max.delta=10/(x+1):1,area=10^2,repulse.rad=10^3)
	y=sample(c(x,sample(1:(x),1)),1)
	adjN[y,x+1]=1
	Q=qgraph(adjN,Q,layout="spring",layout.par=layout.par)
	cons=Q$layout
	adj=adjN
} 
dev.off()
}
}


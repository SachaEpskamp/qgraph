\name{qgraph.animate}
\alias{qgraph.animate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Animate a growing network
}
\description{
This function is meant to facilitate the creation of animations based on growing networks. Networks are created based on the Fruchterman Reingold algorithm, which is constraint by limiting the maximum displacement of nodes that are already in the graph.
}
\usage{
qgraph.animate(input, ind = NULL, ..., constraint = 10, growth = "order", 
  titles = NULL, sleep = 0, smooth = TRUE, plotGraphs = TRUE, progress = TRUE,
  initLayout)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
A weights matrix of the graph or a list of weigths matrices with different weights of the same graph (see details). See \code{\link{qgraph}}. Edgelists are currently not supported.
}
  \item{ind}{
An object that specifies which nodes ar included or excluded. See details.
}
  \item{\dots}{
Additional arguments sent to \code{\link{qgraph}}
}
  \item{constraint}{
The constraint factor of included nodes. See details. Defaults to 10 for an soft-constrained animation. Set to Inf for a hard-constrained animation.
}
  \item{growth}{
The way nodes are added by default. Set to "order" to include nodes in the order they appear in the weigths matrix and to "degree" to include nodes based on their degree (high degree first)
}
  \item{titles}{
  Optional vector with a title for each plot
}
  \item{sleep}{
Optional value sent to Sys.sleep() for showing the animation in R
}
  \item{smooth}{
Logical. If set to \code{TRUE} smoothing via \link[stats]{loess} is performed on the layout of all frames.
}
  \item{plotGraphs}{
Logical. If set to \code{FALSE} graphs are not plotted.
}
  \item{progress}{
Logical. If set to \code{TRUE} progress bars are included.
}
  \item{initLayout}{An optional n by 2 matrix containing the initial placement of nodes in the animation.}
}
\details{
Let n be the number of nodes in total in the graph.

This function is designed to facilitate the production of animations by constraining the Fruchterman Reingold algorithm. Several frames are plotted of (a subset of) the same graph. If a node was already in the graph its maximum displacement per iteration of Fruchterman Reingold is equal to the number of nodes times the inverse of the constraint argument (so by default n/10). The higher this constraint value the stricter nodes stay in the same place between plots.

How many and which plots are made are defined by the 'input' and 'ind' arguments. There are two ways to specify the 'input' argument, either by speficying one weigths matrix or by specifying a list of weights matrices. In the sections below is explained what both of these methods do and how they are used.

This function, since it can be seen as an expression that makes several plots, works well in combination with the animation package for saving the animation to a wide variety of filetypes.
}
\section{Single weigths matrix}{
If 'input' is a single weigths matrix then in each frame a subset of the same graph is plotted. This is especially usefull for animating the growth of a network. Which nodes are in each frame is determined by the 'ind' argument.

If 'int' is not specified an animation is created in which in each frame a single node is added. This node is either in order of apearance in the weigths matrix or by its degree, which is determined with the 'growth' argument.

If 'ind' is a logical vector of length n than the first frame will contain the nodes specified with this vector and all other frames will grow in the same way as explained above (each step one node is added).

If 'ind' is a numeric vector of length n which contains all integers between 1 and n ( a single entry per node) then the first frame starts with only the node specified in the first element of the vector and in frame i the ith element is added (each step one node is added).

If 'ind' is a list with numeric vectors as elements containing integers between 1 and n then in frame i the nodes from the ith element of the list will be added. Node numbers that occur multiple times in the list are ignored (they are already added the first time).

Finally, if 'ind' is a logical matrix with n columns and an arbitrary amount of rows, then in frame i only the nodes that are TRUE in row i are included. This is the only way to specify removal of nodes.
}

\section{List of weigths matrices}{
The 'input' argument can also be given a list of weigths matrices if all these matrices have the same dimension (i.e.\ only the weights differ). If this is done than in frame i the ith weigths matrix is used. This is especially usefull for animating the change in a graph.

In this case, the 'ind' argument behaves differently. If this argument is not specified then in each frame all nodes are included.

If 'ind' is a logical vector of length n then only one plot is made with the nodes specified with that vector, and only if the length of 'input' is one.

Other methods woth in the same way as above. However, if the 'ind' argument indicates a different number of frames than the 'input' argument the function will stop and give an error.
}

\value{
Invisibly returns a list of all graphs.
}
\author{
Sacha Epskamp (mail@sachaepskamp.com)
}

\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{qgraph}}
}

\examples{
\dontrun{

## For these examples, first generate a scale free network using preferential attachment:

# Number of nodes:
n <- 100
# Empty vector with Degrees:
Degs <- rep(0, n)
# Empty Edgelist:
E <- matrix(NA, n - 1, 2)
# Add and connect nodes 1 and 2:
E[1, ] <- 1:2
Degs[1:2] <- 1
# For each node, add it with probability proportional to degree:
for (i in 2:(n - 1))
{
E[i, 2] <- i + 1
con <- sample(1:i, 1, prob = Degs[1:i]/sum(Degs[1:i]),i)
Degs[c(con,i+1)] <- Degs[c(con,i+1)] + 1
E[i, 1] <- con
}

# Because this is an edgelist we need a function to convert this to an adjacency matrix:
E2adj <- function(E,n)
{
  adj <- matrix(0,n,n)
  for (i in 1:nrow(E))
  {
      adj[E[i,1],E[i,2]]  <- 1
  }
  adj <- adj + t(adj)
  return(adj)
}



### EXAMPLE 1: Animation of construction algorithm: ###
adjs <- lapply(1:nrow(E),function(i) E2adj(E[1:i,,drop=FALSE],n))
qgraph.animate(adjs,color="black",labels=FALSE,sleep=0.1, smooth = FALSE)
rm(adjs)

### EXAMPLE 2: Add nodes by final degree: ###
adj <- E2adj(E,n)
qgraph.animate(E2adj(E,n),color="black",labels=FALSE,constraint=100,sleep=0.1)

### EXAMPLE 3: Changing edge weights: ###
adjW <- adj*rnorm(n^2)
adjW <- (adjW + t(adjW))/2
adjs <- list(adjW)
for (i in 2:100)
{
  adjW <- adj*rnorm(n^2)
  adjW <- (adjW + t(adjW))/2
  adjs[[i]] <- adjs[[i-1]] + adjW
}
qgraph.animate(adjs,color="black",labels=FALSE,constraint=100,sleep=0.1)


}
}


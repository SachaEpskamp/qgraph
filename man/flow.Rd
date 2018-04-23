\name{flow}
\alias{flow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draws network as a flow diagram showing how one node is connected to all other nodes
}
\description{
This function will draw one node of interest on the left, then subsequently draw all other nodes in vertical levels to the right, in the order of direct (unweighted) connectiveness to the node of interest. Layout is based on the \code{layout_as_tree} function from the igraph package. This allows one to see how one node connects to other nodes in the network.
}
\usage{
flow(object, from, horizontal = TRUE, equalize = TRUE, minCurve = 1, maxCurve = 4, 
      unfadeFirst = FALSE, fade = TRUE, labels, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A qgraph object
}
  \item{from}{
Integer or character indicating the (label of the) node of interest.
}
  \item{horizontal}{
Logical, should the flow diagram be plotted horizontally or vertically
}
  \item{equalize}{
Logical, should the placement of nodes be equalized per level.
}
  \item{minCurve}{
Minimum curve of edges on the same level
}
  \item{maxCurve}{
Maximum curve of edges on the same level
}
  \item{unfadeFirst}{
Logical, should edges between the node of interest be unfaded?
}
  \item{fade}{
'fade' argument as used in \code{\link{qgraph}}
}
\item{labels}{
'labels' argument as used in \code{\link{qgraph}}
}
  \item{\dots}{
Arguments sent to qgraph
}
}

\author{
Sacha Epskamp
}

\examples{
\dontrun{
# Load data:
library("psych")
data(bfi)

# Compute polychoric correlations:
corMat <- cor_auto(bfi[,1:25])

# Glasso network:
g2 <- qgraph(corMat,  cut = 0, graph = "glasso", sampleSize = nrow(bfi),
             threshold = TRUE)

# Flow from A2:
flow(g2, "A2", horizontal = TRUE)
}
}
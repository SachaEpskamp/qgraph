\name{smallworldIndex}
\alias{smallworldIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Small-world index of unweighted graph
}
\description{
Computes the small-world index of an unweighted graph. When the graph is weighted, weights are removed and every nonzero edge weight is set to 1.
}
\usage{
smallworldIndex(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A qgraph or igraph object.
}
}
\value{
A list containing:
\item{transitivity}{The transitivity of the network}
\item{transitivity_random}{The expected transitivity of a random network of the same size and density}
\item{APL}{The average shortest path length of the network}
\item{APL_random}{The expected average shortest path length of a random network of the same size and density}
\item{index}{The small-world index}
}
\references{
Watts, D. J., & Strogatz, S. H. (1998). Collective dynamics of 'small-world' networks. Nature, 393(6684), 440-442.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}


\name{qgraphMixed}
\alias{qgraphMixed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a mixed graph with both directed and undirected edges.
}
\description{
This function can be used to plot a network in which each node is connected by at most 3 edges; one undirected edge and two directed edges.
}
\usage{
qgraphMixed(undirected, directed, parallel = TRUE, parallelAngle = pi/6, 
        diagUndirected = FALSE, diagDirected = TRUE, ltyUndirected = 1, ltyDirected = 1, 
        curve = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{undirected}{
The undirected network weights matrix.
}
  \item{directed}{
The directed network weights matrix.
}
  \item{parallel}{
Logical indicating if edges should be plotted parallel or curved.
}
  \item{parallelAngle}{
See \code{\link{qgraph}}
}
  \item{diagUndirected}{
Logical indicating if the diagonal of the undirected edges should be included.
}
  \item{diagDirected}{
Logical indicating if the diagonal of the directed edges should be included.
}
  \item{ltyUndirected}{
lty of undirected edges
}
  \item{ltyDirected}{
lty of directed edges
}
  \item{curve}{
Curvature of directed edges
}
  \item{\dots}{
Arguments sent to \code{\link{qgraph}}
}
}

\author{
Sacha Epskamp <mail@sachaepskamp.com>
}


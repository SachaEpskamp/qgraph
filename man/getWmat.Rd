\name{getWmat}
\alias{getWmat}
\alias{getWmat.matrix}
\alias{getWmat.data.frame}
\alias{getWmat.igraph}
\alias{getWmat.qgraph}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain a weights matrix
}
\description{
This function extracts a weights matrix from various kinds of objects.
}
\usage{
\method{getWmat}{matrix}(x,nNodes,labels, directed = TRUE,\dots)
\method{getWmat}{data.frame}(x,nNodes,labels, directed = TRUE,\dots)
\method{getWmat}{igraph}(x,labels,\dots)
\method{getWmat}{qgraph}(x,directed,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An input object
}
  \item{nNodes}{
Number of Nodes
}
  \item{labels}{
A vector specifying the labels of each node
}
  \item{directed}{
Logical indicating if the graph should be directed
}
  \item{\dots}{
Ignored
}
}

\value{
A weights matrix
}

\author{
Sacha Epskamp <mail@sachaepskamp.com>
}


\name{centclustFuns}
\alias{centralityTable}
\alias{centralityPlot}
\alias{clusteringTable}
\alias{clusteringPlot}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes long format tables and plots centrality measures and clustering coefficients
}
\description{
These functions use \code{\link[qgraph]{centrality_auto}} and \code{\link[qgraph]{clustering_auto}} to compute the centrality measures and clustering coefficients of several graphs and returns them in a long format data frame or a \code{\link[ggplot2]{ggplot}} 
}
\usage{
centralityTable(\dots, labels, relative = TRUE)
clusteringTable(\dots, labels, relative = TRUE)
centralityPlot(\dots, labels, relative = TRUE)
clusteringPlot(\dots, labels, relative = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Graph objects that have \code{getWmat} methods.
}
  \item{labels}{
An optional string overwriting the labels
}
  \item{relative}{
Logical, should the measures be scaled to the highest absolute measure?
}
}
\value{
A \code{data.frame} for \code{centralityTable} and \code{clusteringTable} and a \code{\link[ggplot2]{ggplot}} object for the plotting functions
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

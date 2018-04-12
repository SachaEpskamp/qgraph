\name{mutualInformation}
\alias{mutualInformation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes the mutual information between nodes
}
\description{
Computes the mutual information from one node to all other nodes, or between sets of nodes.
}
\usage{
mutualInformation(ggm, from, to = "all", covMat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ggm}{
Partial correlation network. Can be missing if 'covMat' is supplied.
}
  \item{from}{
Integer vector corresponding to one set of nodes. Defaults to all nodes.
}
  \item{to}{
Integer vector corresponding to another set of nodes, or \code{'all'} to compute the mutual information of each node to all other nodes.
}
  \item{covMat}{
Variance-covariance matrix. Can be missing if 'ggm' is supplied.
}
}

\author{
Sacha Epskamp
}

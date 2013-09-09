\name{qgraph.gui}
\alias{qgraph.gui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical user interface for qgraph
}
\description{
THIS FUNCTION HAS TEMPORARILY BEEN REMOVED FROM THE PACKAGE.
%This function works the same as \code{\link{qgraph}} except that a graphical user interface is opened allowing the user to %change parameters.
}
\usage{
qgraph.gui(input, corMat, ...)
}

\arguments{
  \item{input}{
%Input, see \code{\link{qgraph}}
}
  \item{corMat}{
%Logical, is 'input' a correlation or covariance matrix. If this is TRUE a different GUI window is opened with more options. By default this is TRUE only if 'input' is a symmetrical matrix with all absolute values less or equal than 1 and all diagonal elements equal to 1. If a covariance matrix is used for 'input' and this argument is set to TRUE the plots are made using a `cov2cor` call, but the covariance matrix is used in computing EFA and PCA results.
}
  \item{\dots}{
%Arguments passed to \code{\link{qgraph}}
}
}

\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

\seealso{
\code{\link{qgraph}}
}
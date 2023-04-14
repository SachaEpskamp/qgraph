\name{as.igraph.qgraph}
\alias{as.igraph.qgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts qgraph object to igraph object.
}
\description{
This function converts the output of \code{\link{qgraph}} to an 'igraph' object that can be used in the igraph package (Csardi & Nepusz, 2006)
}
\usage{
\method{as.igraph}{qgraph}(x, ..., attributes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{"qgraph"} object
}
\item{\dots}{Not used.}
  \item{attributes}{
Logical, should graphical attributes also be transferred?
}
}

\references{
 Csardi G, Nepusz T (2006). The igraph software package for complex network research, InterJournal, Complex Systems 1695. http://igraph.sf.net
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

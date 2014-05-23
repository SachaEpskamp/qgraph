\name{averageLayout}
\alias{averageLayout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes an average layout over several graphs
}
\description{
This function can be used to compute a joint layout over several graphs.
}
\usage{
averageLayout(..., layout = "spring", layout.par = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Multiple graph objects such as \code{qgraph} objects or weights matrices.
}
  \item{layout}{
Same as in \code{\link{qgraph}}
}
  \item{layout.par}{
Same as in \code{\link{qgraph}}
}
}

\value{
A layout matrix
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}


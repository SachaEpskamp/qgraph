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
A qgraph object.
}
}
\references{
Watts, D. J., & Strogatz, S. H. (1998). Collective dynamics of 'small-world' networks. nature, 393(6684), 440-442.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}


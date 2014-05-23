\name{centrality and clustering plots}
\alias{centralityPlot}
\alias{centralityTable}
\alias{clusteringPlot}
\alias{clusteringTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Centrality and Clustering plots and tables
}
\description{
These functions can be used to facilitate interpreting centrality and clustering coefficients. The plot functions use ggplot2 (Wickham, 2009). The table functions create a long format table which can easilly be plotted in ggplot2.
}
\usage{
centralityPlot(..., labels, relative = TRUE, include)
clusteringPlot(..., labels, relative = TRUE,  include, signed = FALSE)
centralityTable(..., labels, relative = TRUE)
clusteringTable(..., labels, relative = TRUE, signed = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Objects usuable in the \code{\link{getWmat}} generic, such as qgraph objects and weights matrices. Can also be lists containing these objects. Graphs in a list will be plotted in the same panel as different lines and graphs in seperate arguments will be plotted in seperate panels.
}
  \item{labels}{
A vector overwriting the labels used
}
  \item{relative}{
Logical, should all measures be divided by the largest measure?
}
  \item{include}{
A vector of measures to include. if missing all measures available will be included.
}
\item{signed}{
Logical indicating if signed clustering coefficients should be plotted.
}
}

\references{
H. Wickham. ggplot2: elegant graphics for data
analysis. Springer New York, 2009.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com> \& Jolanda Kossakowski
}

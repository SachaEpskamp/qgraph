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
centralityPlot(..., labels, standardized = TRUE, relative = FALSE, include, 
                theme_bw = TRUE, print = TRUE)
clusteringPlot(..., labels,  standardized = TRUE, relative = FALSE,  include, 
                signed = FALSE, theme_bw = TRUE, print = TRUE)
centralityTable(..., labels, standardized = TRUE,  relative = FALSE)
clusteringTable(..., labels,  standardized = TRUE, relative = FALSE, 
                signed = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Objects usuable in the \code{\link{getWmat}} generic, such as qgraph objects and weights matrices. Can also be lists containing these objects. Graphs in a list will be plotted in the same panel as different lines and graphs in seperate arguments will be plotted in seperate panels.
}
  \item{labels}{
A vector overwriting the labels used
}
  \item{standardized}{
Logical, should all measures be standardized?
}
  \item{relative}{
Logical, should all measures be scaled relative to the largest value?
}
  \item{include}{
A vector of measures to include. if missing all measures available will be included.
}
\item{signed}{
Logical indicating if signed clustering coefficients should be plotted.
}
\item{theme_bw}{
Adds the ggplot2 black and white theme to the plot
}
\item{print}{
  If \code{TRUE}, the plot is sent to the print command and returned invisible, if \code{FALSE} the plot is returned normally. Needed to include plots in e.g., pdf files.
}
}

\references{
H. Wickham. ggplot2: elegant graphics for data
analysis. Springer New York, 2009.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com> \& Jolanda Kossakowski
}

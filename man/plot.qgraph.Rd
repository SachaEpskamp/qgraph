\name{plot.qgraph}
\alias{plot.qgraph}

\title{
Plot method for "qgraph"
}
\description{
Plots an object created by \code{\link{qgraph}}.
}
\usage{
\method{plot}{qgraph}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A "qgraph" object
}
  \item{\dots}{
Not used
}
}

\details{
If the result of \code{\link{qgraph}} is stored, such as \code{Graph <- qgraph(...)}, the plot can be recreated in two ways. \code{qgraph(Graph, ...))} reruns \code{qgraph} with the same arguments used in the origina call except those restated in the dots. For example \code{qgraph(Graph, shape = "square")} will recreate the same plot but now use square nodes instead of circular. \code{plot(Graph)} will NOT rerun \code{qgraph} but simply plot the qgraph object. This means that now specific graph attributes can be changed before plotting.

More specific, \code{qgraph(Graph)} will base the new plot on the \code{Arguments} element of the \code{qgraph} object and \code{plot(qgraph)} will base the new plot on the \code{graphAttributes} element of qgraph.

}

\author{
Sacha Epskamp (mail@sachaepskamp.com)
}

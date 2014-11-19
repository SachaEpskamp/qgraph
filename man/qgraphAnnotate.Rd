\name{qgraphAnnotate}
\alias{qgraphAnnotate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Annotates a qgraph object with mouseover tooltips in a HTML file
}
\description{
This function creates a HTML file based on the result from a \code{\link{qgraph}} call. The HTML file is made using \code{\link[sendplot]{sendplot}}
}
\usage{
qgraphAnnotate(graph, ..., fromqgraph = c("labels", "nodeNames", "tooltips", "groups"), 
                filename = "qgraph", image.size = "600x600", window.size = image.size, 
                legend = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
Result from \code{\link{qgraph}}
}
  \item{\dots}{
Named vectors with elements to send to the tooltip. The length of each vector should equal the amount of nodes.
}
  \item{fromqgraph}{
Vector specifying which arguments from \code{\link{qgraph}} should be used in the tooltips. Only supported for \code{"labels"}, \code{"nodeNames"}, \code{"tooltips"} and \code{"groups"}
}
  \item{filename}{
String indicating the root name of the file
}
  \item{image.size}{
String indicating the size of the file in pixels.
}
  \item{window.size}{
String indicating the size of the image window in pixels.
}
  \item{legend}{
Logical, should a legend be plotted? Defaults to \code{FALSE}
}
}
\details{
This function uses the sendplot package (Gaile, Shepherd, Sucheston, Bruno and Manly, 2013).
}
\value{
A string indicating the location of the created \code{HTML} file, which can be used with \code{browseURL}.
}
\references{
Daniel P Gaile, Lori A. Shepherd, Lara Sucheston, Andrew Bruno and Kenneth F. Manly (2013). sendplot: Tool for sending interactive plots with tool-tip content.. R package version 4.0.0. http://CRAN.R-project.org/package=sendplot
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

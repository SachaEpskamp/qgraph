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
These functions can be used to facilitate interpreting centrality and clustering coefficients. The plot functions use ggplot2 (Wickham, 2009). The table functions create a long format table which can easily be plotted in ggplot2.
}
\usage{
centralityPlot(..., labels, scale = c("raw0", "raw", "z-scores", "relative"),
                 include =c("Degree","Strength","OutDegree","InDegree","OutStrength",
                 "InStrength"), theme_bw = TRUE, print = TRUE, verbose = TRUE,
                 standardized, relative, weighted = TRUE,signed = TRUE,
                 orderBy = "default", decreasing = FALSE, communities = NULL,
                 useCommunities = "all")
clusteringPlot(..., scale = c("raw0", "raw", "z-scores", "relative"), labels,
                  include , signed = FALSE, theme_bw = TRUE, print = TRUE,
                  verbose = TRUE, standardized, relative,orderBy = "default",
                  decreasing = FALSE)
centralityTable(..., labels, standardized = TRUE,  relative = FALSE, weighted =
                 TRUE, signed = TRUE, communities = NULL, useCommunities = "all")
clusteringTable(..., labels,  standardized = TRUE, relative = FALSE,
                signed = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Objects usable in the \code{\link{getWmat}} generic, such as qgraph objects and weights matrices. Can also be lists containing these objects. Graphs in a list will be plotted in the same panel as different lines and graphs in separate arguments will be plotted in separate panels.
}
\item{scale}{Scale of the x-axis. \code{"z-scores"} to plot standardized coefficients, \code{"raw"} to plot raw coefficients, \code{"raw0"} to plot raw coefficients while including 0 on the x-axis and  \code{"relative"} to show values on a relative scale from 0 (lowest) to 1 (highest).}
  \item{labels}{
A vector overwriting the labels used. Can be missing.
}
  \item{include}{
A vector of measures to include. If missing, \code{centralityPlot} includes the default set of measures shown in the usage section, while \code{clusteringPlot} includes all available measures. Not included by default in \code{centralityPlot} are \code{"Closeness"}, \code{"Betweenness"}, \code{"ExpectedInfluence"}, \code{"OutExpectedInfluence"}, and \code{"InExpectedInfluence"}. Can also be \code{"all"} or \code{"All"} to include all available centrality measures. When \code{communities} is supplied, the bridge centrality measures \code{"Bridge Strength"}, \code{"Bridge Indegree"} and \code{"Bridge Outdegree"} are included by default as well, and \code{"Bridge Betweenness"}, \code{"Bridge Closeness"}, \code{"Bridge Expected Influence (1-step)"} and \code{"Bridge Expected Influence (2-step)"} can be included by hand or via \code{"all"}.
}
\item{theme_bw}{
Adds the ggplot2 black and white theme to the plot.
}
\item{print}{
  If \code{TRUE}, the plot is sent to the print command and returned invisible, if \code{FALSE} the plot is returned normally. Needed to include plots in e.g., pdf files.
}
\item{verbose}{
Should messages be printed to the console?
}
  \item{standardized}{
Logical, should all measures be standardized? Argument is deprecated and will be removed.
}
  \item{relative}{
Logical, should all measures be scaled relative to the largest value? Argument is deprecated and will be removed.
}
\item{weighted}{Logical, set to \code{FALSE} to set all edge weights to 1 or -1}
\item{signed}{Logical, set to \code{FALSE} to make all edge weights absolute}
\item{orderBy}{String indicating which measure to order by. Can be \code{"default"} (nodes ordered by natural sort of their labels), or one of the measures plotted (e.g., \code{"Strength"})}
\item{decreasing}{Logical indicating if the nodes should be ordered increasing or decreasing}
\item{communities}{A community assignment of each node (see \code{\link{bridgeCentrality}} for supported formats). When supplied, the bridge centrality statistics of Jones, Ma and McNally (2021) are computed and included in the table/plot (only in \code{centralityPlot} and \code{centralityTable}). Defaults to \code{NULL}, in which case no bridge centrality statistics are computed.}
\item{useCommunities}{A character vector specifying which communities should be included in the computation of bridge centrality. Defaults to \code{"all"}. See \code{\link{bridgeCentrality}}.}
}

\value{
\code{centralityPlot} and \code{clusteringPlot} return a ggplot2 plot object (invisibly if \code{print = TRUE}). \code{centralityTable} and \code{clusteringTable} return a long format data frame with the columns \code{graph}, \code{type}, \code{node}, \code{measure} and \code{value}.
}

\references{
H. Wickham. ggplot2: elegant graphics for data
analysis. Springer New York, 2009.

Jones, P. J., Ma, R., & McNally, R. J. (2021). Bridge centrality: A network approach to understanding comorbidity. Multivariate Behavioral Research, 56(2), 353-367.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com> and Jolanda Kossakowski
}
\seealso{
\code{\link{bridgeCentrality}}, \code{\link{centrality_auto}}
}

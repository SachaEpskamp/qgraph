\name{bridgeCentrality}
\alias{bridgeCentrality}
\alias{print.bridgeCentrality}
\title{
Bridge centrality statistics
}
\description{
Computes the bridge centrality statistics of Jones, Ma and McNally (2021): bridge strength, bridge betweenness, bridge closeness, and bridge expected influence (1-step and 2-step), given a network and a prespecified set of communities. Bridge centrality statistics aim to identify nodes that are important in the connectivity \emph{between} communities in a network.

Bridge centrality was developed by Payton J. Jones and first implemented in the \code{bridge} function of the \code{networktools} package. This function is an independent implementation of the same statistics, designed to give identical results, so that bridge centrality can be plotted with \code{\link{centralityPlot}} in the same style as the other centrality measures (see the \code{communities} argument of \code{\link{centralityPlot}}).
}
\usage{
bridgeCentrality(graph, communities, useCommunities = "all", directed,
                 normalize = FALSE, labels)
}
\arguments{
  \item{graph}{
A network, in any object usable by the \code{\link{getWmat}} generic, such as a \code{\link{qgraph}} object, a weights matrix, or an \code{igraph} object.
}
  \item{communities}{
The community assignment of each node. Can be a character, factor or numeric vector with one element per node (e.g., \code{c("A", "A", "B", "B")}); a named list with node indices per community (e.g., \code{list(A = 1:2, B = 3:4)}, the same format as the \code{groups} argument of \code{\link{qgraph}}); or an igraph \code{communities} object. If \code{graph} is a \code{qgraph} object that was created using the \code{groups} argument, \code{communities} may be omitted and the groups are used instead.
}
  \item{useCommunities}{
A character vector specifying which communities should be included in the computation. Defaults to \code{"all"}. If a subset of communities is used, statistics of nodes outside these communities are returned as \code{NA}.
}
  \item{directed}{
Logical, is the network directed? If omitted, directedness is detected from the symmetry of the weights matrix.
}
  \item{normalize}{
Logical, should the bridge centrality statistics be divided by their highest possible value (assuming max edge strength = 1), in order to normalize for different community sizes? Defaults to \code{FALSE}.
}
  \item{labels}{
A vector overwriting the node labels. Can be missing.
}
}
\details{
Bridge strength is the sum of the absolute weights of all edges connecting a node to nodes in other communities. In directed networks, bridge strength is the sum of bridge in-degree and bridge out-degree, which are returned as well.

Bridge betweenness is the number of times a node lies on the shortest path between two nodes from different communities.

Bridge closeness is the inverse of the average shortest path length between a node and all nodes outside its community.

Bridge expected influence (1-step) is the sum of the signed weights of all edges connecting a node to nodes in other communities. In directed networks, only outgoing edges are considered. Bridge expected influence (2-step) additionally considers the indirect influence on other communities through intermediate nodes, weighted by the connecting edge weight.

Bridge betweenness and bridge closeness are only defined for positive edge weights; negative edges, if present, are excluded from the computation of these two statistics. Bridge expected influence is recommended when a network contains negative edges. Self-loops are not included in the computation.

Shortest paths are computed using distances equal to the inverse of the absolute edge weights, as in \code{\link{centrality}}.
}
\value{
A list of class \code{"bridgeCentrality"} containing a named vector per statistic. For undirected networks: \code{"Bridge Strength"}, \code{"Bridge Betweenness"}, \code{"Bridge Closeness"}, \code{"Bridge Expected Influence (1-step)"} and \code{"Bridge Expected Influence (2-step)"}. For directed networks, \code{"Bridge Indegree"} and \code{"Bridge Outdegree"} are included as well. The element \code{communities} contains the community assignment of each node in vector form.
}
\references{
Jones, P. J., Ma, R., & McNally, R. J. (2021). Bridge centrality: A network approach to understanding comorbidity. Multivariate Behavioral Research, 56(2), 353-367.

Jones, P. J. (2017). networktools: Tools for Identifying Important Nodes in Networks. R package. https://CRAN.R-project.org/package=networktools
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>; bridge centrality was developed and first implemented in the \code{networktools} package by Payton J. Jones.
}
\seealso{
\code{\link{centralityPlot}}, \code{\link{centralityTable}}, \code{\link{centrality_auto}}
}
\examples{
set.seed(1)
data(big5)
\dontrun{
# Correlation network of 10 big 5 items, with two communities:
items <- c(1:5, 11:15)
W <- cor(big5[, items])
communities <- rep(c("N", "E"), each = 5)
bridgeCentrality(W, communities)

# Communities are taken from the groups of a qgraph object:
g <- qgraph(W, groups = list(N = 1:5, E = 6:10), DoNotPlot = TRUE)
bridgeCentrality(g)

# Plot bridge centrality with centralityPlot:
centralityPlot(W, communities = communities)
}
}
\keyword{ graphs }
\keyword{ centrality }

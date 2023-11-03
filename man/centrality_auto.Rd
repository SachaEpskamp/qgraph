\encoding{UTF-8}
\name{centrality_auto}
\alias{centrality_auto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automatic centrality statistics of graphs
}
\description{
This function can be used on several kinds of graphs to compute several node centrality statistics and the edge-betweenness centrality. The input graph can be an adjacency matrix, a weight matrix, an edgelist (weighted or unweighted), a \code{\link{qgraph}} object or an \code{\link[igraph]{igraph}} object.
}
\usage{
centrality_auto(x, weighted = TRUE, signed = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A graph. Can be a \code{qgraph} object, an \code{igraph} object, an adjacency matrix, a weight matrix and an edgelist, or a weighted edgelist.
}
\item{weighted}{Logical, set to \code{FALSE} to set all edge weights to 1 or -1}
\item{signed}{Logical, set to \code{FALSE} to make all edge weights absolute}
}
\details{
The function recognizes whether the network is unweighted vs. weighted, undirected vs. directed, and connected vs. disconnected and computes a set of centrality indices that is best suited for that particular kind of network. Edge signs are always disregarded, while edge weights and directions, if present, are considered in the computation of the indices. If the network is disconnected, closeness centrality is computed only considering the largest component (notice that this is different from what function \code{\link{centrality}} does).

If \code{x} is unweighted and directed, then the indegree, the outdegree, the node betweenness centrality, the closenes centrality, and the edge betweenness centrality are computed.
If \code{x} is unweighted and undirected, then the degree, the node betweenness centrality, the closenes centrality, and the edge betweenness centralities are computed.
If \code{x} is weighted and directed, then the instrength and the outstrength (same as indegree and outdegree, but considering weights), the node betweenness centrality, the closeness centrality, and edge betweenness centralities are computed
If \code{x} is weighted and undirected, then the strength, the node betweenness centrality, the closenes centrality, and edge betweenness centralities are computed.
Additionally, the shortest path length between each pair of nodes is also computed for all the kinds of networks.
}
\value{
A list containing:
\item{node.centrality }{A dataframe that includes the node centrality statistics. A subset of the following centrality indices is included, depending on the input network: \code{Degree}, \code{InDegree}, \code{OutDegree}, \code{Strength}, \code{InStrength}, \code{OutStrength}, \code{Betweenness}, and \code{Closeness}.}
\item{ShortestPathLengths }{A matrix containing the shortest path lengths of each pairs of nodes. These path lenghts are based on the inverse of the absolute edge weights.}
\item{edge.betweenness.centrality}{The edge betweenness centrality statistic (Newman & Girvan, 2004). Edges are ordered by their decreasing centrality.}
}

\references{
Newman, M. E. J., Girvan, M. (2004). Finding and evaluating community structure in networks. Phisical Review E 69(026113).

Costantini, G., Epskamp, S., Borsboom, D., Perugini, M., Mõttus, R., Waldorp, L., Cramer, A. O. J., State of the aRt personality research: A tutorial on network analysis of personality data in R. Manuscript submitted for publication.
}
\author{
Giulio Costantini (giulio.costantini@unimib.it), Sacha Epskamp (mail@sachaepskamp.com)
}

\seealso{
\code{\link{qgraph}}, \code{\link{centrality}}
}
\examples{
set.seed(1)
adj <- matrix(sample(0:1,10^2,TRUE,prob=c(0.8,0.2)),nrow=10,ncol=10)
Q <- qgraph(adj)
centrality_auto(Q) 
# notice that a value NA is returned for the closeness centrality of nodes 3 and 9, which are not 
# strongly connected to the largest component of the network (3 cannot reach other nodes, 9 cannot 
# be reached).
}

\keyword{ graphs }
\keyword{ centrality }

\name{clustcoef_auto}
\alias{clustcoef_auto}
\alias{clustWS}
\alias{clustZhang}
\alias{clustOnnela}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Local clustering coefficients.
}
\description{
Compute local clustering coefficients, both signed and unsigned and both for weighted and for unweighted networks.
}
\usage{
clustcoef_auto(x, thresholdWS = 0, thresholdON = 0)
clustWS(x, thresholdWS=0)
clustZhang(x)
clustOnnela(x, thresholdON=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An undirected graph. Can be a \code{qgraph} object, an \code{igraph} object, an adjacency matrix, a weight matrix and an edgelist, or a weighted edgelist.}
  \item{thresholdWS}{
The threshold used to binarize a weighted network \code{x} to compute the binary clustering coefficients \code{clustWS} and \code{signed_clustWS}. Edges with weights lower than \code{thresholdWS} in absolute value are zeroed. For unweighted networks, \code{thresholdWS = 0} is the suggested value.
}
  \item{thresholdON}{
In the computation of Onnela's clustering coefficient \code{clustOnnela}, edge of weights lower than \code{thresholdON} in absolute value are excluded. The value \code{thresholdON = 0} (i.e., no edge is excluded) is generally suggested also for weighted networks.
}
}
\details{
\code{clustWS} computes the clustering coefficient for unweighted networks introduced by Watts & Strogatz (1998) and the corresponding signed version (Costantini & Perugini, in press).
\code{ClustZhang} computes the clustering coefficient for weighted networks introduced by Zhang & Horvath (2005) and the corresponding signed version (Costantini & Perugini, in press).
\code{clustOnnela} computes the clustering coefficient for weighted networks introduced by Onnela et al. (2005) and the corresponding signed version (Costantini & Perugini, in press).
\code{clustering_auto} automatically recognizes the kind of the input network \code{x} (weighted vs. unweighted, signed vs. unsigned) and computes a subset of indices according to the kind of the network: signed indices are not computed for unsigned networks and weighted indices are not computed for unweighted networks. However the unsigned indices are computed for signed networks, by considering the absolute value of the weights, and the unweighted indices are computed for weighted networks, after a binarization according to the parameter \code{thresholdWS}. \code{clustering_auto} computes also the weighted clustering coefficient by Barrat et al. (2004), relying on function \code{\link[igraph]{transitivity}} from package \code{\link[igraph]{igraph}}.
For the computation of the local clustering coefficient, a node must have at least two neighbors: for nodes with less than two neighbors \code{NaN} is returned.
}
\value{A dataframe that includes one or more of the following indices.
\item{clustWS}{The Watts & Strogatz's (1998) unweighted clustering coefficient}
\item{signed_clustWS}{The signed version of the Watts & Strogatz's clustering coefficient}
\item{clustZhang}{The Zhang & Horvath's (2005) weighted clustering coefficient}
\item{signed_clustZhang}{The signed version of the Zhang & Horvath's (2005) clustering coefficient}
\item{clustOnnela}{The Onnela et al.'s (2005) clustering coefficient}
\item{signed_clustOnnela}{The signed version of the Onnela et al.'s (2005) clustering coefficient}
\item{clustBarrat}{The Barrat et al.'s (2004) weighted clustering coefficient}
}
\references{
Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A. (2004). The architecture of complex weighted networks. In Proc. Natl. Acad. Sci. USA 101 (pp. 3747-3752).

Costantini, G., Perugini, M. (in press), Generalization of Clustering Coefficients to Signed Correlation Networks

Langfelder, P., & Horvath, S. (2008). WGCNA: an R package for weighted correlation network analysis. BMC Bioinformatics, 9, 559.

Onnela, J. P., Saramaki, J., Kertesz, J., & Kaski, K. (2005). Intensity and coherence of motifs in weighted complex networks. Physical Review E, 71(6), 065103.

Watts, D. J., & Strogatz, S. H. (1998). Collective dynamics of "small-world" networks. Nature, 393(6684), 440-442.

Zhang, B., & Horvath, S. (2005). A general framework for weighted gene co-expression network analysis. Statistical Applications in Genetics and Molecular Biology, 4(1).
}
\author{
Giulio Costantini (giulio.costantini@unimib.it), Sacha Epskamp (mail@sachaepskamp.com)
}
\note{
Part of the code has been adapted from package \code{WGCNA} (Langfelder & Horvath, 2008).
}

\section{Warning}{
The function requires an undirected network. To convert a directed network to undirected use for instance function \code{\link{upper.tri}} (see examples).
}
\seealso{
\code{\link{centrality_auto}}
}
\examples{
set.seed(1)
# generate a random (directed) network:
net_ig <- igraph::erdos.renyi.game(n=8, p.or.m=.4, type="gnp", directed=TRUE) 

# convert it to an adjacency matrix:
net <- as.matrix(igraph:::get.adjacency(net_ig, type="both")) 

# convert it to a signed and weighted network:
net <- net*matrix(rnorm(ncol(net)^2), ncol=ncol(net)) 

# make it undirected:
net[upper.tri(net)] <- t(net)[upper.tri(net)] 
clustcoef_auto(net)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{signed}
\keyword{weighted}
\keyword{clustering}% __ONLY ONE__ keyword per line

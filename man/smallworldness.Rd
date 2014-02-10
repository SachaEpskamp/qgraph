\name{smallworldness}
\alias{smallworldness}
\encoding{UTF8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the small-worldness index.
}
\description{
Compute the small-worldness index (Humphries \& Gurney, 2008) relying on the global transitity of the network (Newman, 2003).
}
\usage{
smallworldness(x, B = 1000, up = 0.995, lo = 0.005)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A graph. Can be a \code{qgraph} object object, an \code{igraph} object, an adjacency matrix, a weight matrix and an edgelist (possibly weighted).
}
  \item{B}{
The number of random networks.
}
  \item{up}{
The upper quantile.
}
  \item{lo}{
the lower quantile.
}
}
\details{
The function computes the transitivity of the target network (\code{trans_target}) and the average shortest path length (\code{avlength_target}). Then it computes the average of the same indices on B random networks (\code{mean_trans_random} and \code{mean_avlength_rnd}). The small-worldness is then computed as \code{(clusttrg/clustrnd_m)/(lengthtrg/lengthrnd_m)}. The  \code{lo} and \code{up} quantiles of the distribution of the random networks are also returned for both the transitivity and the average shortest path length.

A network can be said "smallworld" if \code{smallworldness} is higher than one (a stricter rule is \code{smallworldness}>=3; Humphries & Gurney, 2008). To consider a network as "smallworld", it is also suggested to inspect that the network has a transitivity substantially higher than comparable random networks (i.e., \code{trans_target} >> \code{mean_trans_random}) and that the average shortest path length is similar or higher than that computed on random networks (i.e., \code{avlength_target} >= \code{mean_avlength_rnd}).
Edge weights, signs and directions are ignored in the computation of the indices.
}
\value{
\item{smallworldness}{the "small-worldness" index proposed by Humphries & Gurney (2008)}
\item{trans_target}{the global transitivity (Newman, 2003)}
\item{mean_trans_random}{the average of the transitivities of the random networks}
\item{loCI_trans_random}{the \code{lo} quantile of the distribution of the transitivity in the B random networks}
\item{upCI_trans_random}{the \code{up} quantile of the distribution of the transitivity in the B random networks}
\item{avlength_target}{the mean length of the shortest paths in the target network}
\item{avlength_target}{the mean length of the shortest paths in the target network}
\item{mean_avlength_rnd}{the average of the mean shortest path lengths in the random networks}
\item{loCI_avlength_rnd}{the \code{lo} quantile of the distribution of the mean shortest path lengths in the random networks}
\item{upCI_avlength_rnd}{the \code{up} quantile of the distribution of the mean shortest path lengths in the random networks}
}
\references{
Costantini, G., Epskamp, S., Borsboom, D., Perugini, M., Mottus, R., Waldorp, L., Cramer, A. O. J., State of the aRt personality research: A tutorial on network analysis of personality data in R. Manuscript submitted for publication.
Humphries, M. D., & Gurney, K. (2008). Network "small-world-ness": a quantitative method for determining canonical network equivalence. PloS One, 3(4), e0002051.
Newman, M. E. J. (2003). The structure and function of complex networks*. SIAM Review, 45(3), 167–256.
}
\author{
Giulio Costantini (giulio.costantini@unimib.it), Sacha Epskamp (mail@sachaepskamp.com)
}
\note{
If a directed network is given as input, an edge between every two nodes i and j is considered present if there is an arrow either from i to j or from j to i.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
set.seed(1)
# an almost regular lattice. The average length is much higher than random networks.
regnet<-igraph::watts.strogatz.game(dim=1, size=300, nei=3, p=.0001, loops=FALSE, multiple=FALSE)
smallworldness(regnet, B=100)
# a small-world network: the transitivity is much higher than random, the average path length is somehow similar to that of random networks
swnet<-igraph::watts.strogatz.game(dim=1, size=300, nei=3, p=.05, loops=FALSE, multiple=FALSE)
smallworldness(swnet, B=100)
# a pseudorandom network: both the average path length and the transitivity are similar to random networks.
rndnet<-igraph::watts.strogatz.game(dim=1, size=300, nei=3, p=.95, loops=FALSE, multiple=FALSE)
smallworldness(rndnet, B=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smallworld}
\keyword{transitivity}
\name{smallworldness}
\alias{smallworldness}
\encoding{UTF8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the small-worldness index.
}
\description{
Compute the small-worldness index (Humphries & Gurney, 2008) relying on the global transitity of the network (Newman, 2003) and on its average shortest path length.
}
\usage{
smallworldness(x, B = 1000, up = 0.995, lo = 0.005)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A graph. Can be a \code{qgraph} object object, an \code{igraph} object, an adjacency matrix, a weight matrix and an edgelist, or a weighted edgelist.
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
The function computes the transitivity of the target network and the average shortest path length. Then it computes the average of the same indices on \code{B} random networks. The small-worldness index is then computed as the transitivity (normalized by the random transitivity) over the average shortest path length (normalized by the random average shortest path length). The  \code{lo} and \code{up} quantiles of the distribution of the random networks are also returned for both the transitivity and the average shortest path length.

A network can be said "smallworld" if its \code{smallworldness} is higher than one (a stricter rule is \code{smallworldness}>=3; Humphries & Gurney, 2008). To consider a network as "smallworld", it is also suggested to inspect that the network has a transitivity substantially higher than comparable random networks and that its average shortest path length is similar or higher (but not many times higher) than that computed on random networks.
Edge weights, signs and directions are ignored in the computation of the indices.
}
\value{
\item{smallworldness}{the "small-worldness" index proposed by Humphries & Gurney (2008)}
\item{trans_target}{the global transitivity of the target network (Newman, 2003)}
\item{averagelength_target}{the average shortest path length in the target network}
\item{trans_rnd_M}{the average transitivity in the \code{B} random networks}
\item{trans_rnd_lo}{the \code{lo} quantile of the transitivity in the \code{B} random networks}
\item{trans_rnd_up}{the \code{up} quantile of the transitivity in the \code{B} random networks}
\item{averagelength_rnd_M}{the average shortest path length in the \code{B} random networks}
\item{averagelength_rnd_lo}{the \code{lo} quantile of the shortest path length in the \code{B} random networks}
\item{averagelength_rnd_up}{the \code{up} quantile of the shortest path length in the \code{B} random networks}
}
\references{
Costantini, G., Epskamp, S., Borsboom, D., Perugini, M., Mottus, R., Waldorp, L., Cramer, A. O. J., State of the aRt personality research: A tutorial on network analysis of personality data in R. Manuscript submitted for publication.

Humphries, M. D., & Gurney, K. (2008). Network "small-world-ness": a quantitative method for determining canonical network equivalence. PLoS One, 3(4), e0002051.

Newman, M. E. J. (2003). The structure and function of complex networks*. SIAM Review, 45(3), 167–256.
}
\author{
Giulio Costantini (giulio.costantini@unimib.it), Sacha Epskamp (mail@sachaepskamp.com)
}
\note{
If a directed network is given as input, an edge between every two nodes i and j is considered present if there is an arrow either from i to j or from j to i or both.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
set.seed(1)
# a regular lattice. Even if the small-worldness is higher than three, the average path length is 
# much higher than that of random networks.
regnet<-igraph::watts.strogatz.game(dim=1, size=1000, nei=10, p=0, loops=FALSE, multiple=FALSE)
smallworldness(regnet, B=10)

\dontrun{
# a small-world network: the transitivity is much higher than random, the average path length is 
# close to that of random networks
swnet<-igraph::watts.strogatz.game(dim=1, size=1000, nei=10, p=.1, loops=FALSE, multiple=FALSE)
smallworldness(swnet, B=10)

# a pseudorandom network: both the average path length and the transitivity are similar to random 
# networks.
rndnet<-igraph::watts.strogatz.game(dim=1, size=1000, nei=10, p=1, loops=FALSE, multiple=FALSE)
smallworldness(rndnet, B=10)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smallworld}
\keyword{transitivity}
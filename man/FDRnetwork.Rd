\name{FDRnetwork}
\alias{FDRnetwork}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model selection using local False Discovery Rate
}
\description{
This function is a wrapper arounf \code{\link[fdrtool]{fdrtool}} to easilly compute a correlation or partial correlation network in which all nonsignificant edges are set to zero.
}
\usage{
FDRnetwork(net, cutoff = 0.1, method = c('lfdr', 'pval', 'qval'))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
A correlation or partial correlation matrix
}
  \item{cutoff}{
  The cutoff value to use. The edges of which the value of the first element of \code{method} are higher than the cutoff are removed. Thus, by default, edges with a local false discovery rate of higher than 0.1 are removed from the graph.
}
  \item{method}{The method to use with the cutoff. Can be \code{'lfdr'} for the local false discobvery rate, \code{'pval'} for the p-value of \code{'qval'} for the q-value.}
}
\details{
\code{method = 'lfdr'} could result in a very sparse network, so also looking at other values is advisable.
}
\references{
Bernd Klaus and Korbinian Strimmer. (2014). fdrtool: Estimation of (Local) False Discovery Rates and Higher Criticism. R package version 1.2.12. http://CRAN.R-project.org/package=fdrtool
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}
\examples{
\dontrun{
### Using bfi dataset from psych ###
library("psych")
data(bfi)

### CORRELATIONS ###
# Compute correlations:
CorMat <- cor_auto(bfi[,1:25])

# Run local FDR:
CorMat_FDR <- FDRnetwork(CorMat)

# Number of edges remaining:
mean(CorMat_FDR[upper.tri(CorMat_FDR,diag=FALSE)]!=0)

# None, so might use different criterion:
CorMat_FDR <- FDRnetwork(CorMat, method = "pval")


# Compare:
L <- averageLayout(CorMat, CorMat_FDR)

layout(t(1:2))
qgraph(CorMat, layout = L, title = "Correlation network", 
       maximum = 1, cut = 0.1, minimum = 0, esize = 20)
qgraph(CorMat_FDR, layout = L, title = "Local FDR correlation network", 
       maximum = 1, cut = 0.1, minimum = 0, esize = 20)

# Centrality:
centralityPlot(list(cor=CorMat, fdr = CorMat_FDR))


### PARTIAL CORRELATIONS ###
# Partial correlation matrix:
library("parcor")
PCorMat <- cor2pcor(CorMat)

# Run local FDR:
PCorMat_FDR <- FDRnetwork(PCorMat, cutoff = 0.1, method = "pval")

# Number of edges remaining:
mean(PCorMat_FDR[upper.tri(PCorMat_FDR,diag=FALSE)]!=0)

# Compare:
L <- averageLayout(PCorMat, PCorMat_FDR)

layout(t(1:2))
qgraph(PCorMat, layout = L, title = "Partial correlation network", 
          maximum = 1, cut = 0.1, minimum = 0, esize = 20)
qgraph(PCorMat_FDR, layout = L, title = "Local FDR partial correlation network", 
          maximum = 1, cut = 0.1, minimum = 0, esize = 20)

# Centrality:
centralityPlot(list(cor=PCorMat, fdr = PCorMat_FDR))
}
}

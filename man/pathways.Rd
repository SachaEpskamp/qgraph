\name{pathways}
\alias{pathways}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Highlight shortest pathways in a network
}
\description{
This function highlights the shortest paths between nodes in a network made by \code{\link{qgraph}}. Based on Isvoranu et al. (2016).
}
\usage{
pathways(graph, from, to, fading = 0.25, lty = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
Output from \code{\link{qgraph}}.
}
  \item{from}{
A vector indicating the first set of nodes between which pathways should be highlighted. Can be numeric or characters corresponding to node labels.
}
  \item{to}{
A vector indicating the second set of nodes between which pathways should be highlighted. Can be numeric or characters corresponding to node labels.
}
  \item{fading}{
The fading of the edges that are not part of shortest paths between 'from' and 'to'.
}
  \item{lty}{
The line type of the edges that are not part of shortest paths between 'from' and 'to'. 
}
}

\references{
Isvoranu, A. M., van Borkulo, C. D., Boyette, L. L., Wigman, J. T., Vinkers, C. H., Borsboom, D., & Group Investigators. (2016). A Network Approach to Psychosis: Pathways Between Childhood Trauma and Psychotic Symptoms. Schizophrenia bulletin, sbw055.
}
\author{
Sacha Epskamp \& Adela M. Isvoranu
}

\seealso{
\code{\link{qgraph}}
}
\examples{
library("qgraph")
library("psych")
data(bfi)

# Compute correlations:
CorMat <- cor_auto(bfi[,1:25])

# Compute graph with tuning = 0 (BIC):
BICgraph <- qgraph(CorMat, graph = "glasso", sampleSize = nrow(bfi),
                   tuning = 0, layout = "spring", title = "BIC", details = TRUE)

# All paths between Agreeableness and Neuroticism:
pathways(BICgraph, 
         from = c("A1","A2","A3","A4","A5"), 
         to = c("N1","N2","N3","N4","N5"))

}

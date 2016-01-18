\name{makeBW}
\alias{makeBW}
\title{
A qgraph plot can be understood in black and white
}
\description{
Plot a qgraph network that can be understood also in black and white or grayscale. Positive lines are full and negative ones are dashed. Nodes colors are associated to unique motifs. Up to 12 different motifs are supported at the moment.  
}
\usage{
makeBW(x, colorlist = NA,  plot = TRUE)
}
\arguments{
  \item{x}{
A qgraph object
}
  \item{colorlist}{
Optional: a vector of colors. See details. 
}
  \item{plot}{
logical: if FALSE, the network is not plotted. 
}
}
\details{
If no colorlist is specified, each color is randomly associated to one of the motifs. Specifying colorlist serves for (a) assigning colors to a specific motif, because the first color in the vector will always be associated to the first motif (this can be used e.g., for being consistent across plots), or (b) for associating motifs only to some of the colors, but not to others, since only in colors in the colorlist are associated to motifs if a colorlist is specified.
}
\value{
Silently returns a qgraph object "x" in which two new elements are present, "$graphAttributes$Nodes$density" and "$graphAttributes$Nodes$angles", which affect how the nodes are plotted. Can also be further customized and then re-plotted using plot(x). 
}

\author{
Giulio Costantini
}

\examples{

set.seed(1)
x <- cor(matrix(rnorm(25), nrow = 5))
colors <- c("red", "red", "blue", "blue", "white")

# colored qgraph plot
qg <- qgraph(x, colors = colors)

# randomly assing motifs to colors (notice that white nodes stay white)
makeBW(qg)
# associate a motif only to one of the colors
makeBW(qg, colorlist = c("blue"))
# define an order, which allows to choose motifs
makeBW(qg, colorlist = c("blue", "red"))
makeBW(qg, colorlist = c("red", "blue"))
}

\keyword{black}
\keyword{white}

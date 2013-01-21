\name{qgraph.panel}
\alias{qgraph.panel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
qgraph.panel
}
\description{
Creates a 4-panel graph. See details. The usage is the same as \code{\link{qgraph}}
}
\usage{
qgraph.panel(input, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{The weights matrix or edgelist. See \code{\link{qgraph}}. Correlation matrix is recommended.}
  \item{\dots}{Optional additional arguments (only 'layout' and 'graph' are omitted). See \code{\link{qgraph}}.}
}
\details{
This function will create a 4-panel plot containing four plots useful in analyzing correlation matrices:
\enumerate{
\item Association graph with circular layout
\item Association graph with spring layout
\item Concentration graph with spring layout
\item Factorial graph with spring layout
}
}
\author{
Sacha Epskamp (qgraph@sachaepskamp.com)
}
\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.
}
\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.svg}}
}
\examples{
\dontrun{
data(big5)
data(big5groups)

qgraph.panel(cor(big5),groups=big5groups,minimum=0.2,borders=FALSE,vsize=1,cut=0.3)
}
}

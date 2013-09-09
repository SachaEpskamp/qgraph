\name{qgraph.efa}
\alias{qgraph.efa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph.efa}
\description{
This function performs an Exploratory Factor Analysis (EFA) using the \code{\link{factanal}} (stats) function and sends the acquired factor loadings to \code{\link{qgraph.loadings}}.
}
\usage{
qgraph.efa(dat,factors=1,...,rotation="promax",residuals=TRUE,
		factorCors=NULL,scores="regression",
		corMat=nrow(dat)==ncol(dat) && all(dat==t(dat)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{dat}{A correlation matrix, data matrix or a "factanal" object}
	\item{factors}{The number of factors to extract}
	\item{rotation}{rotation to be used. Can be "varimax", "promax" or "none"}
	\item{residuals}{Logical indicating if residuals should be plotted. Defaults to TRUE}
	\item{factorCors}{Logical indicating if correlations of factors should be extracted and plotted. Defaults to FALSE if a correlation matrix is used and TRUE if a data matrix is used.}
	\item{...}{arguments passed to \code{\link{qgraph.loadings}}}
	\item{scores}{Method used to extract scores in \code{\link{factanal}}}
	\item{corMat}{Logical indicating if the 'dat' object is a correlation matrix (TRUE) or data matrix (FALSE) }
}

\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.

}
\author{
Sacha Epskamp (mail@sachaepskamp.com)
}
\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.pca}}
\code{\link{qgraph.loadings}}
}
\examples{
\dontrun{
data(big5)
data(big5groups)

qgraph.efa(big5,5,groups=big5groups,rotation="promax",minimum=0.2,cut=0.4,
		   vsize=c(1,7),borders=FALSE,vTrans=200)

# Tree layout:
qgraph.efa(big5,5,groups=big5groups,rotation="promax",minimum=0.2,cut=0.4,
		   vsize=c(1,7),borders=FALSE,layout="tree",width=20,filetype="R")
}
}

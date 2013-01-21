\name{qgraph.loadings}
\alias{qgraph.loadings}

\title{qgraph.loadings}
\description{
This function is a wrapper function for \code{\link{qgraph}} designed to visualize factor loadings.
}
\usage{
qgraph.loadings( fact, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{fact}{A matrix containing factor loadings (items per row, factors per column) or an "loadings" object}
	\item{...}{Additional optional arguments passed to \code{\link{qgraph}} and special arguments used in this function (described below).}
}
\section{Additional optional arguments}{	
\describe{
	\item{layout}{If "default" a standard layout for factor models will be made. If this is "circle" the default layout is circled (factors in the centre, items at the edge). No other layouts are currently supported.}
	\item{vsize}{A vector where the first value indicates the size of manifest variables and the second value indicates the size of latent variables.}
	\item{model}{"reflective" to have arrows go to manifest variables, "formative" to have arrows go to latent variables or "none" (default) for no arrows}
	\item{crossloadings}{Logical, if TRUE then for each manifest variable the strongest loading is omitted (default to FALSE).}
	\item{groups}{An optional list containing the measurement model, see  \code{\link{qgraph}}}
	\item{Fname}{When there is only one factor, this is it's name. If there are more factors, the names in the groups list are used only if the factors can be identified.}
	\item{resid}{Values for the residuals}
	\item{residSize}{Size of the residuals, defaults to 0.1}
	\item{factorCors}{Correlation matrix of the factors}
}}

\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.

}
\author{
Sacha Epskamp (qgraph@sachaepskamp.com)
}
\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.pca}}
\code{\link{qgraph.efa}}
}
\examples{
\dontrun{
# Load big5 dataset:
data(big5)
data(big5groups)

big5efa <- factanal(big5,factors=5,rotation="promax",scores="regression")
big5loadings <- loadings(big5efa)
qgraph.loadings(big5loadings,groups=big5groups,rotation="promax",minimum=0.2,
				cut=0.4,vsize=c(1.5,15),borders=FALSE,vTrans=200)

# Tree layout:
qgraph.loadings(big5loadings,groups=big5groups,rotation="promax",minimum=0.2,
				cut=0.4,vsize=c(1.5,15),borders=FALSE,vTrans=200,
				layout="tree",width=20,filetype="R")
}
}

\name{qgraph.pca}
\alias{qgraph.pca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph.pca}
\description{
This function performs an Principal Component Analysis (PCA) using the 'princomp' function of the psych package (Revelle, 2010)  and sends the acquired factor loadings to \code{\link{qgraph.loadings}}.
}
\usage{
qgraph.pca( cor, factors=1, ..., rotation="promax", factorCors = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{cor}{A correlation matrix or a "principal" object}
	\item{factors}{The number of factors to extract}
	\item{...}{arguments passed to \code{\link{qgraph.loadings}}}
	\item{rotation}{rotation to be used. Can be "varimax", "promax" or "none"}
	\item{factorCors}{Logical, should the correlations between factors be plotted? Defaults to TRUE}
}

\references{
Sacha Epskamp, Angelique O. J. Cramer, Lourens J.
  Waldorp, Verena D. Schmittmann, Denny Borsboom (2012).
  qgraph: Network Visualizations of Relationships in
  Psychometric Data. Journal of Statistical Software,
  48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.


Revelle, W. (2010) psych: Procedures for Personality and Psychological Research Northwestern University, Evanston, http://personality-project.org/r/psych.manual.pdf, 1.0-93
}
\author{
Sacha Epskamp (mail@sachaepskamp.com)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.efa}}
\code{\link{qgraph.loadings}}
}
\examples{
\dontrun{
data(big5)
data(big5groups)

qgraph.pca(cor(big5),5,groups=big5groups,rotation="promax",minimum=0.2,
			cut=0.4,vsize=c(1,7),borders=FALSE,vTrans=200)

# Tree layout:
qgraph.pca(cor(big5), 5,groups=big5groups, rotation="promax", minimum=0.2, 
  cut=0.4, vsize=c(1.5,7), borders=FALSE, layout="tree", width=20, filetype="R")
}
}

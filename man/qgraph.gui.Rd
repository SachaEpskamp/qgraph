\name{qgraph.gui}
\alias{qgraph.gui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical user interface for qgraph
}
\description{
This function works the same as \code{\link{qgraph}} except that a graphical user interface is opened allowing the user to change parameters.
}
\usage{
qgraph.gui(input, corMat, ...)
}

\arguments{
  \item{input}{
Input, see \code{\link{qgraph}}
}
  \item{corMat}{
Logical, is 'input' a correlation or covariance matrix. If this is TRUE a different GUI window is opened with more options. By default this is TRUE only if 'input' is a symmetrical matrix with all absolute values less or equal than 1 and all diagonal elements equal to 1. If a covariance matrix is used for 'input' and this argument is set to TRUE the plots are made using a `cov2cor` call, but the covariance matrix is used in computing EFA and PCA results.
}
  \item{\dots}{
Arguments passed to \code{\link{qgraph}}
}
}
\details{
Press enter after entering values to save them for the next plot.
}
\references{
 Adrian Bowman, Ewan Crawford, Gavin Alexander, Richard W. Bowman
  (2007). rpanel: Simple Interactive Controls for R Functions Using
  the tcltk Package. Journal of Statistical Software, 17(9), 1-18.
  URL http://www.jstatsoft.org/v17/i09/.
}
\author{
Sacha Epskamp <qgraph@sachaepskamp.com>
}

\seealso{
\code{\link{qgraph}}
}
\examples{
\dontrun{
data(big5)
data(big5groups)
 
qgraph.gui(cor(big5),groups=big5groups)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

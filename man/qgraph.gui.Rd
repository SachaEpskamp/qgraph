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
qgraph.gui(input, ...)
}

\arguments{
  \item{input}{
Input, see \code{\link{qgraph}}
}
  \item{\dots}{
Arguments passed to \code{\link{qgraph}}
}
}
\details{
Does not work with integrated plotting area in RStudio. Instead, call a new window with x11() or X11() first (or use the 'new' button).
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

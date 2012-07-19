\name{qgraph.arrow}
\alias{qgraph.arrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
qgraph.arrow
}
\description{
This function creates an open or closed arrowhead. It is used in \code{\link{qgraph}} and not designed for seperate use.
}
\usage{
qgraph.arrow(x, y, x.orig, y.orig, length, angle = 30 * pi/180, lwd, 
			 col = "black", open = T, Xasp = 1, lty = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
X coordinate of the arrow end.
}
  \item{y}{
Y coordinate of the arrow end.
}
  \item{x.orig}{
X coordinate of a point of origin.
}
  \item{y.orig}{
Y coordinate of a point of origin
}
  \item{length}{
Length of arrowhead in inches
}
  \item{angle}{
Angle of arrowhead in radian
}
  \item{lwd}{
Line width
}
  \item{col}{
Color of the arrowhead
}
  \item{open}{
TRUE for open arrowhead, FALSE for closed arrowhead.
}
  \item{Xasp}{
width/height ratio of the plotting area
}
  \item{lty}{
Line type
}
}
\author{
Sacha Epskamp (qgraph@sachaepskamp.com)
}

\name{qgraphHTML}
\alias{qgraphHTML}
\title{Interactive HTML output for qgraph graphs}
\description{
Exports a qgraph object to a self-contained interactive HTML file. The graph
is rendered as SVG with exactly the layout and style of the corresponding R
plot (node placement, colors, node and edge sizes, curvature, line types,
labels and edge labels), and supports:
\itemize{
  \item dragging nodes (edges follow),
  \item selecting a node by clicking it, with two highlight modes that can be
    toggled: highlighting the node with its direct connections, or
    highlighting all outgoing shortest paths from that node (using inverse
    absolute edge weights as distances, respecting edge directions),
  \item tooltips showing the node label, the node name (\code{nodeNames})
    and an optional custom text (\code{tooltips} argument of
    \code{\link{qgraph}}),
  \item saving the current view as an SVG or PNG file,
  \item an interactive legend (in the \code{"rich"} version) in which groups
    and nodes can be clicked to highlight them.
}
The file has no external dependencies and can be opened offline in any
modern browser. This function is also called by \code{\link{qgraph}} and
\code{plot.qgraph} when \code{filetype = "html"} is used.
}
\usage{
qgraphHTML(x, filename = "qgraph.html", htmltype = c("rich", "plain"),
           open = interactive(), verbose = TRUE)
}
\arguments{
  \item{x}{A \code{"qgraph"} object, as returned (invisibly) by \code{\link{qgraph}}.}
  \item{filename}{Name of the output file. The extension \code{.html} is
    added if not present. Note that when this function is called via
    \code{qgraph(..., filetype = "html")}, the \code{filename} argument of
    \code{qgraph} is used (without extension), as for the other file types.}
  \item{htmltype}{\code{"rich"} (default) writes a full viewer with a
    toolbar (highlight-mode toggle, reset layout, save SVG/PNG) and an
    interactive legend panel. \code{"plain"} writes a minimal page
    containing only the interactive graph, suitable for embedding in
    another page (e.g., in an \code{<iframe>}); in the plain version the
    highlight mode can be toggled with the \code{m} key or by
    shift-clicking a node.}
  \item{open}{Logical; open the resulting file in the default browser?}
  \item{verbose}{Logical; print a message with the location of the output file?}
}
\details{
Most graphical attributes are reproduced exactly from the qgraph object.
A small number of plot elements are approximated or not (yet) supported in
the HTML output: means/SDs bars inside nodes, knots (tied edges), subplots
and images as nodes, and the XKCD style. Self-loop and arrowhead geometry
are close approximations of their R counterparts.
}
\value{
The path of the written file, invisibly.
}
\author{Sacha Epskamp <mail@sachaepskamp.com>}
\seealso{\code{\link{qgraph}}}
\examples{
\dontrun{
data(big5)
data(big5groups)
Q <- qgraph(cor(big5), minimum = 0.25, groups = big5groups,
            nodeNames = colnames(big5), legend = TRUE, DoNotPlot = TRUE)

# Rich interactive viewer:
qgraphHTML(Q, "big5.html")

# Minimal embeddable version:
qgraphHTML(Q, "big5_plain.html", htmltype = "plain")

# Equivalently, directly from qgraph:
qgraph(cor(big5), minimum = 0.25, groups = big5groups,
       nodeNames = colnames(big5), filetype = "html", filename = "big5")
}
}

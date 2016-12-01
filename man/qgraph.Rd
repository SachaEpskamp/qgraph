\encoding{UTF8}
\name{qgraph}
\alias{qgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph}
\description{
This is the main function of qgraph which automatically creates an appropriate network and sends it to the plotting method.}
\usage{
qgraph( input, ... )
}
\arguments{
	\item{input}{Can be either a weights matrix or an edgelist. Can also be an object of class "sem" (sem), "mod" (sem), "lavaan" (lavaan), "principal" (psych), "loadings" (stats), "factanal" (stats), "graphNEL" (Rgraphviz), "pcAlgo" (pcalg),  \code{"huge"} (huge), \code{"select"} (huge) or the output of \code{glasso}} 
	\item{...}{Any additional arguments described below. Also a list with class "qgraph" can be added that contains any of these arguments (this is returned invisibly by the function)}
}	
\section{Important additional arguments}{	
\describe{
\item{layout}{This argument controls the layout of the graph. "circle" places all nodes in a single circle, "groups" gives a circular layout in which each group is put in separate circles and "spring" gives a force embedded layout. It also can be a matrix with a row for each node and x and y coordinates in the first and second column respectively. Defaults to "circular" in weighted graphs without a groups list, "groups" in weighted graphs with a groups list, and "spring" in unweighted graphs. Can also be a function from the igraph package.}
\item{groups}{An object that indicates which nodes belong together. Can be a list in which each element is a vector of integers identifying the numbers of the nodes that belong together, or a factor.}
\item{minimum}{Edges with absolute weights under this value are not shown (but not omitted). Defaults to 0. Can also be set to \code{"sig"} to only show significant edges for \code{graph = "association"} and \code{graph = "concentration"}).  Significance level is controlled by \code{alpha} and \code{bonf} arguments}
\item{maximum}{qgraph regards the highest of the maximum or highest absolute edge weight as the highest weight to scale the edge widths too. To compare several graphs, set this argument to a higher value than any edge weight in the graphs (typically 1 for correlations).}
\item{cut}{In weighted graphs, this argument can be used to cut the scaling of edges in width and color saturation. Edges with absolute weights over this value will have the strongest color intensity and become wider the stronger they are, and edges with absolute weights under this value will have the smallest width and become vaguer the weaker the weight. If this is set to \code{NULL}, no cutoff is used and all edges vary in width and color. Defaults to NULL for graphs with less then 20 nodes. For larger graphs the \code{cut} value is automatically chosen to be equal to the maximum of the 75th quantile of absolute edge strengths or the edge strength corresponding to 2n-th edge strength (n being the number of nodes.)}
\item{details}{Logical indicating if minimum, maximum and cutoff score should be printed under the graph. Defaults to FALSE.}

\item{threshold}{A numeric value that defaults to 0. Edges with absolute weight that are not above this value are REMOVED from the network. This differs from \code{minimum} which simply changes the scaling of width and color so that edges with absolute weight under minimum are not plotted/invisible. Setting a threshold influences the spring layout and centrality measures obtained with the graph whereass setting a minimum does not. In the case of correlation (\code{graph = "association"}) or partial correlation (\code{graph = "concentration"}) networks this argument can also be given a string to omit insignificant edges. See description of this argumet in the next section (Additional options for correlation/covariance matrices).}
\item{palette}{The palette used for coloring nodes when the \code{groups} argument is used. Can be one of \code{"rainbow"} (default), \code{"colorblind"} (making use of http://jfly.iam.u-tokyo.ac.jp/color/), \code{"pastel"}, \code{"gray"}, \code{"R"} and \code{"ggplot2"}.}
\item{theme}{This argument sets different defaults for various graphical arguments (most notably \code{posCol}, \code{negCol} and \code{palette}). Can be \code{"classic"}, \code{"colorblind"}, \code{"gray"}, \code{"Hollywood"}, \code{"Borkulo"}, \code{"TeamFortress"}, \code{"Reddit"}, \code{"Leuven"} or \code{"Fried"}.}
}
}


\section{Additional options for correlation/covariance matrices}{
\describe{
  \item{graph}{Type of graph to be made when a correlation or covariance matrix is used as input. Setting this to other values than 
  \code{"default"} will check if the matrix is a correlation or covariance matrix; if the matrix is not positive definite \code{\link[Matrix]{nearPD}} from the Matrix package will be used. Options are:
  \describe{
  \item{"association" or "cor"}{Plots a correlation network. Runs \code{\link[stats]{cov2cor}} if input is detected to be a covariance matrix and plots the input as is}
  \item{"concentration" or "pcor"}{Plots a partial correlation network, using \code{cor2pcor} from the parcor package (Kraemer, Schaefer and Boulesteix, 2009) on the input matrix}
      \item{"glasso"}{Will run \code{\link{EBICglasso}} to obtain an optimal sparse estimate of the partial correlation matrix using the glasso package (Friedman, Hastie and Tibshirani, 2011)}
  }
  Outdated and limited supported options are \code{"factorial"}, which will create a graph based on an exploratory factor analysis, and \code{"sig"} will transform all correlations in p-values (using the fdrtool package; Korbinian Strimmer, 2014) and force mode="sig". "sig2" will do the same but show p-values based on negative statistics in shades of orange}
  \item{threshold}{In addition to a numeric value to omit edges this argument can also be assigned a string to omit insignficant edges. Note that this REMOVES edges from the network (which influences centrality measures and the spring layout). Can be \code{"sig"} to compute significance without correction for multiple testing, \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"} or \code{"none"} which are used directly in the \code{adjust} argument in \code{\link[psych]{corr.p}} of the psych package (Revelle, 2014). In addition, this argument can be assigned \code{"locfdr"} in which edges are set to zero if the local FDR is below \code{FDRcutoff}. \code{\link[fdrtool]{fdrtool}} from the fdrtool package (Klaus and Strimmer, 2014) is used to compute these measures, which is used inside \code{\link{FDRnetwork}}.}
  
  \item{sampleSize}{The sample-size. Used when \code{graph = "glasso"} or \code{minimum = "sig"}}
    \item{tuning}{A tuning parameter used in estimation. Currently only used when \code{graph = "glasso"} and corresponds to the \code{gamma} argument}
      \item{refit}{
    Logical, should the optimal graph be refitted without LASSO regularization? Defaults to \code{FALSE} and only used if \code{graph = "glasso"}.
  }
     \item{countDiagonal}{
    Should diagonal be counted in EBIC computation? Defaults to \code{FALSE}. Set to \code{TRUE} to mimic qgraph < 1.3 behavior (not recommended!).
  }
    \item{alpha}{The significance level (defaults to 0.05) to be used for not showing edges if \code{minimum = "sig"}.}
	  \item{bonf}{Logical indicating if a bonferonni correction should be applied if \code{minimum = "sig"}.}
    \item{FDRcutoff}{Cutoff used in which partial correlations should be included if \code{graph = "fdr"}. Defaults to 0.9}
  }
}

\section{Output arguments}{
\describe{
\item{mar}{A vector of the form c(bottom, left, top, right) which gives the margins. Works similar to the argument in par(). Defaults to c(3,3,3,3)}
	\item{filetype}{A character containing the file type to save the output in. "R" outputs in a new R window, "pdf" creates a pdf file. "svg" creates a svg file (requires RSVGTipsDevice). "tex" creates LaTeX code for the graph (requires tikzDevice). 'jpg', 'tiff' and 'png' can also be used. If this is given any other string (e.g. filetype="") no device is opened. Defaults to 'R' if the current device is the NULL-device or no new device if there already is an open device. A function such as \code{x11} can also be used}
	\item{filename}{Name of the file without extension}
	\item{width}{Width of the plot, in inches}
	\item{height}{Height of the plot, in inches}
  \item{normalize}{Logical, should the plot be normalized to the plot size. If TRUE (default) border width, vertex size, edge width and arrow sizes are adjusted to look the same for all sizes of the plot, corresponding to what they would look in a 7 by 7 inches plot if normalize is FALSE.}
    \item{DoNotPlot}{Runs qgraph but does not plot. Useful for saving the output (i.e. layout) without plotting}
	\item{plot}{Logical. Should a new plot be made? Defaults to TRUE. Set to FALSE to add the graph to the existing plot.}
	\item{rescale}{Logical. Defines if the layout should be rescaled to fit the -1 to 1 x and y area. Defaults to TRUE. Can best be used in combination with plot=FALSE.}
    \item{standAlone}{Logical. If filetype="tex" this argument can be used to choose between making the output a standalone LaTeX file or only the codes to include the graph.}
}}

\section{Graphical arguments}{	
\subsection{Nodes}{
These arguments influence the plotting of nodes in qgraph. Most of them can be assigned a single value or a vector with a value for each node.
\describe{
\item{color}{A vector with a color for each element in the groups list, or a color for each node. Defaults to the background color ("bg" argument, which defaults to "white") without groups list and rainbow(length(groups)) with a groups list.}
    \item{vsize}{A value indicating the size of the nodes (horizontal if shape is "rectangle". Can also be a vector of length 2 (nodes are scaled to degree) or a size for each node. Defaults to 8*exp(-nNodes/80)+1}
    \item{vsize2}{A value indicating the vertical size of the nodes where the shape is "rectangle". Can also be a vector of length 2 (nodes are scaled to degree) or a size for each node. Defaults to the value of 'vsize'. If 'vsize' is not assigned this value is used as a scalar to 'vsize' (e.g., \code{vsize2 = 1/2} would result in rectangled nodes where the height is half the default width)}
    \item{node.width}{Scalar on the default value of 'vsize'. Defaults to 1.}
    \item{node.height}{Scalar on the default value of 'vsize2'. Defaults to 1.}
    \item{borders}{Logical indicating if borders should be plotted, defaults to TRUE.}
  \item{border.color}{Color vector indicating colors of the borders. Is repeated if length is equal to 1. Defaults to "black"}
    \item{border.width}{Controls the width of the border. Defaults to 2 and is comparable to 'lwd' argument in 'points'.}
	\item{shape}{A character containing the shape of the nodes. \code{"circle"}, \code{"square"}, \code{"triangle"} and \code{"diamond"} are supported. In addition, can be a name of an element of \code{polygonList} to plot the corresponding polygon (not reccomended for large graphs), which by default includes shapes \code{"ellipse"} and \code{"heart"} Can also be a vector with a shape for each node. Defaults to "circle".}
    \item{polygonList}{ A list contaning named lists for each element to include polygons to lookup in the \code{shape} argument. Each element must be named as they are used in \code{shape} and contain a list with elements \code{x} and \code{y} contaning the coordinates of the polygon. By default \code{ellipse} and \code{heart} are added to this list. These polygons are scaled according to \code{vsize} and \code{vsize2}}
      \item{vTrans}{Transparency of the nodes, must be an integer between 0 and 255, 255 indicating no transparency. Defaults to 255}
          \item{subplots}{A list with as elements R expressions or NULL for each node. If it is an R expression it is evaluated to create a plot for the node.}
          \item{subpars}{List of graphical parameters to be used in the subplots}
          \item{subplotbg}{Background to be used in the sublots. If missing inherits from 'background' argument.}
        \item{images}{A character vector of length 1 or the same length as the number of nodes indicating the file location of PNG or JPEG images to use as nodes. Can be NA to not plot an image as node and overwrites 'subplots'
}
  \item{noPar}{Set to \code{TRUE} to not have qgraph run the \code{par} function. Useful when sending qgraph plots as sublots using \code{subplots}.}
\item{pastel}{Logical, should default colors (for groups or edge equality constraints) be chosen from pastel colors? If TRUE then \code{\link[colorspace]{rainbow_hcl}} is used.}
\item{rainbowStart}{A number between 0 and 1 indicating the offset used in rainbow functions for default node coloring.}
\item{usePCH}{Logical indicating if nodes should be drawn using polygons or base R plotting symbols. Defaults to \code{TRUE} if more than 50 nodes are used in the graph or if the graph is stored in a file. See details.}
\item{node.resolution}{Resolution of the nodes if \code{usePCH=FALSE}. Defaults to 100}
\item{title}{String with a title to be drawn in the topleft of the plot.}

\item{preExpression}{A parsable string containing R codes to be evaluated after opening a plot and before drawing the graph.}

\item{postExpression}{A parsable string containing R codes to be evaluated just before closing the device.}

  \item{diag}{Should the diagonal also be plotted as edges? defaults to FALSE. Can also be "col" to plot diagonal values as vertex colors.}
}
}

\subsection{Node labels}{
These arguments influence the plotting of node labels in qgraph. Most of them can be assigned a single value or a vector with a value for each node.
\describe{
    \item{labels}{If FALSE, no labels are plotted. If TRUE, order in weights matrix is used as labels. This can also be a vector with a label for each node. Defaults for graphs with less than 20 nodes to a 3 character abbreviation of the columnames and rownames if these are identical or else to TRUE. If a label contains an asterisk (e.g. "x1*") then the asterisk will be omitted and the label will be printed in symbol font (use this for Greek letters). Can also be a list with a label as each element, which can be expressions for more advanced mathematical annotation.}
  \item{label.cex}{Scalar on the label size.}
    \item{label.color}{Character containing the color of the labels, defaults to "black"}
  \item{label.prop}{Controls the proportion of the width of the node that the label rescales to. Defaults to 0.
  9.}
    \item{label.norm}{A single string that is used to normalize label size. If the width of the label is lower than the width of the hypothetical label given by this argument the width of label given by this argument is used instead. Defaults to "OOO" so that every label up to three characters has the same fontsize.}
    \item{label.scale}{Logical indicating if labels should be scaled to fit the node. Defaults to TRUE.}
    \item{label.scale.equal}{Logical, set to \code{TRUE} to make make the font size of all labels equal}
\item{label.font}{Integer specifying the label font of nodes. Can be a vector with value for each node}
   \item{label.fill.vertical}{
      Scalar (0 - 1) indicating the maximum proportion a label may fill a node vertically.
   }
\item{label.fill.horizontal}{
      Scalar (0 - 1) indicating the maximum proportion a label may fill a node horizontally.
   }
   
}
}


\subsection{Edges}{
These arguments influence the plotting of edges qgraph. Most of them can be assigned a single value, a vector with a value per edge when an edgelist is used as input or a matrix containing values for each edge when a wheights matrix is used as input.
\describe{
  \item{esize}{Size of the largest edge (or what it would be if there was an edge with weight maximum). Defaults to 15*exp(-nNodes/90)+1) for weighted graphs and 2 for unweighted graphs. In directed graphs these values are halved.}
  \item{edge.width}{Scalar on 'esize' and 'asize' arguments to make edges wider with a single argument. 'esize' is multiplied with this value and 'asize' with the square root of this value.}
    \item{edge.color}{Color of edges. Can be either a single value to make all edges the same color, a matrix with a color for each edge (when using a weights matrix) or a vector with a color for each edge (when using an edgelist). NA indicates that the default color should be used. Note that unless \code{fade=FALSE} colors still fade to white corresponding to their strength}
    \item{posCol}{Color of positive edges. Can be a vector of two to indicate color of edges under 'cut' value and color of edges over 'cut' value. If 'fade is set to TRUE the first color will be faded the weaker the edge weight is. If this is only one element this color will also be used for edges stronger than the 'cut' value. Defaults to c("#009900","darkgreen")}
\item{negCol}{Color of negative edges. Can be a vector of two to indicate color of edges under 'cut' value and color of edges over 'cut' value. If 'fade is set to TRUE the first color will be faded the weaker the edge weight is. If this is only one element this color will also be used for edges stronger than the 'cut' value. Defaults to c("#BF0000","red")}
\item{unCol}{Color to indicate the default edge color of unweighted graphs. Defaults to "#808080".}
        \item{probCol}{Color of the probability edges. Defaults to \code{"blue"}. Only used when \code{probabilityEdges = TRUE}}
\item{negDashed}{Logical, set to \code{TRUE} to make negative edges dashed (overwrites \code{lty}).}
        \item{probabilityEdges}{Logical, do edges indicate probabilities? If this is set to \code{TRUE} \code{posCol} is overwritten by \code{probCol}. Mainly implemented for automatic generation of graphs}
\item{colFactor}{Exponent of transformation in color intensity of relative strength. Defaults to 1 for linear behavior.}
  \item{trans}{In weighted graphs: logical indicating if the edges should fade to white (FALSE) or become more transparent (TRUE; use this only if you use a background). In directed graphs this is a value between 0 and 1 indicating the level of transparency. (also used as 'transparency')}
    \item{fade}{if TRUE (default) and if 'edge.color' is assigned, transparency will be added to edges that are not transparent (or for which no transparency has been assigned) relative to the edge strength, similar if 'trans' is set to TRUE.}
	\item{loop}{If diag=TRUE, this can be used to scale the size of the loop. defaults to 1.}
	\item{lty}{Line type, see 'par'}
      \item{edgeConnectPoints}{This argument specifies the point for each edge to which it connects to a node, in radians. Can be either a matrix with a row for each edge and two columns: The first column indicates the connection point of the source of the edge and the second column specifies the connection point of the destination of the edge. Can also be an array with a row and column for each node two slices which indicate the source and destination of the edge connecting the two nodes.}
}
}

\subsection{Edge Curvature}{
These arguments control the curvature of edges.  Most of them can be assigned a single value, a vector with a value per edge when an edgelist is used as input or a matrix containing values for each edge when a wheights matrix is used as input.
\describe{
\item{curve}{A value indicating how strongly edges should be curved. Either a single value, a vector (edgelist input) with a value for each edge or a matrix (weights matrix input). NA indicates default curve behavior should be used, which only curves edges if there are multiple edges between two nodes.}
    \item{curveAll}{Logical, indicating if all edges should be curved with the value of the 'curve' or only edges between nodes that have share multiple edges.}
    \item{curveDefault}{The default curvature. Defaults to 1.}
    \item{curveShape}{The shape of the curve, as used in \code{xspline}. Defaults to \code{-1}.}
    \item{curveScale}{Logical, should curve scale with distance between nodes. Defaults to \code{TRUE}. If \code{FALSE}, the curve can be exactly determined. Recommended to set to \code{TRUE} for graphs and \code{FALSE} for diagrams. The curvature is corrected for the number of nodes and will be smaller if there are more nodes.}
    \item{curveScaleNodeCorrection}{Logical, disable the node correction in \code{curveScale}. Defaults to \code{FALSE}. Not recommended. Set to \code{TRUE} ONLY if you know what you are doing.}
    \item{curvePivot}{Quantile to pivot curves on. This can be used to, rather than round edges, make straight edges as curves with "knicks" in them. Can be logical or numeric. \code{FALSE} (default) indicates no pivoting in the curved edges, a number indicates the quantile (and one minus this value as quantile) on which to pivot curved edges and \code{TRUE} indicates a value of 0.1.}
        \item{curvePivotShape}{The shape of the curve around the pivots, as used in \code{xspline}. Defaults to \code{0.25}.}
        \item{parallelEdge}{Logical, set to \code{TRUE} to draw parallel straight edges rather than curved edges when there are multiple edges between two nodes. Can be a vector with value per edge for edgelists or a matrix with a value per edge for weights marices.}
         \item{parallelAngle}{The distance in radians an edge is shifted if \code{parallel=TRUE}. Can be set to \code{NA} (default) to determine based on number of edges between two nodes. Can be a vector with value per edge for edgelists or a matrix with a value per edge for weights marices.}
         \item{parallelAngleDefault}{The default value for parallelAngle, indicating the angle of the edge furthest from the center. Defaults to \code{pi/6}}
}
}

\subsection{Edge Labels}{
These arguments influence the plotting of edge labels qgraph. Most of them can be assigned a single value, a vector with a value per edge when an edgelist is used as input or a matrix containing values for each edge when a wheights matrix is used as input.
\describe{
  \item{edge.labels}{If FALSE, no edge labels are plotted. If TRUE, numerical edge weights are printed on the edges. This can also be a vector with a label for each edge. Defaults to FALSE. If a label contains an asterisk (e.g. "y1*") then the asterisk will be omitted and the label will be printed in symbol font (use this for Greek letters). Can also be a list with a label as each element, which can be expressions for more advanced mathematical annotation.}  
    \item{edge.label.cex}{Either a single number or a number per edge used as a scalar of the edge label size. Defaults to 1.}
    \item{edge.label.bg}{Either a logical or character vector/matrix. Indicates the background behind edge labels. If TRUE (default) a white background is plotted behind each edge label. If FALSE no background is plotted behind edge labels. Can also be a single color character, a vector or matrix of color vectors for each edge.}
    \item{edge.label.position}{Vetor of numbers between 0 and 1 controlling the relative position of each edge label. Defaults to 0.5 for placing edge labels at the middle of the edge.}
\item{edge.label.font}{Integer specifying the label font of edges. Can be a vector or matrix with value for each node}
}
}

\subsection{Layout}{
Arguments concerning the placement of nodes, in combination with 'layout'.
\describe{
  \item{repulsion}{Scalar on the default repulse radius in the spring layout. Defaults to 1. Setting this argument to lower values (e.g., 0.5) will cause nodes in the spring layout to repulse eachother less. This is especially useful if a few unconnected nodes cause the giant component to visually be clustered too much in the same place.}
  \item{layout.par}{A list of arguments passed to \code{\link{qgraph.layout.fruchtermanreingold}} when \code{layout = "spring"} or to an igraph function when such a function is assigned to 'layout'. Defaults to \code{ list(repulse.rad = nNodes^(repulsion * 3))} if \code{layout = "spring"} and \code{list()} otherwise.}
	\item{layout.control}{A scalar on the size of the circles created with the circular layout.}
  \item{aspect}{Should the original aspect ratio be maintained if rescale = TRUE? Defaults to FALSE. Set this to TRUE to keep the aspect ratio of the original layout (e.g. result from layout="spring").}
  \item{rotation}{A vector that can be used to rotate the circles created with the circular layout. Must contain the rotation in radian for each group of nodes. Defaults to zero for each group.}

}

}

\subsection{Legend}{
Arguments to control the legend placed on the right side of the graph.
\describe{
  \item{legend}{Logical value indicating if a legend should be plotted. Defaults to TRUE if a groups object or nodeNames is supplied}
	\item{legend.cex}{Scalar of the legend. defaults to 1}
  \item{legend.mode}{Character string indicating the type of legend to be drawn. \code{"groups"} indicates the legend should be based on the \code{groups} object, \code{"names"} indicates the legend should be based on the \code{nodeNames} object, and \code{style1} and \code{style2} indicate the legend should be based on both. Defaults to \code{"style1"} if both \code{"groups"} and \code{"nodeNames"} arguments are used.}
	\item{GLratio}{Relative size of the graph compared to the layout. Defaults to 2.5}
      \item{layoutScale}{A vector with a scalar for respectively the x and y coordinates of the layout (which default plotting area is from -1 to 1 on both x and y axis). Setting this to e.g. c(2,2) would make the plot twice as big. Use this in combination with 'layoutOffset' and 'plot' arguments to define the graph placement on an existing plot.}
    \item{layoutOffset}{A vector with the offset to the x and  coordinates of the center of the graph (defaults to (0,0)). Use this in combination with 'layoutScale' and 'plot' arguments to define the graph placement on an existing plot.}
  \item{nodeNames}{Names for each node, can be used to plot a legend next to the plot that links the node labels to node names.}
}
}

\subsection{Background}{
These arguments control the background of the plot
\describe{
  \item{bg}{If this is TRUE, a background is plotted in which node colors cast a light of that color on a black background. Can also be a character containing the color of the background Defaults to FALSE}
	\item{bgcontrol}{The higher this is, the less light each node gives if bg=TRUE. Defaults to 6.}
	\item{bgres}{square root of the number of pixels used in bg=TRUE, defaults to 100.}
}
}

\subsection{General graphical arguments}{
\describe{
	\item{pty}{See 'par'}
	\item{gray}{Logical, set to TRUE to plot the graph in grayscale colors}
	\item{tooltips}{A vector with tooltips for each node, only used when filetype='svg' or filetype='tex'}
	\item{overlay}{Logical, should a Venn-diagram like overlay be plotted? If TRUE then for each group a x\% confidence region is plotted for the X and Y position, using \code{\link[ellipse]{ellipse}}}
	\item{overlaySize}{Specifies the size of the overlay ellipses. Corresponds to the confidence level (default is 0.5)}
\item{font}{Integer specifying the default font for node and edge labels}
}
}
}



\section{Arguments for directed graphs}{
\describe{
	\item{directed}{Logical indicating if edges are directed or not. Can be TRUE or FALSE to indicate if all edges are directed, a logical vector (when using edgelists) or a logical matrix (when using weights matrix)}
	\item{arrows}{A logical indicating if arrows should be drawn, or a number indicating how much arrows should be drawn on each edge. If this is TRUE, a simple arrow is plotted, if this is a number, arrows are put in the middle of the edges.}
  \item{arrowAngle}{Angle of the arrowhead, in radians. Defaults to pi/8 for unweighted graphs and pi/4 for weighted graphs.}
	\item{asize}{Size of the arrowhead. Defaults to 2*exp(-nNodes/20)+2.}
	\item{open}{Logical indicating if open (TRUE) or closed (FALSE) arrowheads should be drawn.}
	\item{bidirectional}{If this is TRUE, Then directional edges between nodes that have two edges between them are not curved. Defaults to FALSE. Can also be a logical vector (when using edgelists) or a logical matrix (when using weights matrix)}
}}

\section{Arguments for graphs based on significance values}{
\describe{
	\item{mode}{This argument defines the mode used for coloring the edges. The default, "strength" assumes each edge weight indicates the strength of connection centered around  and makes positive edges green and negative edges red. If this is set to "sig" then the edge weights are assumed to be significance values and colored accordingly. This can also include negative values, which will be interpreted as p-values based on negative statistics.}
  \item{alpha}{The significance level (defaults to 0.05) to be used for not showing edges if \code{minimum = "sig"}, or if \code{Graph = "sig"} a vector of max 4 elements indicating the alpha level cutoffs. Defaults to c(0.0001,0.001,0.01,0.05)}
	\item{sigScale}{The function used to scale the edges if mode="sig". Defaults to $function(x)0.8*(1-x)^(log(0.4/0.8,1-0.05))$}
	  \item{bonf}{Logical indicating if a bonferonni correction should be applied if \code{minimum = "sig"} or \code{mode="sig"}}
}}

\section{Arguments for plotting scores on nodes}{	
\describe{
	\item{scores}{This argument can be used to plot scores of an individual on the test. Should be a vector with the scores for each item. Currently this can only be integer values (e.g.\ LIKERT scales).}
	\item{scores.range}{Vector of length two indicating the range of the scores, if scores is assigned.}
}}

\section{Arguments for manually defining graphs}{
\describe{
  \item{mode}{The mode argument (see section on significance graph arguments) can also be used to make the weights matrix correspond directly to the width of the edges (as in lwd of plot()). To do this, set mode to "direct".}
	\item{edge.color}{This argument can be used to overwrite the colors. Can be either a single value to make all edges the same color, a matrix with a color for each edge (when using a weights matrix) or a vector with a color for each edge (when using an edgelist). NA indicates that the default color should be used. Note that unless \code{fade=FALSE} colors still fade to white corresponding to their strength}
}}


\section{Arguments for knots (tying together edges)}{
\describe{
  \item{knots}{This argument can be used to tie edges together in their center, which can be useful to, for example, indicate interaction effects. This argument can be assigned a list where each element is a vector containing the edge numbers that should be knotted together. Another option is to assign the argument a integer vector (for edgelists) or a matrix (for weight matrices) with 0 indicating edges that should not be tied together, and increasing numbers indicating each knot.}
   \item{knot.size}{The size of the knots. Can be of length one or a vector with the size of each knot. Similar to 'vsize'. Defaults to 1.}
   \item{knot.color}{The color of the knots. Can be of length one or a vector with the size of each knot. Defaults to NA, which will result in a mix of the knotted edge colors.}
  \item{knot.borders}{Logical indicating if a border should be plotted around the knot. Can be of length one or a vector with the size of each knot. Works similar to 'borders'. Defaults to FALSE}
  \item{knot.border.color}{Color of the knot borders. Can be of length one or a vector with the size of each knot. Works similar to 'border.color'. Defaults to "black"}
    \item{knot.border.width}{Width of the knot borders. Can be of length one or a vector with the size of each knot. Works similar to 'border.width'. Defaults to 1}
}}

\section{Arguments for bars}{
\describe{
  \item{means}{
       A vector with means for every node or \code{NA}. Will plot a vertical bar at the location of the mean between \code{meanRange} values. \code{NA} omits a bar.
  }
  \item{SDs}{
       A vector with SDs for every node or \code{NA}. Will plot an error bar of 2 times this value around the \code{means} location. \code{NA} to omit.
  }
  \item{meanRange}{
    The range of the \code{means} argument. Default to \code{range(means,na.rm=TRUE)}
  }
  \item{bars}{A list with for each node containing either NULL or a vector with values between 0 and 1 indicating where bars should be placed inside the node.}
  \item{barSide}{Integer for each node indicating at which side the bars should be drawn. 1, 2, 3 or 4 indicating at bottom, left, top or right respectively.}  
  \item{barColor}{A vector with for each node indicating the color of bars. Defaults to the border color of the node.}  
  \item{barLength}{A Vector indicating the relative length of bars of each node compared to the node size. Defaults to 0.5.}  
      \item{barsAtSide}{Logical, should bars be drawn at the side of a node or at its center? Defaults to FALSE.}  
  
}}

\section{Arguments for pies}{
\describe{
  \item{pie}{
       A vector with values between 0 and 1 for each node (or one value for all nodes). Supplying this argument will make the border of nodes a pie chart. Can also be a list with vectors to make pie charts of multiple parts.}
    \item{pieBorder}{
      The size of the pie chart in the border, between 0 and 1. Defaults to \code{0.15}. Set to 1 to make the whole node a pie chart. Can be a vector with a value for each node.
  }
      \item{pieColor}{
       Colors of the pie plot parts. Can be a vector with a value for each node, or a list with multiple values if there are more parts.
  }
      \item{pieColor2}{
      Final color of the pie chart. Only added if the values in the 'pie' argument do not add up to 1. Defaults to \code{'white'}. Can be a vector with a value for each node.
  }
  \item{pieStart}{
      A vector with values between 0 and 1 for each node (or one value for all nodes), indicating the starting point of the pie chart. 
  }
  \item{pieDarken}{
    A vector with values between 0 and 1 for each node (or one value for all nodes), indicating how much darker the pie border color is made than the node color in the default coloring scheme.
  }
    \item{piePastel}{
    Should pastel colors be used to fill pie chart parts when more than 2 blocks are used?
  }
}
}

%\section{Arguments that support the BDgraph package}{  
%\describe{
  %\item{BDgraph}{A character vector containing \code{"phat"} for plotting posterior probabilities of each edge, \code{"Khat"} for plotting mean posterior partial correlations or both. The order matters as the layout of the second plot is based on the layout of the first plot. Defaults to \code{c("phat","Khat")}}
  %\item{BDtitles}{Logical, should titles be drawn above the plots?}  
%}}

\section{Additional arguments}{	
\describe{
  \item{edgelist}{Logical, if TRUE 'input' is assumed to be an edgelist, else if FALSE input is assumed to be a weights matrix. By default this is chosen automatically based on the dimensions of 'input' and this argument is only needed if the dimensions are ambiguous (square matrix with 2 or 3 rows/columns)}
	\item{weighted}{Logical that can be used to force either a weighted graph (TRUE) or an unweighted graph(FALSE).}
	\item{nNodes}{The number of nodes, only needs to be specified if the first argument is an edge-list and some nodes have no edges}
  \item{XKCD}{If set to TRUE the graph is plotted in XKCD style based on http://stackoverflow.com/a/12680841/567015.}
}}

\details{
Because of the amount of arguments the usage of the qgraph function has been reduced by using the ... method for clarity. This does mean that arguments need to be specified by using their exact name. For instance, to specify color="red" you can not use col="red".

Important to note is that qgraph needs to compute in many graphs where the border of nodes are in the plotting area. If the graph is manually rescaled (such as through the "zoom" option in RStudio) the plotting area is changed. This means that the computed location of the border of nodes is no longer valid if the nodes are to remain perfectly square or circular. To overcome this, the \code{usePCH} argument can be used. If this argument is set to \code{FALSE} nodes will be plotted as polygons meaning they will rescale with rescaling the graph (circles can become ovals) and not have perfect resolution in PDF files. If \code{usePCH} is set to \code{TRUE} a default plotting symbol is used meaning the graph can not be rescaled but the node will look good in PDF. By defaut, \code{qgraph} sets \code{usePCH} to \code{TRUE} if it detects the graph is stored in a file. 

While the \code{usePCH} argument makes graphs rescalable it is not a perfect solution. It is highly recommended to NOT RESCALE PLOTTING AREAS when using qgraph, or to rerun qgraph after the plotting area is rescaled. This means using save graph option fro RStudio shoud be avoided in favor of the \code{filetype} argument in \code{qgraph}
}

\section{Using qgraph to plot graphs}{
The first argument of qgraph(), 'input', is the input. This can be a number of objects but is mainly either a weights matrix or an edgelist. Here we will assume a graph is made of n nodes connected by m edges. qgraph is mainly aimed at visualizing (statistical) relationships between variables as weighted edges. In these edge weights a zero indicates no connection and negative values are comparable in strength to positive values. Many (standardized) statistics follow these rules, the most important example being correlations. In the special case where all edge weights are either 0 or 1 the weights matrix is interpreted as an adjacency matrix and an unweighted graph is made.

a weights matrix is a square n by n matrix in which each row and column represents a node. The element at row i and column j indicates the connection from node i to node j. If the weights matrix is symmetrical an undirected graph is made and if the matrix is asymmetrical a directed graph is made.

Alternatively an edgelist can be used. This is a m by 2 matrix (not a list!) in which each row indicates an edge. The first column indicates the number of the start of the edge and the second column indicates the number of the end of the edge. The number of each node is a unique integer between 1 and n. The total number of nodes will be estimated by taking the highest value of the edgelist. If this is incorrect (there are nodes with no edges beyond the ones already specified) the 'nNodes' argument can be used. If an integer between 1 and n is missing in the edgelist it is assumed to be a node with no edges. To create a weighted graph edge weights can be added as a third column in the edgelist. By default using an edgelist creates a directed graph, but this can be set with the 'directed' argument.
}

\section{Interpreting graphs}{
In weighted graphs green edges indicate positive weights and red edges indicate negative weights. The color saturation and the width of the edges corresponds to the absolute weight and scale relative to the strongest weight in the graph. It is possible to set this strongest edge by using the 'maximum' argument. When 'maximum' is set to a value above any absolute weight in the graph that value is considered the strongest edge (this must be done to compare different graphs; a good value for correlations is 1). Edges with an absolute value under the 'minimum' argument are omitted (useful to keep filesizes from inflating in very large  graphs). 

In larger graphs the above edge settings can become hard to interpret. With the 'cut' argument a cutoff value can be set which splits scaling of color and width. This makes the graphs much easier to interpret as you can see important edges and general trends in the same picture. Edges with absolute weights under the cutoff score will have the smallest width and become more colorful as they approach the cutoff score, and edges with absolute weights over the cutoff score will be full red or green and become wider the stronger they are. 
}

\section{Specifying the layout}{
The placement of the nodes (i.e. the layout) is specified with the 'layout' argument. It can be manually specified by entering a matrix for this argument. The matrix must have a row for each node and two columns indicating its X and Y coordinate respectively. qgraph plots the nodes on a (-1:1)(-1:1) plane, and the given coordinates will be rescaled to fit this plane unless 'rescale' is FALSE (not recommended).  Another option to manually specify the layout is by entering a matrix with more then two columns. This matrix must then consist of zeroes and a number (the order in the weights matrix) for each node indicating it's place. For example:

0 0 2 0 0

1 0 3 0 4

will place node 2 at the top in the center, node 1 at the bottom left corner, node 3 at the bottom in the center and node 4 at the bottom right corner. It is recommended however that one of the integrated layouts is used. 'layout' can be given a character as argument to accomplish that. layout="circular" will simply place all nodes in a circle if the groups argument is not used and in separate circles per group if the groups argument is used (see next section).

The circular layout is convenient to see how well the data conforms to a model, but to show how the data clusters another layout is more appropriate. By specifying layout="spring" the Fruchterman-reingold algorithm (Fruchterman & Reingold, 1991), which has been ported from the SNA package (Butts, 2010), can be used to create a force-directed layout. In principle, what this function does is that each node (connected and unconnected) repulse each other, and connected nodes also attract each other. Then after a number of iterations (500 by default) in which the maximum displacement of each node becomes smaller a layout is achieved in which the distance between nodes correspond very well to the absolute edge weight between those nodes.

A solution to use this function for weighted graphs has been taken from the igraph package (Csardi G & Nepusz T, 2006) in which the same function was ported from the SNA package. New in qgraph are the option to include constraints on the nodes by fixing a coordinate for nodes or reducing the maximum allowed displacement per node. This can be done with the 'layout.par' argument. For more information see \code{\link{qgraph.layout.fruchtermanreingold}}.

By default, 'layout' is set to "spring" for unweighted and directed graphs and "circular" otherwise.
}

\section{Grouping nodes}{
Grouping nodes (e.g., according to a measurement model) can be specified with the 'groups' argument. This can be a factor or a list in which each element is a vector containing the numbers of nodes that belong together (numbers are taken from the order in the weights matrix). All numbers must be included. If a groups list is specified the "groups" layout can be used to place these nodes together, the nodes in each group will be given a color, and a legend can be plotted (by setting 'legend' to TRUE). The colors will be taken from the 'color' argument, or be generated with the \code{\link{rainbow}} function.}

\section{Output}{
By default qgraph will plot the graph in a new R window. However the graphs are optimized to be plotted in a PDF file. To easily create a pdf file set the 'filetype' argument to "pdf". 'filename' can be used to specify the filename and folder to output in. 'height' and 'width' can be used to specify the height and width of the image in inches. By default a new R window is opened if the current device is the NULL-device, otherwise the current device is used (note that when doing this 'width' and 'height' still optimize the image for those widths and heights, even though the output screen size isn't affected, this is especially important for directed graphs!).

Furthermore filetype can also be set to numerous other values. Alternatively any output device in R can be used by simply opening the device before calling qgraph and closing it with dev.off() after calling qgraph.

The graphs can also be outputted in an SVG file using the RSVGTipsDevice package (Plate, 2009). An SVG image can be opened in most browsers (firefox and chrome are recommended), and can be used to display tooltips. Each node can be given a tooltip with the 'tooltips' argument. The function \code{\link{qgraph.svg}} can be used to make a battery of svg pictures with hyperlinks to each other, working like a navigation menu (note, RSVGTipsDevice is a 32-bit only package, so SVG functionality is not available in 64bit versions of R).

Finally, the filetype 'tex' can be used. This uses the tikzDevice package to create a LaTeX file that can then be compiled in your LaTeX compiler to create a pdf file. The main benefit of this over plotting directly in a pdf file is that tooltips can be added which can be viewed in several PDF document readers (Adobe Reader is recommended for the best result).

IMPORTANT NOTE: graphs made in qgraph must be exported programatically using device functions such as pdf() and png(). Manually resizing a graph and using export functions such as the one built into RStudio will give UNSTABLE RESULTS.}

\section{Manual specification of color and width}{
In qgraph the widths and colors of each edge can also be manually controlled. To directly specify the width of each edge set the 'mode'' argument to "direct". This will then use the absolute edge weights as the width of each edge (negative values can still be used to make red edges). To manually set the color of each edge, set the 'edge.color' argument to a matrix with colors for each edge (when using a weights matrix) or a vector with a color for each edge (when using an edgelist).
}

\section{Replotting graphs and reusing layouts}{
If the result of \code{\link{qgraph}} is stored, such as \code{Graph <- qgraph(...)}, the plot can be recreated in two ways. \code{qgraph(Graph, ...))} reruns \code{qgraph} with the same arguments used in the origina call except those restated in the dots. For example \code{qgraph(Graph, shape = "square")} will recreate the same plot but now use square nodes instead of circular. \code{plot(Graph)} will NOT rerun \code{qgraph} but simply plot the qgraph object. This means that now specific graph attributes can be changed before plotting.

More specific, \code{qgraph(Graph)} will base the new plot only on the \code{Arguments} element of the \code{qgraph} object and \code{plot(qgraph)} will base the new plot on the \code{graphAttributes} and  \code{plotOptions} elements of the qgraph object.

To reuse a layout, use the \code{layout} element. e.g., to plot a new graph with the same layout use \code{qgraph(..., layout = Graph$layout)}
}

\section{Additional information}{
By default, edges will be straight between two nodes unless there are two edges between two nodes. To overwrite this behavior the 'bidirectional' argument can be set to TRUE, which will turn two edges between two nodes into one bidirectional edge. 'bidirectional' can also be a vector with TRUE or FALSE for each edge.

To specify the strength of the curve the argument 'curve' can be used (but only in directional graphs). 'curve' must be given a numerical value that represent an offset from the middle of the straight edge through where the curved edge must be drawn. 0 indicates no curve, and any other value  indicates a curve of that strength. A value of 0.3 is recommended for nice curves. This can be either one number or a vector with the curve of each edge.

Nodes and edges can be given labels with the 'labels' and the 'edge.labels' arguments. 'labels' can be set to FALSE to omit labels, TRUE (default) to set labels equal to the node number (order in the weights matrix) or it can be a vector with the label for each node. Edge labels can also be set to FALSE to be omitted (default). If 'edge.labels' is TRUE then the weight of each label is printed. Finally, 'edge.labels' can also be a vector with the label for each edge. If a label (both for edges and nodes) contain an asterisk then the asterisk is omitted and that label is printed in the symbol font (useful to print Greek letters).

A final two things to try: the 'scores' argument can be given a vector with the scores of a person on each variable, which will then be shown using colors of the nodes, And the 'bg' argument can be used to change the background of the graph to another color, or use bg=TRUE for a special background (do set transparency=TRUE when using  background colors other then white).}

\section{Debugging}{
If this function crashes for any reason with the filetype argument specified, run:

dev.off()

To shut down the output device!} 
 
\value{
qgraph returns (invisibly) a 'qgraph' object containing:
\item{Edgelist}{A list containing for each edge the node of origin, node of destination, weight en wether the edge is directed and bidirectional.}
\item{Arguments}{A list containing the arguments used in the \code{qgraph} call.}
\item{plotOptions}{A list containing numerous options used in the plotting method.}
\item{graphAttributes}{A list containing numerous attributes for nodes, edges and the entire graph}
\item{layout}{A matrix containing the layout used in the plot}
\item{layout.orig}{A matrix containing the original (unscaled) layout.}
}


\references{
Carter T. Butts <buttsc@uci.edu> (2010). sna: Tools for Social Network Analysis. R package version 2.2-0. http://CRAN.R-project.org/package=sna

Csardi G, Nepusz T (2006). The igraph software package for complex network research, InterJournal, Complex Systems 1695. http://igraph.sf.net

Sacha Epskamp, Angelique O. J. Cramer, Lourens J. Waldorp, Verena D. Schmittmann, Denny Borsboom (2012). qgraph: Network Visualizations of Relationships in Psychometric Data. Journal of Statistical Software, 48(4), 1-18. URL http://www.jstatsoft.org/v48/i04/.

Jerome Friedman, Trevor Hastie and Rob Tibshirani (2011). glasso: Graphical lasso-estimation of Gaussian graphical models. R package version 1.7. http://CRAN.R-project.org/package=glasso

Bernd Klaus and Korbinian Strimmer. (2014). fdrtool: Estimation of (Local) False Discovery Rates and Higher Criticism. R package version 1.2.12. http://CRAN.R-project.org/package=

Fruchterman, T. & Reingold, E. (1991). Graph drawing by force-directed placement. Software - Pract. Exp. 21, 1129-1164.

N. Kraemer, J. Schaefer. A.-L. Boulesteix (2009). Regularized Estimation of Large-Scale Gene Regulatory Networks using Gaussian Graphical Models BMC Bioinformatics 10:384

Plate, T. <tplate@acm.org> and based on RSvgDevice by T Jake Luciani <jakeluciani@yahoo.com> (2009). RSVGTipsDevice: An R SVG graphics device with dynamic tips and hyperlinks. R package version 1.0-1.

Revelle, W. (2014) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, http://CRAN.R-project.org/package=psych Version = 1.4.4.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

\seealso{
\code{\link{cor_auto}}
\code{\link{qgraph.animate}}
\code{\link{qgraph.efa}}
\code{\link{qgraph.pca}}
\code{\link{qgraph.loadings}}
}
\examples{
\dontrun{  
### Correlations ###
# Load big5 dataset:
data(big5)
data(big5groups)

# Compute correlation matrix:
big5_cors <- cor_auto(big5, detectOrdinal = FALSE)

# Correlations:
big5Graph <- qgraph(cor(big5),minimum=0.25,groups=big5groups,
            legend=TRUE,borders=FALSE, title = "Big 5 correlations")


  # Same graph with spring layout:
  qgraph(big5Graph,layout="spring")
  
  # Same graph with different color scheme:
  qgraph(big5Graph,posCol="blue",negCol="purple")
 
  ### Network analysis ###
  ### Using bfi dataset from psych ###
  library("psych")
  data(bfi)
  
  # Compute correlations:
  CorMat <- cor_auto(bfi[,1:25])
  
  # Compute graph with tuning = 0 (BIC):
  BICgraph <- qgraph(CorMat, graph = "glasso", sampleSize = nrow(bfi),
            tuning = 0, layout = "spring", title = "BIC", details = TRUE)
  
  # Compute graph with tuning = 0.5 (EBIC)
  EBICgraph <- qgraph(CorMat, graph = "glasso", sampleSize = nrow(bfi),
            tuning = 0.5, layout = "spring", title = "BIC", details = TRUE)
  
  # Compare centrality and clustering:
  centralityPlot(list(BIC = BICgraph, EBIC = EBICgraph))
  clusteringPlot(list(BIC = BICgraph, EBIC = EBICgraph))

  # Compute centrality and clustering:
  centrality_auto(BICgraph)
  clustcoef_auto(BICgraph)
  
  ### Directed unweighted graphs ###
  set.seed(1)
  adj=matrix(sample(0:1,10^2,TRUE,prob=c(0.8,0.2)),nrow=10,ncol=10)
  qgraph(adj)
  title("Unweighted and directed graphs",line=2.5)
  
  # Save plot to nonsquare pdf file:
  qgraph(adj,filetype='pdf',height=5,width=10)
  
  #### EXAMPLES FOR EDGES UNDER DIFFERENT ARGUMENTS ###
  # Create edgelist:
  dat.3 <- matrix(c(1:15*2-1,1:15*2),,2)
  dat.3 <- cbind(dat.3,round(seq(-0.7,0.7,length=15),1))
  
  # Create grid layout:
  L.3 <- matrix(1:30,nrow=2)
  
  # Different esize:
  qgraph(dat.3,layout=L.3,directed=FALSE,edge.labels=TRUE,esize=14)
  
  # Different esize, strongest edges omitted (note how 0.4 edge is now 
  # just as wide as 0.7 edge in previous graph):
  qgraph(dat.3[-c(1:3,13:15),],layout=L.3,nNodes=30,directed=FALSE,
         edge.labels=TRUE,esize=14)
  
  # Different esize, with maximum:
  qgraph(dat.3,layout=L.3,directed=FALSE,edge.labels=TRUE,esize=14,maximum=1)
  title("maximum=1",line=2.5)
  
  qgraph(dat.3[-c(1:3,13:15),],layout=L.3,nNodes=30,directed=FALSE,edge.labels=TRUE,
         esize=14,maximum=1)
  title("maximum=1",line=2.5)
  
  # Different minimum
  qgraph(dat.3,layout=L.3,directed=FALSE,edge.labels=TRUE,esize=14,minimum=0.1)
  title("minimum=0.1",line=2.5)
  
  # With cutoff score:
  qgraph(dat.3,layout=L.3,directed=FALSE,edge.labels=TRUE,esize=14,cut=0.4)
  title("cut=0.4",line=2.5)
  
  # With details:
  qgraph(dat.3,layout=L.3,directed=FALSE,edge.labels=TRUE,esize=14,minimum=0.1,
         maximum=1,cut=0.4,details=TRUE)
  title("details=TRUE",line=2.5)
  
  
  # Trivial example of manually specifying edge color and widths:
  E <- as.matrix(data.frame(from=rep(1:3,each=3),to=rep(1:3,3),width=1:9))
  qgraph(E,mode="direct",edge.color=rainbow(9))
  
  
  ### Input based on other R objects ###
  
  ## Exploratory factor analysis:
  big5efa <- factanal(big5,factors=5,rotation="promax",scores="regression")
  qgraph(big5efa,groups=big5groups,layout="circle",minimum=0.2,
         cut=0.4,vsize=c(1.5,10),borders=FALSE,vTrans=200,title="Big 5 EFA")
  
  ## Principal component analysis:
  library("psych")
  big5pca <- principal(cor(big5),5,rotate="promax")
  qgraph(big5pca,groups=big5groups,layout="circle",rotation="promax",minimum=0.2,
         cut=0.4,vsize=c(1.5,10),borders=FALSE,vTrans=200,title="Big 5 PCA")
  
  ## pcalg
  # Example from pcalg vignette:
  library("pcalg")
  data(gmI)
  suffStat <- list(C = cor(gmI$x), n = nrow(gmI$x))
  pc.fit <- pc(suffStat, indepTest=gaussCItest,
               p = ncol(gmI$x), alpha = 0.01)
  
  qgraph(pc.fit)
  
  ## glasso:
  # Using bfi dataset from psych:
  library("psych")
  data(bfi)
  cor_bfi <- cor_auto(bfi[,1:25])
  
  # Run qgraph:
  library("glasso")
  bfi_glasso <- glasso(cor_bfi, 0.1)
  
  # Plot:
  qgraph(bfi_glasso, layout = "spring")
  
  
  ## Huge (glasso):
  library("huge")
  bfi_huge <- huge(huge.npn(bfi[,1:25]), method="glasso")
  
  # Auto select optimal and plot:
  qgraph(bfi_huge, layout = "spring")
  
  # Manual select and plot:
  sel <- huge.select(bfi_huge)
  qgraph(sel, layout = "spring")
  
}

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Graphs }
\keyword{ qgraph }
\keyword{ Correlations}

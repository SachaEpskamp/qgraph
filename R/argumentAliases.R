# Alternative (alias) argument names for qgraph().
#
# qgraph historically mixes several naming styles (e.g., 'vsize', 'posCol',
# 'curveAll', 'edge.color'). The names below are consistently styled
# alternatives that may be used instead. The original names remain fully
# supported and take precedence when both are supplied; no warnings are
# raised for either name to keep existing code and published tutorials
# working unchanged.
#
# Format: alias = canonical name.
qgraphArgumentAliases <- c(
  # Nodes:
  "node.size"                  = "vsize",
  "node.size2"                 = "vsize2",
  "node.color"                 = "color",
  "node.shape"                 = "shape",
  "node.names"                 = "nodeNames",
  "node.trans"                 = "vTrans",
  # Edges:
  "edge.connect.points"        = "edgeConnectPoints",
  "pos.color"                  = "posCol",
  "neg.color"                  = "negCol",
  "un.color"                   = "unCol",
  "prob.color"                 = "probCol",
  "neg.dashed"                 = "negDashed",
  "probability.edges"          = "probabilityEdges",
  "col.factor"                 = "colFactor",
  # Arrows:
  "arrow.size"                 = "asize",
  "arrow.angle"                = "arrowAngle",
  # Curvature:
  "curve.all"                  = "curveAll",
  "curve.default"              = "curveDefault",
  "curve.shape"                = "curveShape",
  "curve.scale"                = "curveScale",
  "curve.scale.node.correction" = "curveScaleNodeCorrection",
  "curve.pivot"                = "curvePivot",
  "curve.pivot.shape"          = "curvePivotShape",
  "parallel.edge"              = "parallelEdge",
  "parallel.angle"             = "parallelAngle",
  "parallel.angle.default"     = "parallelAngleDefault",
  "circle.edge.end"            = "CircleEdgeEnd",
  # Layout:
  "layout.scale"               = "layoutScale",
  "layout.offset"              = "layoutOffset",
  "layout.round"               = "layoutRound",
  # Self-loops:
  "loop.rotation"              = "loopRotation",
  # Legend:
  "gl.ratio"                   = "GLratio",
  # Graph estimation and significance:
  "sample.size"                = "sampleSize",
  "count.diagonal"             = "countDiagonal",
  "fdr.cutoff"                 = "FDRcutoff",
  "omit.insig"                 = "OmitInsig",
  "sig.scale"                  = "sigScale",
  # Means, SDs and bars:
  "mean.range"                 = "meanRange",
  "bar.side"                   = "barSide",
  "bar.color"                  = "barColor",
  "bar.length"                 = "barLength",
  "bars.at.side"               = "barsAtSide",
  # Pies:
  "pie.border"                 = "pieBorder",
  "pie.color"                  = "pieColor",
  "pie.color2"                 = "pieColor2",
  "pie.start"                  = "pieStart",
  "pie.darken"                 = "pieDarken",
  "pie.pastel"                 = "piePastel",
  "pie.radius"                 = "pieRadius",
  "pie.CI.lower"               = "pieCIlower",
  "pie.CI.upper"               = "pieCIupper",
  "pie.CI.mid"                 = "pieCImid",
  "pie.CI.point.cex"           = "pieCIpointcex",
  "pie.CI.point.col"           = "pieCIpointcol",
  # Colors and styling:
  "rainbow.start"              = "rainbowStart",
  "use.pch"                    = "usePCH",
  "stand.alone"                = "standAlone",
  # Loadings plots (passed through to qgraph.loadings):
  "resid.edge"                 = "residEdge",
  "resid.scale"                = "residScale",
  # Plotting:
  "do.not.plot"                = "DoNotPlot",
  "html.type"                  = "htmltype"
)

# Replace alias argument names in an argument list with their canonical
# names. If both the alias and the canonical name are present, the canonical
# name takes precedence and the alias is dropped.
resolveArgumentAliases <- function(args, aliases = qgraphArgumentAliases){
  nms <- names(args)
  if (is.null(nms)) return(args)
  isAlias <- nms %in% names(aliases)
  if (!any(isAlias)) return(args)
  for (i in which(isAlias)){
    canonical <- unname(aliases[nms[i]])
    if (!canonical %in% names(args)){
      names(args)[i] <- canonical
    }
  }
  # Drop any remaining alias-named elements (their canonical name was also given):
  args[!names(args) %in% names(aliases)]
}

# Deprecation mapping: old argument names -> new argument names
# All new names follow dot.case convention for consistency
.qgraph_deprecation_map <- list(
  # Node appearance
  "vsize"       = "node.size",
  "vsize2"      = "node.size2",
  "vTrans"      = "node.transparency",

  # Edge appearance
  "esize"       = "edge.size",
  "posCol"      = "positive.color",
  "negCol"      = "negative.color",
  "unCol"       = "unweighted.color",
  "probCol"     = "probability.color",
  "negDashed"   = "negative.dashed",
  "colFactor"   = "color.factor",

  # Arrow
  "asize"       = "arrow.size",
  "arrowAngle"  = "arrow.angle",

  # Output
  "DoNotPlot"   = "do.not.plot",

  # Legend / layout
  "nodeNames"        = "node.names",
  "GLratio"          = "gl.ratio",
  "layoutScale"      = "layout.scale",
  "layoutOffset"     = "layout.offset",

  # Background
  "bgcontrol"   = "bg.control",
  "bgres"       = "bg.resolution",

  # Estimation
  "sampleSize"     = "sample.size",
  "countDiagonal"  = "count.diagonal",
  "FDRcutoff"      = "fdr.cutoff",

  # Edge curvature
  "curveAll"                    = "curve.all",
  "curveDefault"                = "curve.default",
  "curveShape"                  = "curve.shape",
  "curveScale"                  = "curve.scale",
  "curveScaleNodeCorrection"    = "curve.scale.node.correction",
  "curvePivot"                  = "curve.pivot",
  "curvePivotShape"             = "curve.pivot.shape",

  # Parallel edges
  "parallelEdge"          = "parallel.edge",
  "parallelAngle"         = "parallel.angle",
  "parallelAngleDefault"  = "parallel.angle.default",

  # Misc
  "loopRotation"       = "loop.rotation",
  "nNodes"             = "n.nodes",
  "edgeConnectPoints"  = "edge.connect.points",
  "CircleEdgeEnd"      = "circle.edge.end",
  "usePCH"             = "use.pch",
  "noPar"              = "no.par",
  "standAlone"         = "stand.alone",
  "rainbowStart"       = "rainbow.start",
  "preExpression"      = "pre.expression",
  "postExpression"     = "post.expression",
  "probabilityEdges"   = "probability.edges",

  # Bars
  "barSide"     = "bar.side",
  "barColor"    = "bar.color",
  "barLength"   = "bar.length",
  "barsAtSide"  = "bars.at.side",

  # Pies
  "pieBorder"      = "pie.border",
  "pieColor"       = "pie.color",
  "pieColor2"      = "pie.color2",
  "pieStart"       = "pie.start",
  "pieDarken"      = "pie.darken",
  "piePastel"      = "pie.pastel",
  "pieRadius"      = "pie.radius",
  "pieCImid"       = "pie.ci.mid",
  "pieCIlower"     = "pie.ci.lower",
  "pieCIupper"     = "pie.ci.upper",
  "pieCIpointcex"  = "pie.ci.point.cex",
  "pieCIpointcol"  = "pie.ci.point.col",

  # Subplots
  "subplotbg"   = "subplot.bg",

  # Significance
  "sigScale"    = "sig.scale"
)

# Reverse mapping: new name -> old (internal) name
.qgraph_new_to_old <- setNames(
  names(.qgraph_deprecation_map),
  unname(unlist(.qgraph_deprecation_map))
)

#' Handle deprecated argument names in qgraph
#'
#' Processes the dots list, emits deprecation warnings for old names,
#' and maps them to the corresponding old internal names.
#' Also maps new-style explicit arguments to old internal names.
#'
#' @param explicit_args Named list of explicitly provided arguments (new names)
#' @param dots The ... list from qgraph()
#' @return A merged named list using OLD internal names
.handle_qgraph_args <- function(explicit_args, dots) {
  dep_map <- .qgraph_deprecation_map
  new_to_old <- .qgraph_new_to_old

  # 1. Process dots: warn about deprecated old names, keep as-is (old names)
  deprecated_used <- character(0)
  for (nm in names(dots)) {
    if (nm %in% names(dep_map)) {
      new_name <- dep_map[[nm]]
      deprecated_used <- c(deprecated_used, nm)
      # Check if the new name was also explicitly provided
      if (!is.null(explicit_args[[new_name]])) {
        warning(
          sprintf("Both '%s' and '%s' specified; using '%s'.", nm, new_name, new_name),
          call. = FALSE
        )
        # Remove old name from dots, the explicit new name takes precedence
        dots[[nm]] <- NULL
      } else {
        warning(
          sprintf("Argument '%s' is deprecated, use '%s' instead.", nm, new_name),
          call. = FALSE
        )
        # Keep as old name in dots (internal code expects old names)
      }
    }
  }

  # 2. Map explicit args with NEW names to OLD internal names
  mapped_explicit <- list()
  for (nm in names(explicit_args)) {
    val <- explicit_args[[nm]]
    if (is.null(val)) next  # skip NULL (not provided)
    if (nm %in% names(new_to_old)) {
      # This is a new-style name, map to old internal name
      old_nm <- new_to_old[[nm]]
      mapped_explicit[[old_nm]] <- val
    } else {
      # Already uses old/unchanged name
      mapped_explicit[[nm]] <- val
    }
  }

  # 3. Merge: explicit args take precedence over dots
  result <- dots
  for (nm in names(mapped_explicit)) {
    result[[nm]] <- mapped_explicit[[nm]]
  }

  return(result)
}

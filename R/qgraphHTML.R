# Interactive HTML output for qgraph objects (filetype = "html").
#
# The graph is exported with the exact layout and style stored in the qgraph
# object and rendered as SVG by a self-contained HTML file (no external
# dependencies), supporting node dragging, node selection with two highlight
# modes (direct neighborhood and outgoing shortest paths), tooltips, an
# interactive legend and SVG/PNG export.

# Minimal JSON serializer (avoids adding a package dependency):
qgraphJSON <- function(x){
  if (is.null(x)) return("null")
  if (is.list(x) && !is.data.frame(x)){
    nms <- names(x)
    parts <- vapply(x, qgraphJSON, character(1))
    if (!is.null(nms) && all(nms != "")){
      return(paste0("{", paste0(qgraphJSONstring(nms), ":", parts, collapse = ","), "}"))
    }
    return(paste0("[", paste0(parts, collapse = ","), "]"))
  }
  if (length(x) != 1){
    return(paste0("[", paste0(vapply(x, qgraphJSON, character(1)), collapse = ","), "]"))
  }
  if (is.na(x)) return("null")
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.numeric(x)){
    if (!is.finite(x)) return("null")
    return(format(unclass(x), digits = 8, scientific = FALSE, trim = TRUE))
  }
  qgraphJSONstring(as.character(x))
}
qgraphJSONstring <- function(x){
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  # Escape "</" to prevent closing the inline <script> block:
  x <- gsub("</", "<\\/", x, fixed = TRUE)
  paste0("\"", x, "\"")
}

# Convert R colors (names, #RRGGBB, #RRGGBBAA) to CSS rgba() strings:
qgraphHTMLcolor <- function(x){
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  if (any(ok)){
    rgba <- col2rgb(x[ok], alpha = TRUE)
    out[ok] <- ifelse(rgba[4, ] == 255,
                      rgb(rgba[1, ], rgba[2, ], rgba[3, ], maxColorValue = 255),
                      sprintf("rgba(%d,%d,%d,%s)", rgba[1, ], rgba[2, ], rgba[3, ],
                              format(round(rgba[4, ]/255, 3), trim = TRUE)))
  }
  out
}

qgraphHTML <- function(x, filename = "qgraph.html", htmltype = c("rich", "plain"),
                       open = interactive(), verbose = TRUE){
  if (!is(x, "qgraph")) stop("'x' must be a qgraph object")
  htmltype <- match.arg(htmltype)

  Nodes <- x$graphAttributes$Nodes
  Edges <- x$graphAttributes$Edges
  Graph <- x$graphAttributes$Graph
  E <- x$Edgelist
  opts <- x$plotOptions
  layout <- x$layout
  nNodes <- Graph$nNodes
  nEdges <- length(E$from)

  recycle <- function(v, n, default = NA){
    if (is.null(v)) v <- default
    rep(v, length.out = n)
  }

  # User coordinate ranges, matching plot.qgraph (mar is stored divided by 10):
  mar <- recycle(opts$mar, 4, 0.3)
  usr <- c(-1 - mar[2], 1 + mar[4], -1 - mar[1], 1 + mar[3])
  if (!isTRUE(opts$rescale)){
    # Without rescaling the layout keeps its own scale; pad by the margins:
    rx <- range(layout[, 1], finite = TRUE); ry <- range(layout[, 2], finite = TRUE)
    sc <- max(diff(rx), diff(ry), 1e-9)/2
    usr <- c(rx[1] - mar[2]*sc, rx[2] + mar[4]*sc, ry[1] - mar[1]*sc, ry[2] + mar[3]*sc)
  }

  # Node radii in user coordinates, following Cent2Edge at the normalized
  # 7 x 7 inch reference device (csi = 0.2):
  csi <- 0.2
  radX <- (usr[2] - usr[1])/2.16 * csi/17.5
  radY <- (usr[4] - usr[3])/2.16 * csi/17.5

  labels <- Nodes$labels
  if (isTRUE(labels)) labels <- as.character(seq_len(nNodes))
  if (identical(labels, FALSE) || is.null(labels)) labels <- rep(NA_character_, nNodes)
  if (is.list(labels)) labels <- vapply(labels, function(l) paste(deparse(l), collapse = ""), character(1))
  labels <- as.character(labels)

  width  <- recycle(Nodes$width,  nNodes, 1)
  height <- recycle(Nodes$height, nNodes, 1)
  shape  <- recycle(Nodes$shape,  nNodes, "circle")
  borders<- recycle(Nodes$borders, nNodes, TRUE)

  # Node names (nodeNames argument) and tooltips; only used when character:
  nodeNames <- Nodes$names
  if (!is.character(nodeNames)) nodeNames <- rep(NA_character_, nNodes)
  nodeTooltips <- Nodes$tooltips
  if (!is.character(nodeTooltips)) nodeTooltips <- rep(NA_character_, nNodes)

  # Pies:
  pie <- Nodes$pie
  drawPies <- isTRUE(opts$drawPies) && !is.null(pie)
  if (drawPies && !is.list(pie)) pie <- as.list(pie)
  pieColor <- Nodes$pieColor
  if (drawPies && !is.list(pieColor)) pieColor <- as.list(pieColor)

  nodesOut <- lapply(seq_len(nNodes), function(i){
    out <- list(
      x = layout[i, 1], y = layout[i, 2],
      rx = radX * width[i], ry = radY * height[i],
      shape = shape[i],
      color = qgraphHTMLcolor(recycle(Nodes$color, nNodes, "white")[i]),
      border = qgraphHTMLcolor(recycle(Nodes$border.color, nNodes, "black")[i]),
      bwidth = recycle(Nodes$border.width, nNodes, 1)[i],
      hasBorder = isTRUE(borders[i]),
      label = if (is.na(labels[i])) NULL else labels[i],
      lcolor = qgraphHTMLcolor(recycle(Nodes$label.color, nNodes, "black")[i]),
      lcex = recycle(Nodes$label.cex, nNodes, 1)[i],
      lfont = recycle(Nodes$label.font, nNodes, 1)[i],
      name = recycle(nodeNames, nNodes, NA)[i],
      tooltip = recycle(nodeTooltips, nNodes, NA)[i],
      loopRot = recycle(Nodes$loopRotation, nNodes, 0)[i]
    )
    if (drawPies && length(pie) >= i && !all(is.na(pie[[i]]))){
      # always serialized as a JSON array, also for a single value:
      out$pie <- as.list(as.numeric(pie[[i]]))
      pc <- if (is.list(pieColor)) pieColor[[min(i, length(pieColor))]] else pieColor
      out$pieColor <- as.list(qgraphHTMLcolor(recycle(pc, length(out$pie), "grey")))
      out$pieColor2 <- qgraphHTMLcolor(recycle(Nodes$pieColor2, nNodes, "white")[i])
      out$pieBorder <- recycle(Nodes$pieBorder, nNodes, 0.15)[i]
      out$pieStart <- recycle(Nodes$pieStart, nNodes, 0)[i]
    }
    out
  })

  directed <- recycle(E$directed, nEdges, FALSE)
  bidir    <- recycle(E$bidirectional, nEdges, FALSE)
  edgesOut <- lapply(seq_len(nEdges), function(i){
    lab <- recycle(Edges$labels, nEdges, "")[i]
    list(
      from = E$from[i], to = E$to[i], w = E$weight[i],
      dir = isTRUE(directed[i]), bidir = isTRUE(bidir[i]),
      color = qgraphHTMLcolor(recycle(Edges$color, nEdges, "grey")[i]),
      width = recycle(Edges$width, nEdges, 1)[i],
      lty = recycle(Edges$lty, nEdges, 1)[i],
      curve = recycle(Edges$curve, nEdges, 0)[i],
      label = if (is.na(lab) || lab == "") NULL else as.character(lab),
      labelBg = qgraphHTMLcolor(recycle(Edges$label.bg, nEdges, "#FFFFFF")[i]),
      labelCex = recycle(Edges$label.cex, nEdges, 1)[i],
      labelPos = recycle(Edges$edge.label.position, nEdges, 0.5)[i],
      asize = recycle(Edges$asize, nEdges, 2)[i],
      pAngle = recycle(Edges$parallelAngle, nEdges, 0)[i]
    )
  })

  # Groups (legend):
  groupsOut <- NULL
  groups <- Graph$groups
  if (!is.null(groups) && is.list(groups) && !is.null(names(groups)) &&
      length(groups) > 1 && any(names(groups) != "")){
    groupsOut <- list(
      names = as.list(names(groups)),
      colors = as.list(qgraphHTMLcolor(recycle(Graph$color, length(groups), "grey"))),
      # unnamed list of arrays (a named list would serialize as a JSON object):
      members = lapply(unname(groups), function(g) as.list(as.numeric(g)))
    )
  }

  # Polygon shapes (ellipse/heart/star/crown and user-supplied):
  polys <- Graph$polygonList
  polysOut <- NULL
  if (is.list(polys) && length(polys)){
    polysOut <- lapply(polys, function(p) list(x = as.numeric(p$x), y = as.numeric(p$y)))
  }

  background <- opts$background
  if (is.null(background) || !isColor(background)) background <- "white"

  meta <- list(
    htmltype = htmltype,
    background = qgraphHTMLcolor(background),
    title = if (is.null(opts$title)) NULL else as.character(opts$title),
    titleCex = if (is.null(opts$title.cex)) 1 else opts$title.cex,
    usr = usr,
    edgesort = as.numeric(Graph$edgesort),
    arrows = !identical(opts$arrows, FALSE),
    arrowAngle = if (is.null(opts$arrowAngle)) pi/6 else opts$arrowAngle,
    open = isTRUE(opts$open),
    labelProp = if (is.null(opts$label.prop)) 0.765 else opts$label.prop,
    labelCex = 1,
    polygons = polysOut,
    filename = sub("\\.html?$", "", basename(filename))
  )

  data <- list(meta = meta, nodes = nodesOut, edges = edgesOut, groups = groupsOut)

  template <- readLines(system.file("qgraphHTML", "template.html", package = "qgraph"),
                        warn = FALSE, encoding = "UTF-8")
  html <- paste(template, collapse = "\n")
  html <- sub("__QGRAPH_TITLE__", if (is.null(meta$title)) "qgraph" else meta$title, html, fixed = TRUE)
  html <- sub("__QGRAPH_DATA__", qgraphJSON(data), html, fixed = TRUE)

  if (!grepl("\\.html?$", filename)) filename <- paste0(filename, ".html")
  writeLines(html, filename, useBytes = TRUE)
  if (verbose) message(paste0("Output stored in ", normalizePath(filename)))
  if (open) utils::browseURL(paste0("file://", normalizePath(filename)))
  invisible(filename)
}

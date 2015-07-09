makeBW <- function(x, colorlist = NA, plot = TRUE)
{
  # convert a color qgraph in a black and white version
  # x is a qgraph object
  # supports up to 12 different colors for the nodes
  # colorilst = optional argument, a vector of color to guarantee a precise match
  # between colors and fillers

  
  # generate 12  combinations of density & angles as colors are in the original qgraph
  density <- c(10, 20, 30)
  angles <- c(0, 45, 90, 135)
  combos <- data.frame(expand.grid(density, angles))
  names(combos) <- c("density", "angle")
  # reorder to pick up the most different ones first  
  combos <- combos[c(1, 5, 9, 10, 2, 6, 7, 3, 4, 12, 8, 11), ]  
  
  
  # make BW NODES
  clr <- x$graphAttributes$Nodes$color
  if(all(is.na(colorlist)))
      {
    unq <- unique(clr)
  } else { 
  unq <- colorlist
  }
  
  # count how many different colors are there
  N <- length(unq) 
  if(N > 12)
    stop("Too many colors: Black & White qgraphs can be plotted with up to 14 colors")
  

  # define the new combinations of density / angle for each node
  newcol <- data.frame(matrix(ncol= 2, nrow = length(clr)))
  names(newcol) <- names(combos)
  
  for(i in 1:N)
  {
    newcol[clr == unq[i],] <- combos[i,]
  }
  
  
  x$graphAttributes$Nodes$density <- as.numeric(newcol$density)
  x$graphAttributes$Nodes$angles <- as.numeric(newcol$angle)
  x$graphAttributes$Nodes$borders <- rep(TRUE, nrow(newcol))
  x$graphAttributes$Nodes$border.width <- rep(1, nrow(newcol))
  x$plotOptions$usePCH <- FALSE  
#   lblcol <- newcol$color
#   lblcol[newcol$color != "white"] <- "white"
#   lblcol[newcol$color == "white"] <- "black"
#   x$graphAttributes$Nodes$label.color <- lblcol
  
  
  # make BW EDGES
  x$graphAttributes$Edges$lty <- 2-as.numeric(x$Edgelist$weight >= 0)
  

  # plot and return the qgraph object
  if(plot) plot(x)
  invisible(x)
}
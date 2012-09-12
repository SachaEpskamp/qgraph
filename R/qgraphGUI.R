
qgraph.gui <- function(input,...)
{
	if (!require("rpanel")) stop("Package 'rpanel' is required to use GUI functionality")

  # Dummies to fool R CMD check:
  graph <- minimum <- maximum <- esize <- vsize <- asize < graph <- cbox <- filename <- dimensions <- NULL
	
  qgraph.setup <- function(panel) panel
  qgraph.draw <- function(panel) {
    panel$details <- panel$cbox[1]
	panel$bg <- panel$transparency <-panel$cbox[2]
	panel$overlay <- panel$cbox[3]
	panel$borders <- panel$cbox[4]
	panel$legend <- panel$cbox[5]
	panel$width <- as.numeric(panel$dimensions[1])
	panel$height <- as.numeric(panel$dimensions[2])
    do.call(qgraph,panel)
    panel
  }
  qgraph.newplot <- function(panel)
  {
	qgraph.draw(c(panel,filetype="R"))
	panel
  }
  qgraph.save <- function(panel) {
    panel$details <- panel$cbox[1]
	panel$bg <- panel$transparency <- panel$cbox[2]
	panel$overlay <- panel$cbox[3]
	panel$borders <- panel$cbox[4]
	panel$legend <- panel$cbox[5]
	panel$width <- as.numeric(panel$dimensions[1])
	panel$height <- as.numeric(panel$dimensions[2])
    do.call(qgraph,c(panel,filetype="pdf",filename="qgraph%03d"))
    panel
  }
  
  qgraph.panel <- rp.control("qgraph GUI", input = input, ...)
  rp.slider(qgraph.panel, minimum, 0, 1 , qgraph.setup, "Minimum", 	initval = 0, showvalue = TRUE, pos = list(column=0,row=0))
  rp.slider(qgraph.panel, cut, 0, 1 , qgraph.setup, "Cutoff", initval = 0.4, showvalue = TRUE, pos = list(column=0,row=1))
  rp.slider(qgraph.panel, maximum, 0, 1 , qgraph.setup, "Maximum", initval = 1, showvalue = TRUE, pos = list(column=0,row=2))

   rp.slider(qgraph.panel, esize, 0, 20 , qgraph.setup, "Edge width", initval = 10, showvalue = TRUE,  pos = list(column=1,row=0))
  rp.slider(qgraph.panel, vsize, 0, 10, qgraph.setup, "Node size", initval = 2, showvalue = TRUE, pos = list(column=1,row=1))
  rp.slider(qgraph.panel, asize, 1, 10 , qgraph.setup, "Arrow size", initval = 2, showvalue = TRUE, pos = list(column=1,row=2))
  
  rp.radiogroup(qgraph.panel, graph, c("association", "concentration", "factorial"), title = "Graph", action = qgraph.setup, pos = list(column=0,row=3))
	rp.radiogroup(qgraph.panel, layout, c("circular", "spring"), title = "Layout", action = qgraph.setup, pos = list(column=0,row=4))

	rp.checkbox(qgraph.panel, cbox,qgraph.setup, labels = c("Details","Background", "Overlay","Borders","Legend"), title="Options", pos = list(column=1,row=3), initval=c(FALSE,FALSE,FALSE,TRUE,TRUE))

	rp.textentry(qgraph.panel, filename, qgraph.setup, initval = "qgraph",  pos = list(column=0,row=5),title="Filename (enter to confirm)")
	
	rp.textentry(qgraph.panel, dimensions, qgraph.setup, initval = c(7,7),  pos = list(column=1,row=4),labels = c("Width","Height"),title="Dimensions (enter to confirm)")
	
	rp.button(qgraph.panel, action = qgraph.draw, title = "Plot", ,pos = list(column=0,row=6))	
	
	rp.button(qgraph.panel, action = qgraph.newplot, title = "New" ,pos = list(column=1,row=6))	

	rp.button(qgraph.panel, action = qgraph.save, title = "Write pdf", pos = list(column=1,row=5))
  
}

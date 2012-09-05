

pathDiagramGUI <- function(object,...)
{
  if (!require("rpanel")) stop("Package 'rpanel' is required to use GUI functionality")
  
  object <- qgraphSEM(object)
  
  Groups <- unique(object@RAM$group)
  Ng <- length(Groups)
  
    qgraph.setup <- function(panel) panel
  qgraph.draw <- function(panel) {
    panel$residuals <- panel$cbox[1]
    panel$means <- panel$cbox[2]
    panel$width <- as.numeric(panel$dimensions[1])
    panel$height <- as.numeric(panel$dimensions[2])    
    panel$include <- which(panel$inclGroups)
    if (panel$GroupOr=="Horizontal") layout(t(1:sum(panel$inclGroups))) else layout(1:sum(panel$inclGroups))
    do.call(pathDiagram,panel)
    panel
  }
  qgraph.newplot <- function(panel)
  {
    qgraph.draw(c(panel,filetype="R"))
    panel
  }
  qgraph.save <- function(panel) {
    panel$residuals <- panel$cbox[1]
    panel$means <- panel$cbox[2]
    panel$width <-  par("din")[1]
    panel$height <- par("din")[2]
    panel$include <- which(panel$inclGroups)
    pdf(paste0(panel$filename,".pdf"),panel$width,panel$height)
    if (panel$GroupOr=="Horizontal") layout(t(1:sum(panel$inclGroups))) else layout(1:sum(panel$inclGroups))
    do.call(pathDiagram,c(panel))
    dev.off()
    message(paste("Output stored in",paste0(getwd(),"/",panel$filename,".pdf")))
    panel
  }
  
  qgraph.panel <- rp.control("qgraph GUI", object = object, ask = FALSE, GroupOr = "Verical", inclGroups = rep(TRUE,Ng), ...)
  
  
  rp.radiogroup(qgraph.panel, what, c("Model","Estimates","Standardized","Equality","Color"), title = "Edges", action = qgraph.setup, pos = list(column=0,row=0), initval = "Model")
  
  rp.radiogroup(qgraph.panel, whatLabels, c("Labels","Estimates","Standardized","Equality","Hide"), title = "Labels", action = qgraph.setup, pos = list(column=1,row=0), initval = "Labels")
  
# 
#   rp.slider(qgraph.panel, minimum, 0, 1 , qgraph.setup, "Minimum", 	initval = 0, showvalue = TRUE, pos = list(column=0,row=0))
#   rp.slider(qgraph.panel, cut, 0, 1 , qgraph.setup, "Cutoff", initval = 0.4, showvalue = TRUE, pos = list(column=0,row=1))
#   rp.slider(qgraph.panel, maximum, 0, 1 , qgraph.setup, "Maximum", initval = 1, showvalue = TRUE, pos = list(column=0,row=2))
#   
#   rp.slider(qgraph.panel, esize, 0, 20 , qgraph.setup, "Edge width", initval = 10, showvalue = TRUE,  pos = list(column=1,row=0))
#   rp.slider(qgraph.panel, vsize, 0, 10, qgraph.setup, "Node size", initval = 2, showvalue = TRUE, pos = list(column=1,row=1))
#   rp.slider(qgraph.panel, asize, 1, 10 , qgraph.setup, "Arrow size", initval = 2, showvalue = TRUE, pos = list(column=1,row=2))
#   
  rp.radiogroup(qgraph.panel, style, c("OpenMx", "LISREL"), title = "Style", action = qgraph.setup, pos = list(column=0,row=1), initval="OpenMx")
  
  rp.checkbox(qgraph.panel, cbox,qgraph.setup, labels = c("Residuals","Means"), title="Show", pos = list(column=1,row=1), initval=c(TRUE, TRUE)) 
  
  rp.radiogroup(qgraph.panel, layout, c("tree", "spring", "circle"), title = "Layout", action = qgraph.setup, pos = list(column=0,row=2), initval="tree")
#   
#   rp.textentry(qgraph.panel, dimensions, qgraph.setup, initval = c(7,7),  pos = list(column=1,row=2),label=c("Width","Height"),title="Dimensions")
#   
#   
  rp.textentry(qgraph.panel, filename, qgraph.setup, initval = "qgraph",  pos = list(column=1,row=3),title="Filename")
  
  rp.button(qgraph.panel, action = qgraph.save, title = "Write pdf", pos = list(column=0,row=3))
  
  
  rp.button(qgraph.panel, action = qgraph.draw, title = "Plot", ,pos = list(column=0,row=4))	
  
  rp.button(qgraph.panel, action = qgraph.newplot, title = "New" ,pos = list(column=1,row=4))
  
  rp.slider(qgraph.panel, rotation, 1, 4 , qgraph.setup, "Rotation",   initval = 1,
            resolution = 1, showvalue = TRUE, pos = list(column=1,row=2))
# 
#   
#   qgraph.printcall <- function(panel)
#   {
#     cat("pathDiagram(",paste(paste(names(panel),"=",sapply(panel,function(x)paste(deparse(dput(x)),collapse=""))),collapse=","),")")
#     return(panel)
#   }
#   
#   rp.button(qgraph.panel, action = qgraph.printcall, title = "Print call" ,pos = list(column=0,row=5))
#   
  if (Ng>1)
  {
    rp.radiogroup(qgraph.panel, GroupOr, c("Horizontal", "Vertical"), title = "Orientation", action = qgraph.setup, pos = list(column=0,row=6), initval="Vertical")
    
    rp.checkbox(qgraph.panel, inclGroups, qgraph.setup, labels = Groups, title="Include", pos = list(column=1,row=6), initval=rep(TRUE,Ng))
  }
  

  
}


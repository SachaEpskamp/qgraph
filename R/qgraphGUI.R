
qgraph.gui <- function(input,corMat,...)
{
  
  stop("This function has been removed in favor of our Shiny app: https://jolandakos.shinyapps.io/NetworkApp/")
#   
# 	if (!require("rpanel")) stop("Package 'rpanel' is required to use GUI functionality")
# 
#   ## CHECK FOR CORRELATION MATRIX ###:
#   if (missing(corMat))
#   {
#     if (is.matrix(input) && isSymmetric(input) && all(diag(input)==1) && all(abs(input)<=1))
#     {
#       corMat <- TRUE
#     } else corMat <- FALSE
#   }
#   
#   if (!is.logical(corMat)) stop("'corMat' must be logical")
#   
# 	if (any(grepl("RStudio", .libPaths(), ignore.case=TRUE)))
# 	{
# 	  if (grepl("win",Sys.info()["sysname"],ignore.case=TRUE))
# 	  {
# 	    windows()
# 	  } else X11()
# 	} else
# 	{
# 	  dev.new()
# 	}
#   # Dummies to fool R CMD check:
#   graph <- minimum <- maximum <- esize <- vsize <- asize <- graph <- cbox <- filename <- dimensions <- OnTheFly <- LatSize <- GraphType <- FAopts <- File <- Control <- NULL
# 	
#   ### Correlation matrix GUI:
#   if (corMat)
#   {
#     covMat <- input
#     if (any(diag(input)!=1)) input <- round(cov2cor(input),12)
#     
#     qgraph.setup <- function(panel) {
#       if (isTRUE(panel$OnTheFly)) qgraph.draw(panel)
#       panel
#     }
#     
#     qgraph.draw <- function(panel) {
#       panel$details <- panel$cbox[1]
#   	panel$bg <- panel$transparency <-panel$cbox[2]
#   	panel$overlay <- panel$cbox[3]
#   	panel$borders <- panel$cbox[4]
#   	panel$legend <- panel$cbox[5]
#   # 	panel$width <- as.numeric(panel$dimensions[1])
#   # 	panel$height <- as.numeric(panel$dimensions[2])
#       if (panel$GraphType=="EFA")
#       {
#         panel2 <- panel
#         panel2$vsize <- c(panel$vsize,panel$LatSize)
#         panel2$dat <- covMat
#         panel2$factors <- as.numeric(panel$FAopts[1])
#         panel2$rotation <- panel$FAopts[2]
#         do.call(qgraph.efa,panel2)
#         panel2$graph <- NULL
#       } else if (panel$GraphType == "PCA")
#       {
#         panel2 <- panel
#         panel2$vsize <- c(panel$vsize,panel$LatSize)
#         panel2$cor <- covMat
#         panel2$factors <- as.numeric(panel$FAopts[1])
#         panel2$rotation <- panel$FAopts[2]
#         panel2$graph <- NULL
#         do.call(qgraph.pca,panel2)
#       } else
#       {
#         panel$graph <- panel$GraphType
#         do.call(qgraph,panel)
#       }
#       panel
#     }
#     qgraph.newplot <- function(panel)
#     {
#       if (any(grepl("RStudio", .libPaths(), ignore.case=TRUE)))
#       {
#         if (grepl("win",Sys.info()["sysname"],ignore.case=TRUE))
#         {
#           windows()
#         } else X11()
#       } else
#       {
#         dev.new()
#       }
#   	qgraph.draw(panel)
#   	panel
#     }
#     
#     qgraph.save <- function(panel) {
#       panel2 <- panel
#       panel2$width <-  par("din")[1]
#       panel2$height <- par("din")[2]
#       panel2$filename <- panel$File[1]
#       panel2$filetype <- panel$File[2]
#       qgraph.draw(panel2)
#       panel
#     }
#     
#     qgraph.panel <- rp.control("qgraph GUI", input = input, ...)
#     
#     rp.checkbox(qgraph.panel, OnTheFly ,qgraph.setup, title="Plot on the fly", pos = list(column=0,row=0), initval=FALSE)
#     
#     rp.slider(qgraph.panel, minimum, 0, 1 , qgraph.setup, "Minimum", 	initval = 0, showvalue = TRUE, pos = list(column=0,row=1))
#     rp.slider(qgraph.panel, cut, 0, 1 , qgraph.setup, "Cutoff", initval = 0.4, showvalue = TRUE, pos = list(column=0,row=2))
#     rp.slider(qgraph.panel, maximum, 0, 1 , qgraph.setup, "Maximum", initval = 1, showvalue = TRUE, pos = list(column=0,row=3))
#   
#      rp.slider(qgraph.panel, esize, 0, 20 , qgraph.setup, "Edge width", initval = 4, showvalue = TRUE,  pos = list(column=1,row=1))
#     rp.slider(qgraph.panel, vsize, 0, 20, qgraph.setup, "Node size", initval = 2, showvalue = TRUE, pos = list(column=1,row=2))
#     rp.slider(qgraph.panel, LatSize, 1, 20 , qgraph.setup, "Latent size (FA)", initval = 5, showvalue = TRUE, pos = list(column=1,row=3))
#     
#     rp.radiogroup(qgraph.panel, GraphType, c("association", "concentration", "factorial", "EFA", "PCA"), title = "Graph", action = qgraph.setup, pos = list(column=0,row=4))
#     
#     
#   	rp.textentry(qgraph.panel, FAopts, qgraph.setup, initval = c("1","promax"),  pos = list(column=1,row=4),title="FA Options", labels = c("# Factors","Rotation"))  
#     
#     
#   	rp.radiogroup(qgraph.panel, layout, c("circular", "spring", "tree"), title = "Layout", action = qgraph.setup, pos = list(column=0,row=5))
#   
#   	rp.checkbox(qgraph.panel, cbox,qgraph.setup, labels = c("Details","Background", "Overlay","Borders","Legend"), title="Options", pos = list(column=1,row=5), initval=c(FALSE,FALSE,FALSE,TRUE,TRUE))
#   
#   	rp.textentry(qgraph.panel, File, qgraph.setup, initval = c("qgraph","pdf"),  pos = list(column=1,row=6),title="Output", labels = c("Name","Type"))
#   	
#   # 	rp.textentry(qgraph.panel, dimensions, qgraph.setup, initval = c(7,7),  pos = list(column=1,row=4),labels = c("Width","Height"),title="Dimensions (enter to confirm)")
#   	
#   	rp.button(qgraph.panel, action = qgraph.draw, title = "Plot", ,pos = list(column=0,row=7))	
#   	
#   	rp.button(qgraph.panel, action = qgraph.newplot, title = "New" ,pos = list(column=1,row=7))	
#   
#   	rp.button(qgraph.panel, action = qgraph.save, title = "Save", pos = list(column=0,row=6))
#     
#   } else {
#     # Default GUI:
#     
#     qgraph.setup <- function(panel) {
#       if (isTRUE(panel$OnTheFly)) qgraph.draw(panel)
#       panel
#     }
#     
#     qgraph.draw <- function(panel) {
#       if (panel$Control[1]!="") {
#         panel$minimum <- as.numeric(panel$Control[1])
#       } else {
#         panel$minimum <- NULL
#       }
#       
#       if (panel$Control[2]!="") {
#         panel$cut <- as.numeric(panel$Control[1])
#       } else {
#         panel$cut <- NULL
#       }      
#       
#       if (panel$Control[3]!="") {
#         panel$maximum <- as.numeric(panel$Control[3])
#       } else {
#         panel$maximum <- NULL
#       }
#       
#       if (panel$Control[4]!="") {
#         panel$esize <- as.numeric(panel$Control[4])
#       } else {
#         panel$esize <- NULL
#       }
#       
#       if (panel$Control[5]!="") {
#         panel$vsize <- as.numeric(panel$Control[5])
#       } else {
#         panel$vsize <- NULL
#       }
#       
#       if (panel$Control[6]!="") {
#         panel$asize <- as.numeric(panel$Control[6])
#       } else {
#         panel$asize <- NULL
#       }
#       
#       panel$details <- panel$cbox[1]
#       panel$bg <- panel$transparency <-panel$cbox[2]
#       panel$overlay <- panel$cbox[3]
#       panel$borders <- panel$cbox[4]
#       panel$legend <- panel$cbox[5]
#       do.call(qgraph,panel)
#       panel
#     }
#     qgraph.newplot <- function(panel)
#     {
#       if (any(grepl("RStudio", .libPaths(), ignore.case=TRUE)))
#       {
#         if (grepl("win",Sys.info()["sysname"],ignore.case=TRUE))
#         {
#           windows()
#         } else X11()
#       } else
#       {
#         dev.new()
#       }
#       qgraph.draw(panel)
#       panel
#     }
#     
#     qgraph.save <- function(panel) {
#       panel2 <- panel
#       panel2$width <-  par("din")[1]
#       panel2$height <- par("din")[2]
#       panel2$filename <- panel$File[1]
#       panel2$filetype <- panel$File[2]
#       qgraph.draw(panel2)
#       panel
#     }
#     
#     qgraph.panel <- rp.control("qgraph GUI", input = input, ...)
#     
#     rp.checkbox(qgraph.panel, OnTheFly ,qgraph.setup, title="Plot on the fly", pos = list(column=0,row=0), initval=FALSE)
#     
#     rp.textentry(qgraph.panel, Control, qgraph.setup, initval = rep("",6),  pos = list(column=1,row=1),title="Control", labels = c("Minimum","Cut","Maximum","Edge Width", "Node Size", "Arrow Size"))
#     
#     
#     rp.checkbox(qgraph.panel, cbox,qgraph.setup, labels = c("Details","Background", "Overlay","Borders","Legend"), title="Options", pos = list(column=0,row=1), initval=c(FALSE,FALSE,FALSE,TRUE,TRUE))
#     
#     rp.radiogroup(qgraph.panel, layout, c("circular", "spring"), title = "Layout", action = qgraph.setup, pos = list(column=1,row=2))
#     
#     
#     rp.textentry(qgraph.panel, File, qgraph.setup, initval = c("qgraph","pdf"),  pos = list(column=1,row=3),title="Output", labels = c("Name","Type"))
#     
#     # 	rp.textentry(qgraph.panel, dimensions, qgraph.setup, initval = c(7,7),  pos = list(column=1,row=4),labels = c("Width","Height"),title="Dimensions (enter to confirm)")
#     
#     rp.button(qgraph.panel, action = qgraph.draw, title = "Plot", ,pos = list(column=0,row=4))	
#     
#     rp.button(qgraph.panel, action = qgraph.newplot, title = "New" ,pos = list(column=1,row=4))	
#     
#     rp.button(qgraph.panel, action = qgraph.save, title = "Save", pos = list(column=0,row=3))
#   }
}


### SINGLE GROUP ###
setMethod("pathDiagram.S4",signature("qgraph.semModel"),function(object,style,layout="tree",means=TRUE,horizontal=TRUE,curve,edge.labels=TRUE,nCharNodes=3,nCharEdges=3,ask,mar,title=TRUE,...){
  
  # Defaults:
  if (missing(mar))
  {
    if (title) mar <- c(3,3,6,3) else mar <- c(3,3,3,3)
  }
  
  if (missing(curve))
  {
    if (layout == "tree")
    {
      curve <- 0.2
    } else {
      curve <- 0
    }
  }
  
  # Set and check style: 
  if (missing(style)) style <- "OpenMx"
  if (style!="OpenMx") stop("Only OpenMx style is currently supported.")
  
  # Remove means if means==FALSE
  if (means==FALSE)
  {
    object@RAM <- object@RAM[object@RAM$edge!="int",]
  }
  
  # Add rows for bidirectional edges:
  if (any(object@RAM$edge=="<->" & object@RAM$lhs != object@RAM$rhs))
  {
    bidirs <- object@RAM[object@RAM$edge=="<->" & object@RAM$lhs != object@RAM$rhs,]
    bidirs[c("lhs","rhs")] <- bidirs[c("rhs","lhs")]
    object@RAM <- rbind(object@RAM,bidirs)
  }
  
  # Extract names:
  manNames <- object@Vars$name[object@Vars$manifest]
  latNames <- object@Vars$name[!object@Vars$manifest]
  Labels <- c(manNames,latNames)
  nM <- length(manNames)
  nL <- length(latNames)
  
  # Add intercept:
  if (any(object@RAM$edge=="int")) 
  {
    Labels[Labels=="1"] <- "_1"
    Labels <- c(Labels,"1")
  }
  nN <- length(Labels)
  
  Groups <- unique(object@RAM$group)
  qgraphRes <- list()
  if (missing(ask))
  {
    if (length(Groups)>1) ask <- TRUE else ask <- FALSE
  }
  askOrig <- par("ask")
  
  par(ask=ask)
  for (gr in Groups)
  {
    GroupRAM <- object@RAM[object@RAM$group==gr,]
    
    # Extract edgelist:
    Edgelist <- GroupRAM[c("lhs","rhs")]
    Edgelist$lhs <- match(Edgelist$lhs,Labels)
    Edgelist$lhs[GroupRAM$edge=="int"] <- nN
    Edgelist$rhs <- match(Edgelist$rhs,Labels)
    
    # Coerce to numeric matrix:
    Edgelist$lhs <- as.numeric(Edgelist$lhs)
    Edgelist$rhs <- as.numeric(Edgelist$rhs)
    Edgelist <- as.matrix(Edgelist)
    
    # Bidirectional:
    Bidir <- GroupRAM$edge == "<->"
    
    # lty:
    lty <- ifelse(GroupRAM$fixed,2,1)
    
    # Shape:
    Shape <- c(rep("square",nM),rep("circle",nL))
    if (any(GroupRAM$edge=="int")) Shape <- c(Shape,"triangle")
    
    Curve <- curve
      
    # Layout:
    if (layout=="tree")
    {
      # Curves:
      Curve <- ifelse(GroupRAM$lhs != GroupRAM$rhs & ((GroupRAM$lhs%in%manNames & GroupRAM$rhs%in%manNames) | (GroupRAM$lhs%in%latNames & GroupRAM$rhs%in%latNames)),curve,0)
      Curve <- ifelse(GroupRAM$lhs%in%manNames,-1*Curve,Curve)
      Curve <- ifelse(GroupRAM$edge=="int" & GroupRAM$rhs%in%latNames,-1*curve,Curve)
      
    
      # Empty layout:
      Layout <- matrix(,length(Labels),2)
      
      # Add vertical levels:
      Layout[,2] <- ifelse(Labels%in%manNames,-1,1)
      
      # Add vertical levels:
      Layout[Labels%in%manNames,1] <- seq(-1,1,length=nM)
      if (any(GroupRAM$edge=="int"))
      {
        sq <- seq(-1,1,length=nL+1)
        cent <- floor(median(1:(nL+1)))
        Layout[!Labels%in%manNames,1] <- sq[c(which(1:(nL+1) < cent),which(1:(nL+1) > cent),cent)]
      } else
      {
        Layout[Labels%in%latNames,1] <- seq(-1,1,length=nL)
      }
      
      if (!horizontal) 
      {
        Layout <- Layout[,2:1]
        Layout[,1] <- -1 * Layout[,1]
      }
    } else Layout <- layout
    
    # loopRotation:
    if (layout=="tree")
    {
      loopRotation <- ifelse(Labels%in%manNames,pi,0)
      if (!horizontal) loopRotation <- loopRotation - 0.5*pi
    } else loopRotation <- NULL
    
    # Edge labels:
    if (edge.labels)
    {
      eLabels <- GroupRAM$label
    } else eLabels <- rep("",nrow(Edgelist))
    
    # Abbreviate:
    if (nCharEdges>0)
    {
      eLabels <- abbreviate(eLabels,nCharEdges)
    }
    if (nCharNodes>0)
    {
      Labels <- abbreviate(Labels,nCharNodes)
    }
    
    qgraphRes[[which(Groups==gr)]] <- qgraph(Edgelist,
           labels=Labels,
           bidirectional=Bidir,
           shape=Shape,
           layout=Layout,
           lty=lty,
           loopRotation=loopRotation,
           curve=Curve,
           edge.labels=eLabels,
           mar=mar,
            ...)
    
    if (title)
    {
      if (length(Groups)==1) title("Path Diagram",line=3) else title(paste0("Path Diagram for group '",gr,"'"),line=3)
    }
  }
  par(ask=askOrig)
  if (length(qgraphRes)==1) qgraphRes <- qgraphRes[[1]]
  invisible(qgraphRes)
  })
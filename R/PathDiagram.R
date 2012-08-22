# Arguments:
# rotation: 1 = normal (endo manifests under), 2 3 and 4 flip counterclockwise.
# 2 = endo manifests righy
# 3 = endo man up
# 4 = endo man left

# manifests: vector of manifest labels ordered
# latents: vector of latents ordered

mixCols <- function(x,w)
{
  # x = vector of colors
  # w = weights
  if (missing(w)) w <- rep(1,length(x))
  
  RGB <- col2rgb(x)
  wMeans <- apply(RGB,1,weighted.mean,w=w)
  return(rgb(wMeans[1],wMeans[2],wMeans[3],maxColorValue=255))
}

loopOptim <- function(x,Degrees)
{
  NotinRange <- sum(sapply(Degrees,function(d)!any(c(d,d-2*pi,d+2*pi)>(x-pi/4) & c(d,d-2*pi,d+2*pi)<(x+pi/4))))
  Dist2Edges <- sapply(Degrees,function(d)min(abs(x - c(d,d-2*pi,d+2*pi))))
  NotinRange * 2 * pi * 2 + sum(sort(Dist2Edges)[1:2])
}

RotMat <- function(d) matrix(c(cos(-d),sin(-d),-sin(-d),cos(-d)),2,2)

mixInts <- function(vars,intMap,Layout,trim=FALSE,residuals=TRUE)
{
  n <- length(vars)
  
  if (residuals)
  {
    if (!trim)
    {
      if (n+nrow(intMap)==1)
      {
        sq <- 0
      }
      if (n+nrow(intMap) == 2)
      {
        sq <- c(0,0.5) 
      } else {
      sq <- seq(-1,1,length=n+nrow(intMap))
      }
    } else {
      if (n+nrow(intMap) == 2)
      {
        sq <- c(0,0.5) 
      } else {
        sq <- seq(-1,1,length=n+nrow(intMap)+2)[-c(1,n+nrow(intMap)+2)]
      }
    }
    cent <- median(1:n)
    c <- 1
    for (i in seq_along(vars))
    {
      if (vars[i]%in%intMap[,2])
      {
        if (i < cent)
        {
          Layout[intMap[intMap[,2]==vars[i],1],1] <- sq[c]
          Layout[vars[i],1] <- sq[c+1]
          c <- c+2
        } else
        {
          Layout[intMap[intMap[,2]==vars[i],1],1] <- sq[c+1]
          Layout[vars[i],1] <- sq[c]
          c <- c+2                   
        }
      } else
      {
        Layout[vars[i],1] <- sq[c]
        c <- c+1
      }
    }
  } else {
    if (!trim)
    {
      if (n==1)
      {
        sq <- 0
      }
      if (n == 2)
      {
        sq <- c(-1,1) 
      } else {
        sq <- seq(-1,1,length=n)
      }
    } else {
      if (n == 1)
      {
        sq <- 0
      }
      if (n == 2)
      {
        sq <- c(-0.5,0.5) 
      } else {
        sq <- seq(-1,1,length=n+2)[-c(1,n+2)]
      }
    }
    c <- 1
    for (i in seq_along(vars))
    {
      if (vars[i]%in%intMap[,2])
      {
        Layout[intMap[intMap[,2]==vars[i],1],1] <- sq[c]
        Layout[vars[i],1] <- sq[c]
        c <- c + 1 
      } else
      {
        Layout[vars[i],1] <- sq[c]
        c <- c+1
      }
    }    
  }
  return(Layout)
}


setMethod("pathDiagram.S4",signature("qgraph.semModel"),function(object,what="paths",whatLabels,style,layout="tree",means=TRUE,residuals=TRUE,meanStyle="multi",rotation=1,curve,nCharNodes=3,nCharEdges=3,sizeMan = 5,sizeLat = 8,sizeInt = 2,ask,mar,title=TRUE,include,manifests,latents,groups,color,resScale,...){

  

  # Check:
  if (!rotation%in%1:4)
  {
    stop("Rotation must be 1, 2 3 or 4.")
  }
  if (any(object@RAM$edge=="int")) 
  {
    object@Vars$name[object@Vars$name=="1"] <- "_1"
  }
  
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
  
  if (missing(whatLabels))
  {
    edge.labels <- TRUE
  } else
  {
    edge.labels <- FALSE    
  }
  
  # Set and check style: 
  if (missing(style)) style <- "OpenMx"
  if (!grepl("mx|lisrel",style,ignore.case=TRUE)) stop("Only OpenMx  or LISREL style is currently supported.")
  if (grepl("mx",style,ignore.case=TRUE) & !missing(resScale)) warning("'resScale' ingored in OpenMx style")
  if (missing(resScale)) resScale <- 1
  
  # Remove means if means==FALSE
  if (means==FALSE)
  {
    object@RAM <- object@RAM[object@RAM$edge!="int",]
  }
  # Remove residuals if residuals=FALSE
  if (residuals==FALSE)
  {
    object@RAM <- object@RAM[!(object@RAM$edge=="<->"&object@RAM$lhs==object@RAM$rhs),]
  }  
  
  # Add rows for bidirectional edges:
  if (any(object@RAM$edge=="<->" & object@RAM$lhs != object@RAM$rhs))
  {
    bidirs <- object@RAM[object@RAM$edge=="<->" & object@RAM$lhs != object@RAM$rhs,]
    bidirs[c("lhs","rhs")] <- bidirs[c("rhs","lhs")]
    bidirs$par <- -1
    object@RAM <- rbind(object@RAM,bidirs)
  }
  object@RAM <- object@RAM[!duplicated(object@RAM),]
  
  # Extract names:
  manNames <- object@Vars$name[object@Vars$manifest]
  if (!missing(manifests))
  {
    if (!(all(manNames%in%manifests) & length(manifests) == length(manNames)))
    {
      stop(paste("Argument 'manifests' should be a vector containing reordered elements of the vector",dput(manNames)))
    }
    manNames <- manifests
  }
  latNames <- object@Vars$name[!object@Vars$manifest]
  if (!missing(latents))
  {
    if (!(all(latNames%in%latents) & length(latents) == length(latNames)))
    {
      stop(paste("Argument 'latents' should be a vector containing reordered elements of the vector",dput(latNames)))
    }
    latNames <- latents
  }
  Labels <- c(manNames,latNames)
  nM <- length(manNames)
  nL <- length(latNames)
  
  # Define groups and colors setup:
  if (!missing(groups))
  {
    if (is.character(groups))
    {
      if (any(grepl("man",groups,ignore.case=TRUE)) & any(grepl("lat",groups,ignore.case=TRUE)))
      {
        groups <- as.list(c(manNames,latNames))
      } else if (any(grepl("man",groups,ignore.case=TRUE)) & !any(grepl("lat",groups,ignore.case=TRUE)))
      {
        groups <- as.list(manNames)
      } else if (!any(grepl("man",groups,ignore.case=TRUE)) & any(grepl("lat",groups,ignore.case=TRUE)))
      {
        groups <- as.list(latNames)
      } else stop("Character specification of 'groups' must contain 'man','lat' or both")
    }
    if (is.factor(groups) | is.character(groups)) groups <- tapply(1:length(groups),groups,identity)
    
    if (!is.list(groups)) stop("'groups' argument is not a factor or list")
    
    if (missing(color)) 
    {
      color <- rainbow(length(groups))
    }
  } else {
    if (missing(color)) 
    {
      color <- "white"
    } 
  }
  
  # Define exogenous variables:
  object@Vars$exogenous <- FALSE
  for (i in which(!object@Vars$manifest))
  {
    if (!any(object@RAM$edge[object@RAM$rhs==object@Vars$name[i]] == "->" & object@RAM$lhs[object@RAM$rhs==object@Vars$name[i]]%in%latNames))
    {
      object@Vars$exogenous[i] <- TRUE
    }
  }
  for (i in which(object@Vars$manifest))
  {
    if (all(object@RAM$lhs[object@RAM$rhs==object@Vars$name[i] & object@RAM$lhs%in%latNames]%in%object@Vars$name[object@Vars$exogenous]) &
      all(object@RAM$rhs[object@RAM$lhs==object@Vars$name[i] & object@RAM$rhs%in%latNames]%in%object@Vars$name[object@Vars$exogenous]) &
      !any(object@RAM$lhs[object@RAM$rhs==object@Vars$name[i] & object@RAM$edge=="->"]%in%manNames))
    {
      object@Vars$exogenous[i] <- TRUE
    }
  }
  # If all exo, treat all as endo:
  if (all(object@Vars$exogenous))
  {
    object@Vars$exogenous <- FALSE
  }
  # If al endo, treat formative manifest as exo (MIMIC mode)
  if (!all(object@Vars$exogenous))
  {
    object@Vars$exogenous[object@Vars$manifest & !(object@Vars$name%in%object@RAM$rhs[object@RAM$edge=="->"])] <- TRUE
  }
  
  Groups <- unique(object@RAM$group)
  qgraphRes <- list()
  if (missing(ask))
  {
    if (length(Groups)>1) ask <- TRUE else ask <- FALSE
  }
  askOrig <- par("ask")
  
  if (missing(include)) include <- 1:length(Groups)
  
  par(ask=ask)
  for (gr in Groups[(1:length(Groups))%in%include])
  {
    GroupRAM <- object@RAM[object@RAM$group==gr,]
    
    Labels <- c(manNames,latNames)
    
    Ni <- sum(GroupRAM$edge=="int")
    # Add intercept:
    if (any(object@RAM$edge=="int")) 
    {
      Labels[Labels=="1"] <- "_1"
      if (meanStyle == "single") 
      {
        Labels <- c(Labels,"1")
      } else if (meanStyle == "multi")
      {
        Labels <- c(Labels,rep("1",Ni))
      } 
    }
    nN <- length(Labels)
    
  
    # Extract edgelist:
    Edgelist <- GroupRAM[c("lhs","rhs")]
    Edgelist$lhs <- match(Edgelist$lhs,Labels)
    Edgelist$lhs[GroupRAM$edge=="int"] <- (nM+nL+1):nN
    Edgelist$rhs <- match(Edgelist$rhs,Labels)
    
    # Coerce to numeric matrix:
    Edgelist$lhs <- as.numeric(Edgelist$lhs)
    Edgelist$rhs <- as.numeric(Edgelist$rhs)
    Edgelist <- as.matrix(Edgelist)

    manInts <- Edgelist[GroupRAM$edge=="int" & GroupRAM$rhs%in%manNames,]
    latInts <- Edgelist[GroupRAM$edge=="int" & GroupRAM$rhs%in%latNames,]
    
    manIntsEndo <- manInts[!object@Vars$exogenous[manInts[,2]],,drop=FALSE]
    manIntsExo <- manInts[object@Vars$exogenous[manInts[,2]],,drop=FALSE]
    latIntsEndo <- latInts[!object@Vars$exogenous[latInts[,2]],,drop=FALSE]
    latIntsExo <- latInts[object@Vars$exogenous[latInts[,2]],,drop=FALSE]
    
    endoMan <- which(Labels%in%manNames&Labels%in%object@Vars$name[!object@Vars$exogenous])
    exoMan <- which(Labels%in%manNames&Labels%in%object@Vars$name[object@Vars$exogenous])
    endoLat <- which(Labels%in%latNames&Labels%in%object@Vars$name[!object@Vars$exogenous])
    exoLat <- which(Labels%in%latNames&Labels%in%object@Vars$name[object@Vars$exogenous])
    
    # Bidirectional:
    Bidir <- GroupRAM$edge == "<->"
    if (!grepl("mx",style,ignore.case=TRUE))
    {
      Bidir[GroupRAM$lhs==GroupRAM$rhs] <- FALSE
    }
    # lty:
    lty <- ifelse(GroupRAM$fixed,2,1)
    
    # Shape:
    Shape <- c(rep("square",nM),rep("circle",nL))
    if (any(GroupRAM$edge=="int")) Shape <- c(Shape,rep("triangle",Ni))
    
    Curve <- curve
      
    # Layout:
    if (layout=="tree" | layout=="circle" | layout=="circular")
    {
#       if (all(!object@Vars$exogenous))
#       {
        if (meanStyle=="single")
        {
          # Curves:
          Curve <- ifelse(GroupRAM$lhs != GroupRAM$rhs & ((GroupRAM$lhs%in%manNames & GroupRAM$rhs%in%manNames) | (GroupRAM$lhs%in%latNames & GroupRAM$rhs%in%latNames)),curve,0)
          Curve <- ifelse(GroupRAM$lhs%in%manNames,-1*Curve,Curve)
          Curve <- ifelse(GroupRAM$edge=="int" & GroupRAM$rhs%in%latNames,-1*curve,Curve)
          
          # Empty layout:
          Layout <- matrix(,length(Labels),2)
          
          # Add vertical levels:
          Layout[,2] <- ifelse(Labels%in%manNames,1,2)
          
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
          
        } else if (meanStyle=="multi")
        {          
          # Empty layout:
          Layout <- matrix(,length(Labels),2)
          
          # Add vertical levels:
          Layout[endoMan,2] <- 1
          Layout[endoLat,2] <- 2
          Layout[exoLat,2] <- 3
          Layout[exoMan,2] <- 4
          Layout[latIntsEndo[,1],2] <- 2
          Layout[latIntsExo[,1],2] <- 3
          
          if (residuals)
          {
            Layout[manIntsExo[,1],2] <- 4
            Layout[manIntsEndo[,1],2] <- 1
          } else {
            Layout[manIntsExo[,1],2] <- 5
            Layout[manIntsEndo[,1],2] <- 0            
          }
          
          # Add horizontal levels:
          if (nrow(manIntsEndo)>0)
          {
            Layout <- mixInts(endoMan,manIntsEndo,Layout,residuals=residuals)
          } else
          {
            Layout[endoMan,1] <- seq(-1,1,length=length(endoMan))
          }
          if (nrow(manIntsExo)>0)
          {
            Layout <- mixInts(exoMan,manIntsExo,Layout,residuals=residuals)
          } else
          {
            Layout[exoMan,1] <- seq(-1,1,length=length(exoMan))
          }
          
          if (nrow(latIntsEndo)>0)
          {
            Layout <- mixInts(endoLat,latIntsEndo,Layout,trim=TRUE)
          } else
          {
            Layout[endoLat,1] <- seq(-1,1,length=length(endoLat)+2)[-c(1,length(endoLat)+2)]
          }
          if (nrow(latIntsExo)>0)
          {
            Layout <- mixInts(exoLat,latIntsExo,Layout,trim=TRUE)
          } else
          {
            Layout[exoLat,1] <- seq(-1,1,length=length(exoLat)+2)[-c(1,length(exoLat)+2)]
          }
          
          # Curves:
          Curve <- ifelse(Layout[Edgelist[,1],2]==Layout[Edgelist[,2],2]&Edgelist[,1]!=Edgelist[,2]&GroupRAM$edge!="int",curve,0)
          
        } else stop("MeanStyle not supported")
        
        ### ORDINALIZE LAYOUT ###
        Layout[Layout[,2]>0&Layout[,2]<5,2] <- as.numeric(as.factor(Layout[Layout[,2]>0&Layout[,2]<5,2]))
        Layout[Layout[,2]==0,2] <- 0.5
        Layout[Layout[,2]==5,2] <- max(Layout[Layout[,2]<5,2]) + 0.5
#         for (i in unique(Layout[,2]))
#         {
#           Layout[Layout[,2]==i,1] <- (as.numeric(as.factor(Layout[Layout[,2]==i,1])) - 1) / (sum(Layout[,2]==i) - 1)
#         }
        # FLIP LAYOUT ###
        if (rotation==2) 
        {
          Layout <- Layout[,2:1]
          Layout[,1] <- -1 * Layout[,1]
        }
        if (rotation==3) 
        {
          Layout[,1] <- -1 * Layout[,1]
          Layout[,2] <- -1 * Layout[,2]
        }
        if (rotation==4) 
        {
          Layout <- Layout[,2:1]
          Layout[,2] <- -1 * Layout[,2]
        }
        Layout[,2] <- Layout[,2]-max(Layout[,2]) + 0.5
    } else Layout <- layout
    
    # loopRotation:
    if (layout=="tree" | layout=="circle")
    {
      loopRotation <- rep(0,nN)
      loopRotation[endoMan] <- pi
      loopRotation[exoMan] <- 0
      loopRotation[endoLat] <- 0
      loopRotation[exoLat] <- pi
      loopRotation <- loopRotation - 0.5 * (rotation-1) *pi
      
      if (any(object@Vars$exogenous))
      {
        ### For latents, find opposite of mean angle
        for (i in which(Labels%in%latNames))
        {
          # Layout subset of all connected:
          subEdgelist <- Edgelist[(Edgelist[,1]==i|Edgelist[,2]==i)&(Edgelist[,1]!=Edgelist[,2]),]
          conNodes <- c(subEdgelist[subEdgelist[,1]==i,2],subEdgelist[subEdgelist[,2]==i,1])
          subLayout <- Layout[conNodes,]
          Degrees <- apply(subLayout,1,function(x)atan2(x[1]-Layout[i,1],x[2]-Layout[i,2]))
          loopRotation[i] <- optimize(loopOptim,c(0,2*pi),Degrees=Degrees,maximum=TRUE)$maximum
          }
      }
    } else loopRotation <- NULL
    
    # Edge labels:
    if (edge.labels)
    {
      eLabels <- GroupRAM$label
    } else eLabels <- rep("",nrow(Edgelist))
    
    # vsize:
    vSize <- numeric(nN)
    vSize[Labels%in%manNames] <- sizeMan
    vSize[Labels%in%latNames] <- sizeLat
    vSize[Labels=="1"] <- sizeInt

    eColor <- NULL

    ### WHAT TO PLOT? ###
    if (grepl("path|diagram|model",what,ignore.case=TRUE))
    {
      
    } else if (grepl("stand|std",what,ignore.case=TRUE))
    {
      Edgelist <- cbind(Edgelist,GroupRAM$std)
      if (edge.labels) eLabels <- as.character(round(GroupRAM$std,2))
    } else if (grepl("est|par",what,ignore.case=TRUE))
    {
      Edgelist <- cbind(Edgelist,GroupRAM$est)
      if (edge.labels) eLabels <- as.character(round(GroupRAM$est,2))
    } else if (grepl("eq|cons",what,ignore.case=TRUE))
    {
      eColor <- rep(rgb(0.5,0.5,0.5),nrow(Edgelist))
      unPar <- unique(object@RAM$par[object@RAM$par>0 & duplicated(object@RAM$par)])
      cols <- rainbow(length(unPar))
      for (i in 1:length(unPar))
      {
        eColor[GroupRAM$par==unPar[i]] <- cols[i]
      }
    } else if (!grepl("col",what,ignore.case=TRUE)) stop("Could not detect use of 'what' argument")

    ### VERTEX COLOR ###
    if (!missing(groups))
    {
      NodeGroups <- groups
      
      Ng <- length(NodeGroups)
      
      if (length(color)==1)
      {
        Vcolors <- rep(color,nN)
      } else if (length(color)==nM)
      {
        Vcolors <- c(color,rep("",nN-nM))
      } else if (length(color)==nN)
      {
        Vcolors <- color  
      } else if (length(color)!=Ng)
      {
        stop("'color' vector not of appropriate length")
      }
      
      if (missing(manifests) & any(sapply(NodeGroups,mode)!="character")) warning("Groups specified numerically and 'manifests' not supplied. Results might be unexpected.")
      
      if (length(color)==Ng)
      {
        Vcolors <- rep("",nN)
        for (g in 1:Ng)
        {
          if (mode(NodeGroups[[g]])=="character") NodeGroups[[g]] <- match(NodeGroups[[g]],Labels)
          Vcolors[NodeGroups[[g]]] <- color[g]
        }
        
#         Vcolors[Vcolors=="" & Labels%in%manNames] <- "white"
      }
      
      # If missing color, obtain weighted mix of connected colors:
      VcolorsBU <- Vcolors
      if (ncol(Edgelist) ==3) W <- Edgelist[,3] else W <- rep(1,nrow(Edgelist))
      for (i in 1:(nM+nL))
      {
        if (Vcolors[i]=="")
        {
          cons <- c(Edgelist[Edgelist[,1]==i,2],Edgelist[Edgelist[,2]==i,1])
          cons <- cons[VcolorsBU[cons]!=""]
          if (length(cons)>0)
          {
            Vcolors[i] <- mixCols(VcolorsBU[cons],W[cons])
          } else Vcolors[i] <- "white"
        }
      }
      Vcolors[Vcolors==""] <- "white"
    } else 
    {
      NodeGroups <- NULL
      
      if (length(color)==1)
      {
        Vcolors <- rep(color,nN)
      } else if (length(color)==nM)
      {
        Vcolors <- c(color,rep("white",nN-nM))
      } else if (length(color)==nN)
      {
        Vcolors <- color  
      } else stop("'color' vector not of appropriate length")
    }
    
    if (grepl("col",what,ignore.case=TRUE))
    {
      eColor <- character(nrow(Edgelist))
      for (i in 1:nrow(Edgelist))
      {
        cols <- Vcolors[Edgelist[i,]]
        if (all(cols=="white"))
        {
          eColor[i] <- rgb(0.5,0.5,0.5)
        } else {
          eColor[i] <- mixCols(cols[cols!="white"])
        }
      }
    }
    
    if (!missing(whatLabels))
    {
      if (grepl("path|diagram|model|name|label",whatLabels,ignore.case=TRUE))
      {
        eLabels <- GroupRAM$label
      } else if (grepl("stand|std",whatLabels,ignore.case=TRUE))
      {
        eLabels <- as.character(round(GroupRAM$std,2))
      } else if (grepl("est|par",whatLabels,ignore.case=TRUE))
      {
        eLabels <- as.character(round(GroupRAM$est,2))
      } else if (grepl("eq|cons",whatLabels,ignore.case=TRUE))
      {
        if (edge.labels) eLabels <- GroupRAM$par
      } else if (grepl("no|omit|hide|invisible",whatLabels,ignore.case=TRUE))
      {
        eLabels <- rep("",nrow(Edgelist))
      } else stop("Could not detect use of 'whatLabels' argument")
    }

      # Abbreviate:
      if (nCharEdges>0)
      {
        eLabels <- abbreviate(eLabels,nCharEdges)
      }
      if (nCharNodes>0)
      {
        Labels <- abbreviate(Labels,nCharNodes)
      }
    
#     ### CONVERT TO LISREL STYLE ###
#     if (grepl("lisrel",style,ignore.case=TRUE))
#     {
#       whichResid <- which(GroupRAM$lhs == GroupRAM$rhs & GroupRAM$edge == "<->")
#       nResid <- length(whichResid)
#       Edgelist[whichResid,1] <- (nN+1):(nN+nResid)
#       rots <- loopRotation[Edgelist[whichResid,2]]
#       Lresid <- matrix(,nResid,2)
#       hLength <- diff(range(Layout[,1]))
#       vLength <- diff(range(Layout[,2]))
#       for (i in 1:nResid)
#       {
#         Lresid[i,1] <- Layout[Edgelist[whichResid[i],2],1] + sin(rots[i]) * resScale * 0.25 * hLength/vLength
#         Lresid[i,2] <- Layout[Edgelist[whichResid[i],2],2] + cos(rots[i]) * resScale * 0.25
#       }
#       
#       # Add nodes:
#       Layout <- rbind(Layout,Lresid)
#       Labels <- c(Labels,rep("",nResid))
#       Shape <- c(Shape,rep("circle",nResid))
#       loopRotation <- NULL
#       vSize <- c(vSize,rep(0,nResid))
#       Vcolors <- c(Vcolors,rep(rgb(0,0,0,0),nResid))
#     }
    
    if (grepl("mx",style,ignore.case=TRUE)) LoopAsResid <- FALSE else LoopAsResid <- TRUE
    
    ### ROTATE IF CIRCLE:
    if (layout=="circle")
    {
      if (rotation%in%c(2,4)) stop("Circle layout only supported if rotation is 1 or 3")
      Layout[,2] <- -1*Layout[,2] + max(Layout[,2]) + 0.5
      Ltemp <- Layout
      unVert <- sort(unique(Layout[,2]))
      for (i in unVert)
      {
        l <- sum(Layout[,2]==i)
        sq <- seq(0,2*pi,length=l+1)[-1]
        c <- 1
        for (j in order(Layout[Layout[,2]==i,1]))
        {
          Ltemp[Layout[,2]==i,][j,] <- RotMat(sq[c])%*%c(0,i)
          c <- c + 1
        }
      }
      Layout <- Ltemp
    }
    
    if (layout=="spring") loopRotation <- NULL
    
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
            vsize = vSize,
           edge.color=eColor,
            groups=NodeGroups,
            color=Vcolors,
            residuals=LoopAsResid,
            ...)
    
    if (title)
    {
#       if (length(Groups)==1) title("Path Diagram",line=3) else title(paste0("Path Diagram for group '",gr,"'"),line=3)
        title(gr,line=3)
    }
  }
  par(ask=askOrig)
  if (length(qgraphRes)==1) qgraphRes <- qgraphRes[[1]]
  invisible(qgraphRes)
  })
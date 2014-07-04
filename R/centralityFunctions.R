######################################
# a wrapper for centrality measures ##
######################################

# converts a matrix to a vector
mat2vec<-function(x, diag=FALSE, tol=1e-10)
{
  # Compute weights matrix:
  x <- getWmat(x)
  
  if(!is.matrix(x)) stop("A matrix is required as input")  
  symm<-ifelse(isSymmetric.matrix(object=unname(x), tol=tol), TRUE, FALSE) # detect whether the graph is directed
  if(diag==FALSE)
  {
    if(symm) vec<-x[upper.tri(x)] else vec<-c(x[upper.tri(x)], x[lower.tri(x)])
  } else if (diag==TRUE)
  {
    vec<-as.vector(x)
  }
  vec
}

# Exported:
centrality_auto<-function(x)
{
  # This function recognizes whether a network is weighted, directed and
  # whether there are disconnected nodes.
  # It computes centrality according to the matrix given as input.
  # If the network is disconnected, closeness is computed only for the largest component.
  
  # INPUT
  # x = an adjacency matrix or a weights matrix
  
  # OUTPUT
  # the output depends on the network given as input.
  # - if x is unweighted and directed
  #     then the InDegree, the OutDegree, the unweighted node betweenness,
  #     node closenes, and edge betweenness centralities are computed
  # - if x is unweighted and undirected
  #     then the Degree, the unweighted node betweenness,
  #     node closenes, and edge betweenness centralities are computed
  # - if x is weighted and directed
  #     then the InStrength, the OutStrength, the weighted node betweenness,
  #     node closenes, and edge betweenness centralities are computed
  # - if x is weighted and undirected
  #     then the Strength, the weighted node betweenness,
  #     node closenes, and edge betweenness centralities are computed
  
#   require(sna)
#   require(qgraph)
#   require(igraph)
  
  # Compute weights matrix:
  x <- getWmat(x)
  
  # If list of matrices, return list of output:
  if (is.list(x))
  {
    return(lapply(x, centrality_auto))
  }
  
  if(!is.matrix(x)) stop("the input network must be an adjacency or weights matrix")  
  
  diag(x)<-0 # loops are not included in centrality analysis
  x<-abs(x) # signs are not included in centrality analysis
  directed.gr<-ifelse(isSymmetric.matrix(object=x, tol=1e-12), FALSE, TRUE) # detect whether the graph is directed
  weighted.gr<-ifelse(all(mat2vec(x)%in%c(0,1)), FALSE, TRUE) # detect whether the graph is weighted
  
  # compute centrality with package qgraph: InDegree, OutDegree, Closeness, Betwenness
  net_qg<-qgraph(x, diag=FALSE, labels=colnames(x), DoNotPlot=TRUE, minimum=0)
  centr<-centrality(net_qg)
  
  # betweenness should be divided by two if the network is undirected
  if(directed.gr & !weighted.gr) centr1<-data.frame(cbind("Betweenness"=centr$Betweenness, "Closeness"=centr$Closeness, "InDegree"=centr$InDegree, "OutDegree"=centr$OutDegree))
  if(directed.gr & weighted.gr) centr1<-data.frame(cbind("Betweenness"=centr$Betweenness, "Closeness"=centr$Closeness, "InStrength"=centr$InDegree, "OutStrength"=centr$OutDegree))
  if(!directed.gr & !weighted.gr) centr1<-data.frame(cbind("Betweenness"=centr$Betweenness/2, "Closeness"=centr$Closeness, "Degree"=centr$OutDegree))
  if(!directed.gr & weighted.gr) centr1<-data.frame(cbind("Betweenness"=centr$Betweenness/2, "Closeness"=centr$Closeness, "Strength"=centr$OutDegree))
  
  row.names(centr1)<-colnames(x)
  
  # if the largest component is smaller than the network, closeness is recomputed only on the largest component
  largcomp<-component.largest(x, connected="strong") # select only the largest component
  if(sum(largcomp)<ncol(x))
  {
    x2<-x[largcomp,largcomp]
    clos<-centrality(qgraph(x2, diag=FALSE, labels=colnames(x)[largcomp], DoNotPlot=TRUE, minimum=0))$Closeness
    centr1$Closeness[largcomp]<-clos
    centr1$Closeness[!largcomp]<-NA
  }
  
  # compute edge betweenness with package igraph
  net_ig_abs<-graph.adjacency(adjmatrix=abs(1/x), mode=ifelse(directed.gr, "directed", "undirected"), weighted=(if(weighted.gr)TRUE), diag=FALSE)
  # edge betweenness centrality
  edgebet<-edge.betweenness(graph=net_ig_abs, directed=directed.gr)
  el<-data.frame(get.edgelist(graph=net_ig_abs), stringsAsFactors=FALSE)
  edgebet<-merge(el, edgebet, by=0)
  edgebet$Row.names<-NULL
  names(edgebet)<-c("from", "to", "edgebetweenness")
  edgebet<-edgebet[order(edgebet$edgebetweenness, decreasing=TRUE),]
  
  # shortest path lengths
  ShortestPathLengths<-centr$ShortestPathLengths
  rownames(ShortestPathLengths)<-colnames(ShortestPathLengths)<-colnames(x)
  
  Res <- list("node.centrality"=centr1, "edge.betweenness.centrality"=edgebet, "ShortestPathLengths"=ShortestPathLengths)
  class(Res) <- c("list","centrality_auto")
  return(Res)
}



#################################################
## functions to compute clustering coefficient ##
#################################################
# Exported:
clustcoef_auto<-function(x, thresholdWS=0, thresholdON=0)
{
  # this function computes several indices of clustering coefficient
  # INPUT:
  # x is an adjacency matrix or a weight matrix
  # thresholdWS is the threshold used for binarizing a weighted network for computing the unweighted
  #     clustering coefficient by Watts & Strogatz (1998).
  # OUTPUT:
  # all or a subset of the following indices of clustering coefficient, according to the kind of network
  # in input.
  # - clustWS: the unweighted clustering coefficient by Watts & Strogatz (1998)
  # - signed_clustWS: the generalization of the unweighted clustering coefficient for signed networks
  #                   by Costantini & Perugini (in press)
  # - clustZhang: the weighted clustering coefficient by Zhang & Horvath (2005).
  # - signed_clustZhang: the generalization of the clustering coefficient by Zhang & Horvath to signed networks
  #                       by Costantini & Perugini (in press)
  # - clustOnnela: the weighted clustering coefficient by Onnela et al. (2005)
  # - signed_clustOnnela: the generalization of the clustering coefficient by Onnela et al. to signed networks
  #                       by Costantini & Perugini (in press)
  # -clustBarrat: the weighted clustering coefficient by Barrat et al. (2004)
  
#   require(igraph)
  
  # Compute weights matrix:
  x <- getWmat(x)
  
  # If list of matrices, return list of output:
  if (is.list(x))
  {
    return(lapply(x, clustcoef_auto, thresholdWS=thresholdWS, thresholdON=thresholdWS))
  }
  
  # check adjacency matrix (this code is mostly borrowed from package WGCNA, function checkAdjMat)
  dim = dim(x)
  if (is.null(dim) || length(dim) != 2) 
    stop("adjacency is not two-dimensional")
  if (!is.numeric(x)) 
    stop("adjacency is not numeric")
  if (dim[1] != dim[2]) 
    stop("adjacency is not square")
  if (max(abs(x - t(x)), na.rm = TRUE) > 1e-12) 
    stop("adjacency is not symmetric")
  if (min(x, na.rm = TRUE) < -1 || max(x, na.rm = TRUE) > 1) 
    x<-x/max(abs(x))
  #####################
  
  weighted.gr<-ifelse(all(abs(x)%in%c(0,1)), FALSE, TRUE) # detect whether the graph is weighted
  signed.gr<-ifelse(all(x>=0), FALSE, TRUE) # detect whether the graph is weighted  
  
  # compute Barrat clustering coefficient
  net_ig<-graph.adjacency(adjmatrix=abs(x), mode="undirected", weighted=(if(weighted.gr)TRUE), diag=FALSE)
  cb<-transitivity(net_ig, type="barrat", isolates="zero")
  
  # compute the other measures of clustering coefficient
  cw<-clustWS(x, thresholdWS)
  cz<-clustZhang(x)
  co<-clustOnnela(x, thresholdON)
  
  if (!signed.gr &! weighted.gr) output<-cbind("clustWS"=cw[,1])
  if (!signed.gr & weighted.gr) output<-cbind("clustWS"=cw[,1], "clustZhang"=cz[,1], "clustOnnela"=co[,1], "clustBarrat"=cb)
  if (signed.gr & !weighted.gr) output<-cbind("clustWS"=cw[,1], "signed_clustWS"=cw[,2])
  if (signed.gr & weighted.gr) output<-cbind("clustWS"=cw[,1], "signed_clustWS"=cw[,2], "clustZhang"=cz[,1], "signed_clustZhang"=cz[,2], "clustOnnela"=co[,1], "signed_clustOnnela"=co[,2], "clustBarrat"=cb)
  
  # nodes for which the clustering coefficient cannot be computed have now NaN
  # this puts their value to zero
  output[is.na(output)]<-0
  
  Res <- data.frame(output)
  class(Res) <- c("data.frame","clustcoef_auto")
  rownames(Res) <- colnames(x)
  return(Res)
}


clustWS<-function(x, thresholdWS=0)
{
  # Compute weights matrix:
  W <- getWmat(x)
  
  # If list of matrices, return list of output:
  if (is.list(W))
  {
    return(lapply(W, clustWS, thresholdWS=thresholdWS))
  }
  
  threshold<-thresholdWS
  
  diag(W)<-0
  A<-matrix(0, nrow=nrow(W), ncol=ncol(W))
  A[W>threshold]<-1
  A[W<(-threshold)]<--1
  
  diag(A)<-0
  a_A<-abs(A)
  a_num<-diag(a_A%*%a_A%*%a_A)
  num<-diag(A%*%A%*%A)
  
  ki<-colSums(abs(A))
  den<-ki*(ki-1)
  
  cW<-num/den
  a_cW<-a_num/den
  
  data.frame(cbind("clustWS"=a_cW, "signed_clustWS"=cW))
}

clustZhang<-function(x)
{
  # Compute weights matrix:
  W <- getWmat(x)
  
  # If list of matrices, return list of output:
  if (is.list(W))
  {
    return(lapply(W, clustZhang))
  }
  
  # this function has been adapted from package WGCNA
  diag(W)<-0
  a_W<-abs(W)
  num<-diag(W%*%W%*%W)
  a_num<-diag(a_W%*%a_W%*%a_W)
  den<-colSums(a_W)^2-colSums(W^2)
  cZ<-num/den
  a_cZ<-a_num/den
  data.frame(cbind("clustZhang"=a_cZ, "signed_clustZhang"=cZ))
}

clustOnnela<-function(x, thresholdON=0)
{
  # Compute weights matrix:
  W <- getWmat(x)
  
  # If list of matrices, return list of output:
  if (is.list(W))
  {
    return(lapply(W, clustOnnela, thresholdON = thresholdON))
  }
  
  threshold<-thresholdON
  
  diag(W)<-0
  W[abs(W)<threshold]<-0
  a_W<-abs(W)
  a_W13<-a_W^(1/3)
  W13<-matrix(nrow=nrow(W), ncol=ncol(W))
  W13[W>=0]<-a_W13[W>=0]
  W13[W<0]<-(abs(W[W<0])^(1/3))*(-1)
  
  num<-diag(W13%*%W13%*%W13)
  a_num<-diag(a_W13%*%a_W13%*%a_W13)
  
  A<-matrix(0, nrow=nrow(W), ncol=ncol(W))
  A[abs(W)>threshold]<-1
  ki<-colSums(A)
  den<-ki*(ki-1)
  
  cO<-num/den
  a_cO<-a_num/den
  
  data.frame(cbind("clustOnnela"=a_cO, "signed_clustOnnela"=cO))
}

##################################
# evaluation of smallworldness #
################################
# Exported:
smallworldness<-function(x, B=1000, up=.995, lo=.005)
{
  #compute the small worldness of Humphries & Gurney (2008)
#   require(igraph)
#   require(sna)
  # Compute weights matrix:
  x <- getWmat(x)
  
  # If list of matrices, return list of output:
  if (is.list(x))
  {
    return(lapply(x, smallworldness, B=B, up=up, lo=lo))
  }
  
  # consider only the adjacency matrix
  A<-x!=0
  
  # transitivity of A
  A<-graph.adjacency(A, mode="undirected", diag=F, weighted=NULL)
  N<-vcount(A)
  m<-ecount(A)
  clusttrg<-transitivity(A, type="global", isolates="zero")
  lengthtrg<-average.path.length(graph=A, directed=F, unconnected=F)
  
  #generate B rnd networks with the same degree distribution of A
  deg.dist<-igraph::degree(A, mode="all", loops=F)
  rndA<-lapply(1:B, function(x)degree.sequence.game(deg.dist, method="simple.no.multiple"))
  # compute the average (global) clustering coefficient over the B random networks
  clustrnd<-sapply(rndA, transitivity, type="global", isolates="zero")
  clustrnd_m<-mean(clustrnd)
  # compute the upper and lower quantiles
  clustrnd_lo<-quantile(clustrnd, lo)
  clustrnd_up<-quantile(clustrnd, up)
  
  # compute the average shortest path length in random networks, the shortest path
  # length among unconnected nodes is computed as N, i.e., 1 plus the max possible path length
  lengthrnd<-sapply(rndA, average.path.length, directed=F, unconnected=F)
  lengthrnd_m<-mean(lengthrnd)
  # compute the upper and lower quantiles
  lengthrnd_lo<-quantile(lengthrnd, lo)
  lengthrnd_up<-quantile(lengthrnd, up)
  
  # compute humphries&gourney(2008) smallworld-ness
  sigma<-(clusttrg/clustrnd_m)/(lengthtrg/lengthrnd_m)
  
  c("smallworldness"=sigma, "trans_target"=clusttrg, "averagelength_target"=lengthtrg, "trans_rnd_M"=clustrnd_m, "trans_rnd_lo"=unname(clustrnd_lo), "trans_rnd_up"=unname(clustrnd_up), "averagelength_rnd_M"=lengthrnd_m, "averagelength_rnd_lo"=unname(lengthrnd_lo), "averagelength_rnd_up"=unname(lengthrnd_up))
}

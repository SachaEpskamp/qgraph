
qgraph.cfa <- function( S, N, groups=NULL, ..., pkg = "sem", labels=NULL, fun = qgraph, opts = list())
{

	if (is.null(groups)) groups <- list(1:nrow(S))
	if (is.factor(groups)  | is.character(groups)) groups <- tapply(1:length(groups),groups,function(x)x)
	
	n <- nrow(S)
	k <- length(groups)
	
	if (is.null(names(groups))) names(groups) <- LETTERS[1:k] 
	facts <- names(groups)
	if (is.null(labels)) 
	{
		if (is.null(colnames(S))) 
		{
			labels <- paste("X",1:n,sep="") 
			colnames(S) <- rownames(S) <- labels
		} else labels <- colnames(S)
	}
	
	if (pkg=="sem")
	{
		#reqTest <- require("sem")
		#if (!reqTest) stop("sem could not be loaded, is this package installed?")
		reorder <- as.vector(unlist(groups))
		
		if (length(reorder) != nrow(S)) stop("Each variable must be used only once in the groups list")
		
		S <- S[reorder,reorder]
		rownames(S) <- colnames(S) <- labels <- labels[reorder]
		
		groups <- lapply(groups,match,reorder)
		
		#if (factCor)
		#{
			coms <- combn(n+1:k,2)
			Ffr <- coms[1,]
			Fto <- coms[2,]
			
				RAM <- data.frame(
					heads <- c(rep(1,n),rep(2,n+k+((k^2-k)/2))),
					to <- c(1:n,1:n,Fto,n+1:k),
					from <- c(n+rep(seq_along(groups), sapply(groups, length)),1:n,Ffr,n+1:k),
					parameter <- 1,
					value <-  NA
					)
			names(RAM) <- c("heads","to","from","parameter","value")
			
			RAM$parameter[cumsum(sapply(groups,length))-length(groups[[1]])+1] <- 0
			RAM$parameter[RAM$parameter!=0] <- 1:sum(RAM$parameter!=0)
			RAM <- transform(RAM, value=ifelse(parameter==0,1,NA))
			
			factCorNames <- cbind(c(Ffr,1:k),c(Fto,1:k))
		
			ParNames <- c(paste("*l",1:n,sep=""),paste("*q",1:n,sep=""),paste("*y",apply(factCorNames,1,paste,collapse=""),sep=""))[RAM$parameter!=0]
		#} else
		#{
		#	RAM <- data.frame(
		#			heads <- c(rep(1,n),rep(2,n+k)),
		#			to <- c(1:n,1:n,n+1:k),
		#			from <- c(n+rep(seq_along(groups), sapply(groups, length)),1:n,n+1:k),
		#			parameter <- c(1:(n*2),rep(0,k)),
		#			value <-  c(rep(NA,n*2),rep(1,k))
		#			)
		#	names(RAM) <- c("heads","to","from","parameter","value")	
		##	ParNames <- c(paste("*l",1:n,sep=""),paste("*q",1:n,sep=""),paste("*y",1:k,sep=""))[RAM$parameter!=0]
		#}
		
		VarNames <- c(labels,facts)

		res <- do.call("sem",c(list(as.matrix(RAM),S=S,N=N,param.names=ParNames,var.names=VarNames),opts))

		if (deparse(substitute(fun)) == "qgraph.loadings")
		{
			standCoef <- standardizedCoefficients(res)
			loads <- matrix(0,n,k)
			c <- 1
			for (i in 1:k) 
			{
				loads[c:(c+length(groups[[i]])-1),i] <- standCoef[c:(c+length(groups[[i]])-1),2]
				c <- c+length(groups[[i]])
			}
			Resid <- standCoef[(n+1):(2*n),2]
			FactCorMat <- matrix(0,k,k)
			diag(FactCorMat) <- standCoef[(nrow(standCoef)-k+1):(nrow(standCoef)),2]
			#if (factCor)
			#{
				FactCorMat[lower.tri(FactCorMat)] <- standCoef[(2*n+1):(2*n+((k^2-k)/2)),2]
				FactCorMat[upper.tri(FactCorMat)] <- t(FactCorMat)[upper.tri(FactCorMat)]
			#}
			do.call(fun,list(fact=loads,resid=Resid,factorCors=FactCorMat,model="reflective",groups=groups,labels=labels,...))
		} else fun(res,groups=groups,labels=labels,...)
		
		return(res)
	}
	if (pkg=="lavaan")
	{
		#reqTest <- require("lavaan")
		#if (!reqTest) stop("lavaan could not be loaded, is this package installed?")
		model <- paste(facts," =~ ",lapply(groups,function(x)paste(labels[x],collapse=" + ")),collapse=" \n ")
		model <- paste(model,"\n")

		fit <- do.call("cfa",c(list(model = model, sample.cov = S, sample.nobs = N),opts))

		fun(fit,...)
		return(fit)
	}
}

	
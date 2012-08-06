# function: standardizeRAM
# author:   Ryne Estabrook
# date:     20 Oct 2010
# revised:  01 Nov 2010 (corrected algebra)
#           13 Dec 2010 (corrected 'parameters' output)

standardizeRAM <- function(model, return="parameters", Amat=NA, Smat=NA, Mmat=NA){
	# make sure 'return' is valid
	if (!(return=="parameters"|return=="matrices"|return=="model"))stop("Invalid 'return' parameter. What do you want from me?")
	# get the name of the objective
	obj <- class(model@objective)[1]
	suppliedNames <- !is.na(Amat)&!is.na(Smat)
	cA <- is.character(Amat)
	cS <- is.character(Smat)
	cM <- is.character(Mmat)
	# if the objective function isn't RAMObjective, you need to supply Amat and Smat
	if (obj!="MxRAMObjective"&(!cA))stop("I need either mxRAMObjective or the names of the A and S matrices.")
	output <- model@output
	# stop if there is no objective function
	if (is.null(output))stop("Provided model has no objective function, and thus no output. I can only standardize models that have been run!")
	# stop if there is no output
	if (length(output)<1)stop("Provided model has no output. I can only standardize models that have been run!")
	# get the names of the A, S and M matrices 
	if (cA){nA <- Amat} else {nA <- model@objective@A}
	if (cS){nS <- Smat} else {nS <- model@objective@S}
	if (cM){nM <- Mmat} else {nM <- model@objective@M}
	# get the actual A and S matrices, and make an identity matrix
	A <- model[[nA]]
	S <- model[[nS]]
	d <- dim(S@values)[1]
	I <- diag(d)
	# calculate the model expected covariance matrix
	IA <- solve(I-A@values)
	expCov <- IA %*% S@values %*% t(IA)
	# calculate 1/SDs and put them in a diagonal matrix
	invSDs <- 1/sqrt(diag(expCov))
	# give the inverse SDs names, because mxSummary treats column names as characters
	names(invSDs) <- as.character(1:length(invSDs))
	if (!is.null(dimnames(A@values))){names(invSDs) <- as.vector(dimnames(S@values)[[2]])}
	# put the inverse SDs into a diagonal matrix (might as well recycle my I matrix from above)
	diag(I) <- invSDs
	# standardize the A, S and M matrices
	# A paths are value*sd(from)/sd(to) = I %*% A %*% solve(I)
	# S paths are value/(sd(from*sd(to))) = I %*% S %*% I
	stdA <- I %*% A@values %*% solve(I)
	stdS <- I %*% S@values %*% I
	# populate the model
	model[[nA]]@values[,] <- stdA
	model[[nS]]@values[,] <- stdS
	if (!is.na(nM)){model[[nM]]@values[,] <- rep(0, length(invSDs))}
	# return the model, if asked
	if(return=="model")return(model)
	# return the matrices, if asked
	matrices <- list(model[[nA]], model[[nS]])
	names(matrices) <- c("A", "S")
	if(return=="matrices")return(matrices)
	# else, return the parameters
	# let's rebuild the parameter list
	p <- summary(model)$parameters
	p <- p[(p[,2]==nA)|(p[,2]==nS),]
	## get the rescaling factor
	# this is for the A matrix
	rescale <- invSDs[p$row] * 1/invSDs[p$col]
	# this is for the S matrix
	rescaleS <- invSDs[p$row] * invSDs[p$col]
	# put the A and the S together
	rescale[p$matrix=="S"] <- rescaleS[p$matrix=="S"]
	# rescale
	p[,5] <- p[,5] * rescale
	p[,6] <- p[,6] * rescale
	# rename the columns
	names(p)[5:6] <- c("Std. Estimate", "Std.Std.Error")
	# bye!
	return(p)
	}
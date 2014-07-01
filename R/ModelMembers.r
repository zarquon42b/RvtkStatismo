
setMethod("GetPCABasisMatrix", signature(model = "pPCA"), function(model) {
    W <- t(t(model@PCA$rotation)*model@PCA$sdev) ##Matrix to project scaled PC-scores back into the config space
    return(W)
})

setMethod("GetOrthonormalPCABasisMatrix" ,signature(model="pPCA"),function(model) {
    return(model@PCA$rotation)
})

setMethod("GetNoiseVariance",signature(model = "pPCA"), function(model) {
    return(model@sigma)
})

setMethod("GetMeanVector",signature(model = "pPCA"), function(model) {
    return(model@PCA$center)
})

setMethod("GetPCAVarianceVector", signature(model="pPCA"), function(model) {
    return(model@PCA$sdev^2)
})

setMethod("ComputeLogProbabilityOfDataset",signature(model="pPCA"), function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),TRUE)
    return(out)
})

setMethod("ComputeProbabilityOfDataset",signature(model="pPCA"), function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),FALSE)
    return(out)
})


setMethod("GetProjectionMatrix", signature(model="pPCA"), function(model) {
    WT <- t(GetPCABasisMatrix(model))
    Mmatrix <- crossprod(GetPCABasisMatrix(model))
    diag(Mmatrix) <- diag(Mmatrix)+model@sigma
    Mmatrixinv <- solve(Mmatrix)
    Win <- Mmatrixinv%*%WT
    return(Win)
})


setMethod("DrawMean",  signature(model="pPCA"), function(model) {
    out <- output2sample(.Call("DrawMean",model))
    return(out)
})

setMethod("DrawMeanAtPoint",  signature(model="pPCA",pt="numeric"), function(model,pt) {
    if (length(pt) == 1)
        meanpt <- GetDomainPoints(model)[pt,]
    else if (length(pt) == 3)
        meanpt <- pt
    else
        stop("pt must be an integer or a vector of length 3")
    out <- .Call("DrawMeanAtPoint",model,meanpt)
    return(out)
})


setMethod("DrawSample",  signature(model="pPCA"), function(model,coefficients=NULL, addNoise=FALSE) {
    if (is.vector(coefficients)) {
        npc <- ncol(model@PCA$rotation)
        lcoeff <- length(coefficients)
        if (lcoeff < npc){
            zero <- rep(0,npc)
            zero[1:lcoeff] <- coefficients
            coefficients <- zero
        } else if (lcoeff > npc) {
            coefficients <- coefficients[1:npc]
            message(paste0("  NOTE: only first ", npc, " coefficients used"))
        }
    }
    out <- output2sample(.Call("DrawSample",model,coefficients,addNoise))
    return(out)
})

setMethod("DrawSampleAtPoint",  signature(model="pPCA",coefficients="numeric",pt="numeric"), function(model,coefficients,pt, addNoise=FALSE) {
    npc <- ncol(model@PCA$rotation)
    lcoeff <- length(coefficients)
    if (lcoeff < npc){
        zero <- rep(0,npc)
        zero[1:lcoeff] <- coefficients
        coefficients <- zero
    } else if (lcoeff > npc) {
        coefficients <- coefficients[1:npc]
        message(paste0("  NOTE: only first ", npc, " coefficients used"))
    }
    if (length(pt) == 1)
        meanpt <- GetDomainPoints(model)[pt,]
    else if (length(pt) == 3)
        meanpt <- pt
    else
        stop("pt must be an integer or a vector of length 3")
    
    
    out <- (.Call("DrawSampleAtPoint",model,coefficients,meanpt,addNoise))
    return(out)
})

setMethod("ComputeCoefficientsForDataset",signature(model="pPCA"), function(model,dataset) {
    out <- .Call("ComputeCoefficientsForDataset",model,dataset2representer(dataset))
    return(out)
})

setMethod("GetDomainPoints", signature(model="pPCA"), function(model) {
    out <- t(.Call("GetDomainPoints",model))
    return(out)
})

setMethod("GetDomainSize", signature(model="pPCA"), function(model) {
    out <- ncol(model@representer$vb)
    return(out)
})

setMethod("GetCovarianceAtPoint", signature(model="pPCA",pt1="numeric",pt2="numeric"), function(model,pt1,pt2) {
    if (length(pt1) == 1 && length(pt2)==1) {
        out <- .Call("GetCovarianceAtPointId",model,pt1-1,pt2-1)
    } else if (length(pt1) == 3 && length(pt2)==3) {
        out <- .Call("GetCovarianceAtPointPt",model,pt1,pt2)
    } else {
        stop("either provide 2 integers or 2 3D-vectors")
    }
    return(out)
})

setMethod("GetCovarianceMatrix", signature(model="pPCA"), function(model) {
    out <- .Call("GetCovarianceMatrix",model)
    return(out)
})

setMethod("GetJacobian", signature(model="pPCA", pt="numeric"), function(model,pt) {
    if (length(pt) == 1)
        pt <- GetDomainPoints(model)[pt,]
    out <- .Call("GetJacobian",model,pt)
    return(out)
})

setMethod("ComputeCoefficientsForPointValues", signature(model="pPCA", sample="matrix",pt="numeric",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    sample <- t(sample)
    mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    out <- .Call("ComputeCoefficientsForPointValues",model,sample,mean,ptNoise)
    return(out)
})

setMethod("ComputeCoefficientsForPointValues", signature(model="pPCA", sample="matrix",pt="matrix",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    sample <- t(sample)
    mean <- t(pt)
    out <- .Call("ComputeCoefficientsForPointValues",model,sample,mean,addNoise)
    return(out)
})

setMethod("ComputeCoefficientsForPointValues", signature(model="pPCA", sample="numeric",pt="numeric",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    sample <- matrix(sample,3,1)
    if (length(pt) == 3)
        mean <- matrix(pt,3,1)
    else if (length(pt) == 1)
        mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    else
        stop("in this case pt must be a vector of length 3 or an integer")
    out <- .Call("ComputeCoefficientsForPointValues",model,sample,mean,ptNoise)
    return(out)
})



setMethod("EvaluateSampleAtPoint", signature(model="pPCA", sample="matrix",pt="numeric"), function(model,sample,pt) {
    sample <- list(vb=t(sample),it=matrix(0,0,0))
    if (length(pt) == 1)
        meanpt <- GetDomainPoints(model)[pt,]
    else if (length(pt) == 3)
        meanpt <- pt
    else
        stop("pt must be an integer or a vector of length 3")
    out <- .Call("EvaluateSampleAtPoint",model,sample,meanpt)
    return(out)
})

#' @importFrom Morpho vert2points
setMethod("EvaluateSampleAtPoint", signature(model="pPCA", sample="mesh3d",pt="numeric"), function(model,sample,pt) {
    sample <- vert2points(sample)
    out <- EvaluateSampleAtPoint(model,sample,pt)
    return(out)
})


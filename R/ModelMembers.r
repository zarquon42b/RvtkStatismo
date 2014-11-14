#### MODEL MATRICES #################

#' @rdname StatismoMatrices
setMethod("GetPCABasisMatrix", signature(model = "pPCA"), function(model) {
    W <- t(t(model@PCA$rotation)*model@PCA$sdev) ##Matrix to project scaled PC-scores back into the config space
    return(W)
})

#' @rdname StatismoMatrices
setMethod("GetOrthonormalPCABasisMatrix" ,signature(model="pPCA"),function(model) {
    return(model@PCA$rotation)
})

#' @rdname StatismoMatrices
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

#' @rdname StatismoMatrices
setMethod("GetCovarianceMatrix", signature(model="pPCA"), function(model) {
    out <- .Call("GetCovarianceMatrix",model)
    return(out)
})

#' @rdname StatismoMatrices
setMethod("GetJacobian", signature(model="pPCA", pt="numeric"), function(model,pt) {
    if (length(pt) == 1)
        pt <- GetDomainPoints(model)[pt,]
    out <- .Call("GetJacobian",model,pt)
    return(out)
})

#' @rdname StatismoMatrices
setMethod("GetProjectionMatrix", signature(model="pPCA"), function(model) {
    WT <- t(GetPCABasisMatrix(model))
    Mmatrix <- crossprod(GetPCABasisMatrix(model))
    diag(Mmatrix) <- diag(Mmatrix)+model@sigma
    Mmatrixinv <- solve(Mmatrix)
    Win <- Mmatrixinv%*%WT
    return(Win)
})


####Statismo Parameters
#' @rdname statismoParameters
setMethod("GetNoiseVariance",signature(model = "pPCA"), function(model) {
    return(model@sigma)
})

#' @rdname statismoParameters
setMethod("GetMeanVector",signature(model = "pPCA"), function(model) {
    return(model@PCA$center)
})

#' @rdname statismoParameters
setMethod("GetPCAVarianceVector", signature(model="pPCA"), function(model) {
    return(model@PCA$sdev^2)
})

### SAMPLE INFO

#' @rdname StatismoSample
setMethod("ComputeLogProbabilityOfDataset",signature(model="pPCA"), function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),TRUE)
    return(out)
})

#' @rdname StatismoSample
setMethod("ComputeProbabilityOfDataset",signature(model="pPCA"), function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),FALSE)
    return(out)
})

#' @rdname StatismoSample
setMethod("ComputeMahalanobisDistanceForDataset",signature(model="pPCA"), function(model,dataset) {
    out <- .Call("ComputeMahalanobisDistanceForDataset",model,dataset2representer(dataset),FALSE)
    return(out)
})

#' @rdname statismoMembers
setMethod("DrawMean",  signature(model="pPCA"), function(model) {
    out <- output2sample(.Call("DrawMean",model))
    return(out)
})

#' @rdname statismoMembers
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

#' @rdname statismoMembers
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

#' @rdname statismoMembers
setMethod("DrawSampleVector",  signature(model="pPCA"), function(model,coefficients, addNoise=FALSE) {
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
    
    out <- .Call("DrawSampleVector",model,coefficients,addNoise)
    return(out)
})

#' @rdname statismoMembers
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

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForDataset",signature(model="pPCA"), function(model,dataset) {
    out <- .Call("ComputeCoefficientsForDataset",model,dataset2representer(dataset))
    return(out)
})

#' @rdname statismoMembers
setMethod("RobustlyComputeCoefficientsForDataset",signature(model="pPCA"), function(model,dataset,niterations=100, nu = 6, sigma2=1) {
    out <- .Call("RobustlyComputeCoefficientsForDataset",model,dataset2representer(dataset),niterations, nu, sigma2)
    return(out)
})

#' @rdname statismoMembers
setMethod("GetDomainPoints", signature(model="pPCA"), function(model) {
    out <- t(.Call("GetDomainPoints",model))
    return(out)
})

#' @rdname statismoMembers
setMethod("GetDomainSize", signature(model="pPCA"), function(model) {
    out <- ncol(model@representer$vb)
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValues", signature(model="pPCA", sample="matrix",pt="numeric",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    sample <- t(sample)
    mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    out <- .Call("ComputeCoefficientsForPointValues",model,sample,mean,ptNoise)
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValues", signature(model="pPCA", sample="matrix",pt="matrix",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    sample <- t(sample)
    mean <- t(pt)
    out <- .Call("ComputeCoefficientsForPointValues",model,sample,mean,ptNoise)
    return(out)
})

#' @rdname statismoMembers
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


#' @rdname statismoMembers
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

#' @rdname statismoMembers
#' @importFrom Morpho vert2points
setMethod("EvaluateSampleAtPoint", signature(model="pPCA", sample="mesh3d",pt="numeric"), function(model,sample,pt) {
    sample <- vert2points(sample)
    out <- EvaluateSampleAtPoint(model,sample,pt)
    return(out)
})

#' @rdname statismoMembers
setMethod("GetModelInfo", signature(model="pPCA"), function(model) {

    datainfo <- unlist(model@modelinfo@datainfo)
    if (!is.null(datainfo))
        datainfoframe <- as.data.frame(matrix(datainfo,length(datainfo)/2,2,byrow = T))
    else
        datainfoframe <- data.frame()
    
    paraminfo <- unlist(model@modelinfo@paraminfo)
    if (!is.null(paraminfo))
        paraminfoframe <- as.data.frame(matrix(paraminfo,length(paraminfo)/2,2,byrow = T))
    else
        paraminfoframe <- data.frame()
    scores <- model@PCA$x
    return(list(paraminfo=paraminfoframe,datainfo=datainfoframe,scores=scores))
})

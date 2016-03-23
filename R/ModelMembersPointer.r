#### MODEL MATRICES #################

#' @rdname StatismoMatrices
setMethod("GetPCABasisMatrix", signature(model = "pPCA_pointer"), function(model) {
    W <- .Call("GetPCABasisMatrix",model) ##Matrix to project scaled PC-scores back into the config space
    return(W)
})

#' @rdname StatismoMatrices
setMethod("GetOrthonormalPCABasisMatrix" ,signature(model="pPCA_pointer"),function(model) {
    return(.Call("GetOrthonormalPCABasisMatrix",model))
})

#' @rdname StatismoMatrices
setMethod("GetCovarianceAtPoint", signature(model="pPCA_pointer",pt1="numeric",pt2="numeric"), function(model,pt1,pt2) {
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
setMethod("GetCovarianceMatrix", signature(model="pPCA_pointer"), function(model) {
    out <- .Call("GetCovarianceMatrix",model)
    return(out)
})

#' @rdname StatismoMatrices
setMethod("GetJacobian", signature(model="pPCA_pointer", pt="numeric"), function(model,pt) {
    if (length(pt) == 1)
        pt <- GetDomainPoints(model)[pt,]
    out <- .Call("GetJacobian",model,pt)
    return(out)
})

#' @rdname StatismoMatrices
setMethod("GetProjectionMatrix", signature(model="pPCA_pointer"), function(model) {
    WT <- t(GetPCABasisMatrix(model))
    Mmatrix <- crossprod(t(WT))
    diag(Mmatrix) <- diag(Mmatrix)+GetNoiseVariance(model)
    Mmatrixinv <- solve(Mmatrix)
    Win <- Mmatrixinv%*%WT
    return(Win)
})


####Statismo Parameters

#' @rdname statismoParameters
setMethod("GetNoiseVariance",signature(model = "pPCA_pointer"), function(model) {
    return(.Call("GetNoiseVariance",model))
})

#' @rdname statismoParameters
setMethod("GetMeanVector",signature(model = "pPCA_pointer"), function(model) {
    return(.Call("GetMeanVector",model))
})

#' @rdname statismoParameters
setMethod("GetPCAVarianceVector", signature(model="pPCA_pointer"), function(model) {
    return (.Call("GetPCAVarianceVector",model))
    #return(model@PCA$sdev^2)
})

#' @rdname statismoParameters
setMethod("GetNumberOfPrincipalComponents",signature(model = "pPCA_pointer"), function(model) {
    return(.Call("GetNumberOfPrincipalComponents",model))
})


### SAMPLE INFO


#' @rdname StatismoSample
setMethod("ComputeLogProbabilityOfDataset",signature(model="pPCA_pointer"), function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),TRUE)
    return(out)
})



#' @rdname StatismoSample
setMethod("ComputeProbabilityOfDataset",signature(model="pPCA_pointer"), function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),FALSE)
    return(out)
})
#' @rdname StatismoSample
setMethod("ComputeProbabilityOfCoefficients",signature(model="pPCA_pointer"), function(model,coefficients) {
    npc <- GetNumberOfPrincipalComponents(model)
    if (length(coefficients) != npc)
        warning("number of coefficients != number of PCs")
    out <- .Call("ComputeProbabilityOfCoefficients",model,coefficients)
    return(out)
})

#' @rdname StatismoSample
setMethod("ComputeLogProbabilityOfCoefficients",signature(model="pPCA_pointer"), function(model,coefficients) {
    npc <- GetNumberOfPrincipalComponents(model)
    if (length(coefficients) != npc)
        warning("number of coefficients != number of PCs")
    out <- .Call("ComputeLogProbabilityOfCoefficients",model,coefficients)
    return(out)
})

#' @rdname StatismoSample
setMethod("ComputeMahalanobisDistanceForDataset",signature(model="pPCA_pointer"), function(model,dataset) {
    out <- .Call("ComputeMahalanobisDistanceForDataset",model,dataset2representer(dataset),FALSE)
    return(out)
})

#' @rdname statismoMembers
setMethod("DrawMean",  signature(model="pPCA_pointer"), function(model) {
    out <- output2sample(.Call("DrawMean",model))
    return(out)
})

#' @rdname statismoMembers
setMethod("DrawMeanAtPoint",  signature(model="pPCA_pointer",pt="numeric"), function(model,pt) {
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
setMethod("DrawSample",  signature(model="pPCA_pointer"), function(model,coefficients=NULL, addNoise=FALSE) {
    if (is.vector(coefficients)) {
        npc <- GetNumberOfPrincipalComponents(model)
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
setMethod("DrawSampleVector",  signature(model="pPCA_pointer"), function(model,coefficients, addNoise=FALSE) {
    npc <- GetNumberOfPrincipalComponents(model)
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
setMethod("DrawSampleAtPoint",  signature(model="pPCA_pointer",coefficients="numeric",pt="numeric"), function(model,coefficients,pt, addNoise=FALSE) {
    npc <- GetNumberOfPrincipalComponents(model)
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
setMethod("DrawPCABasisSample",  signature(model="pPCA_pointer",pcaComponent="numeric"), function(model,pcaComponent) {
    out <- output2sample(.Call("DrawPCABasisSample",model,pcaComponent))
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForDataset",signature(model="pPCA_pointer"), function(model,dataset) {
    out <- .Call("ComputeCoefficientsForDataset",model,dataset2representer(dataset))
    return(out)
})

# #' @rdname statismoMembers
# setMethod("RobustlyComputeCoefficientsForDataset",signature(model="pPCA"), function(model,dataset,niterations=100, nu = 6, sigma2=1) {
#     out <- .Call("RobustlyComputeCoefficientsForDataset",model,dataset2representer(dataset),niterations, nu, sigma2)
#    return(out)
# })

#' @rdname statismoMembers
setMethod("GetDomainPoints", signature(model="pPCA_pointer"), function(model) {
    out <- t(.Call("GetDomainPoints",model))
    return(out)
})

#' @rdname statismoMembers
setMethod("GetDomainSize", signature(model="pPCA_pointer"), function(model) {
    out <- ncol(GetDomainPoints(model))
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValues", signature(model="pPCA_pointer", sample="matrix",pt="numeric",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    sample <- t(sample)
    mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    out <- .Call("ComputeCoefficientsForPointValues",model,sample,mean,ptNoise)
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValues", signature(model="pPCA_pointer", sample="matrix",pt="matrix",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    sample <- t(sample)
    mean <- t(pt)
    out <- .Call("ComputeCoefficientsForPointValues",model,sample,mean,ptNoise)
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValues", signature(model="pPCA_pointer", sample="numeric",pt="numeric",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
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
setMethod("ComputeCoefficientsForPointValuesWithCovariance", signature(model="pPCA_pointer", sample="matrix",pt="numeric",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    ptNoise <- as.matrix(ptNoise)
    if (length(ptNoise) == 1)
        ptNoise <- as.matrix(rep(ptNoise,nrow(sample)))
    sample <- t(sample)
    mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
     storage.mode(ptNoise) ="numeric"
    out <- .Call("ComputeCoefficientsForPointValuesWithCovariance",model,sample,mean,ptNoise)
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValuesWithCovariance", signature(model="pPCA_pointer", sample="matrix",pt="matrix",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    if (length(ptNoise) == 1)
        ptNoise <- rep(ptNoise,nrow(sample))
    storage.mode(ptNoise) ="numeric"
    sample <- t(sample)
    mean <- t(pt)
    out <- .Call("ComputeCoefficientsForPointValuesWithCovariance",model,sample,mean,ptNoise)
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValuesWithCovariance", signature(model="pPCA_pointer", sample="numeric",pt="numeric",ptNoise="numeric"), function(model,sample,pt,ptNoise=0) {
    
    if (length(ptNoise) == 1)
        ptNoise <- rep(ptNoise,length(sample))
    sample <- matrix(sample,3,1)
    if (length(pt) == 3)
        mean <- matrix(pt,3,1)
    else if (length(pt) == 1)
        mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    else
        stop("in this case pt must be a vector of length 3 or an integer")
    storage.mode(ptNoise) ="numeric"
    out <- .Call("ComputeCoefficientsForPointValuesWithCovariance",model,sample,mean,ptNoise)
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValuesWithCovariance", signature(model="pPCA_pointer", sample="matrix",pt="numeric",ptNoise="matrix"), function(model,sample,pt,ptNoise=0) {
    if (nrow(ptNoise) != length(sample))
        stop("you need to specify a 3x3 covariance matrix for each point")
    sample <- t(sample)
    mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    storage.mode(ptNoise) ="numeric"
    out <- .Call("ComputeCoefficientsForPointValuesWithCovariance",model,sample,mean,ptNoise)
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValuesWithCovariance", signature(model="pPCA_pointer", sample="matrix",pt="matrix",ptNoise="matrix"), function(model,sample,pt,ptNoise=0) {
    if (nrow(ptNoise) != length(sample))
        stop("you need to specify a 3x3 covariance matrix for each point")
    sample <- t(sample)
    mean <- t(pt)
    storage.mode(ptNoise) ="numeric"
    out <- .Call("ComputeCoefficientsForPointValuesWithCovariance",model,sample,mean,ptNoise)
    return(out)
})

#' @rdname statismoMembers
setMethod("ComputeCoefficientsForPointValuesWithCovariance", signature(model="pPCA_pointer", sample="numeric",pt="numeric",ptNoise="matrix"), function(model,sample,pt,ptNoise=0) {
    if (nrow(ptNoise) != length(sample))
        stop("you need to specify a 3x3 covariance matrix for each point")
    sample <- matrix(sample,3,1)
    if (length(pt) == 3)
        mean <- matrix(pt,3,1)
    else if (length(pt) == 1)
        mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    else
        stop("in this case pt must be a vector of length 3 or an integer")
    storage.mode(ptNoise) ="numeric"
    out <- .Call("ComputeCoefficientsForPointValuesWithCovariance",model,sample,mean,ptNoise)
    return(out)
})



#' @rdname statismoMembers
setMethod("EvaluateSampleAtPoint", signature(model="pPCA_pointer", sample="matrix",pt="numeric"), function(model,sample,pt) {
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
setMethod("EvaluateSampleAtPoint", signature(model="pPCA_pointer", sample="mesh3d",pt="numeric"), function(model,sample,pt) {
    sample <- vert2points(sample)
    out <- EvaluateSampleAtPoint(model,sample,pt)
    return(out)
})



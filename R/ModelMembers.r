
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


setMethod("GetPCABasisMatrixIn", signature(model="pPCA"), function(model) {
    WT <- t(GetPCABasisMatrix(model))
    Mmatrix <- crossprod(GetPCABasisMatrix(model))
    diag(Mmatrix) <- diag(Mmatrix)+model@sigma
    Mmatrixinv <- solve(Mmatrix)
    Win <- Mmatrixinv%*%WT
    return(Win)
})


setMethod("DrawMean",  signature(model="pPCA"), function(model) {
    if (!inherits(model,"pPCA"))
        stop("please provide model of class 'pPCA'")
    out <- output2sample(.Call("DrawMean",model))
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

setMethod("ComputeCoefficientsForDataset",signature(model="pPCA"), function(model,dataset) {
    out <- .Call("ComputeCoefficientsForDataset",model,dataset2representer(dataset))
    return(out)
})
setGeneric("UpdateVariance", function(model) standardGeneric("UpdateVariance"))
setMethod("UpdateVariance", "pPCA",function(model) {
    Variance <- createVarTable(model@PCA$sdev,square=TRUE)
    SetVariance(model) <- Variance
    return(model)
})

setMethod("GetDomainPoints", signature(model="pPCA"), function(model) {
    out <- t(.Call("GetDomainPoints",model))
    return(out)
})

setMethod("GetDomainSize", signature(model="pPCA"), function(model) {
    out <- ncol(model@representer$vb)
    return(out)
})

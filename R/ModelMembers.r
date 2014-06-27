#' Implementation/Emulation of the statsimo StatisticalModel class.
#'
#' Implementation/Emulation of the statsimo StatisticalModel class.
#' @param model object of class \code{\link{pPCA}}
#' @param dataset an (already aligned) mesh or k x 3 matrix containing the datasets coordinates.
#' @param coefficients specify coefficients in the latent space to draw a sample
#' @param addNoise logical: if TRUE noise as specified in the model will be added to the returned sample
#' @return functions return matrices, (log)-probabilties, coefficients or sample (mesh3d or matrix) for specific dataset
#' @details see \url{http://statismo.github.io/statismo/classdoc/html/classstatismo_1_1StatisticalModel.html} for details.
#' @keywords StatisticalModel<representer>
#' @name StatismoModelMembers
#' @rdname statismoMembers
#' @export
setGeneric("GetPCABasisMatrix", function(model) {
  standardGeneric("GetPCABasisMatrix")
})
setMethod("GetPCABasisMatrix", signature(model = "pPCA"), function(model) {
   W <- t(t(model@PCA$rotation)*model@PCA$sdev) ##Matrix to project scaled PC-scores back into the config space
    return(W)
})

#' @rdname statismoMembers
#' @docType methods
#' @export
setGeneric("GetOrthonormalPCABasisMatrix", function(model) {
  standardGeneric("GetOrthonormalPCABasisMatrix")
})
#' @export
setMethod("GetOrthonormalPCABasisMatrix" ,signature(model="pPCA"),function(model) {
    return(model@PCA$rotation)
})

#' @rdname statismoMembers
#' @export
setGeneric("GetNoiseVariance", function(model) {
    standardGeneric("GetNoiseVariance")
})
setMethod("GetNoiseVariance",signature(model = "pPCA"), function(model) {
    return(model@sigma)
})
#' @rdname statismoMembers
#' @export
GetMeanVector <- function(model) {
    return(model@PCA$center)
}
#' @rdname statismoMembers
#' @export
GetPCAVarianceVector <- function(model) {
    return(model@PCA$sdev^2)
}
#' @rdname statismoMembers
#' @export
ComputeLogProbabilityOfDataset <- function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),TRUE)
    return(out)
}
#' @rdname statismoMembers
#' @export
ComputeProbabilityOfDataset <- function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),FALSE)
    return(out)
}

GetPCABasisMatrixIn <- function(model) {
    ##this the more complicated version directly from StatisticalModel.txx
    WT <- t(GetPCABasisMatrix(model))
    Mmatrix <- crossprod(GetPCABasisMatrix(model))
    diag(Mmatrix) <- diag(Mmatrix)+model@sigma
    Mmatrixinv <- solve(Mmatrix)
    Win <- Mmatrixinv%*%WT
    #Win <- (t(model@PCA$rotation)*(1/sqrt(model@PCA$sdev^2+model@sigma))) ##Matrix to project scaled PC-scores back into the config space
    return(Win)
}
#' @rdname statismoMembers
#' @export
DrawMean <- function(model) {
    if (!inherits(model,"pPCA"))
        stop("please provide model of class 'pPCA'")
    out <- output2sample(.Call("DrawMean",model))
    return(out)
}

#' @rdname statismoMembers
#' 
#' @export
DrawSample <- function(model, coefficients=NULL, addNoise=FALSE) {
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
}
    
    
#' @rdname statismoMembers
#' @export
ComputeCoefficientsForDataset <- function(model,dataset) {
    out <- .Call("ComputeCoefficientsForDataset",model,dataset2representer(dataset))
    return(out)
}

output2sample <- function(out) {
    if (inherits(out,"mesh3d"))
        out$vb <- rbind(out$vb,1)
    else
        out <- t(out$vb)
    return(out)
}

#' predict or restrict a mesh or matrix based on a statistical model
#'
#' predict or restrict a mesh or matrix based on a statistical model

#' @param model model of class \code{pPCA}
#' @param dataset a matrix or a mesh3d. If \code{dataset} is missing, \code{lmDataset} and \code{lmModel} need to be provided and the posterior mean will be returned.
#' @param representer if TRUE and the model contains a representer mesh, a surface mesh will be returned, coordinate matrix otherwise.
#' @param origSpace logical: rotate the estimation back into the original coordinate system.
#' @param lmDataset optional: landmarks on the dataset used for alignment.
#' @param lmModel optional: landmarks on the model's mean used for alignment.
#' @param sdmax maximum allowed standard deviation (per Principal axis) within the model space. Defines the probabilistic boundaries.
#' @param mahaprob character: if != "none", use mahalanobis-distance to determine overall probability (of the shape projected into the model space."chisq" uses the Chi-Square distribution of the squared Mahalanobisdistance, while "dist" restricts the values to be within a multi-dimensional sphere of radius \code{sdmax}. If FALSE the probability will be determined per PC separately.
#' @param align if TRUE, the sample will be aligned to the mean.
#' @param addNoise re-add noise while reprojecting from latent into shape space.
#' @param posteriorMean if TRUE, instead the shape will be the mean of the posterior model using the coordinates defined by \code{lmModel} and \code{lmDataset}.
#' @param ptValueNoise numeric: set global noise assumed in the data when \code{posteriorMean = TRUE}
#' @param ... currently not in use.
#' 
#' @return \code{PredictSample} returns a matrix/mesh3d restricted to the boundaries given by the modelspace.
#'
#' @seealso \code{\link{StatismoModelMembers}}
#' @name PredictSample
#' @rdname PredictSample
#' @export


#' @rdname PredictSample
#' @export
setGeneric("PredictSample",function(model,dataset,representer=TRUE,...) {
    standardGeneric("PredictSample")
})

#' @rdname PredictSample
#' @export
setMethod("PredictSample", signature(model="pPCA",dataset="matrix"),function(model, dataset,representer=TRUE,origSpace=TRUE,lmDataset=NULL, lmModel=NULL,sdmax=NULL,mahaprob=c("none","chisq","dist"),align=TRUE, addNoise=FALSE,posteriorMean=FALSE,ptValueNoise=1, ...) {
    #mahaprob <- substr(mahaprob[1],1L,1L)
    mshape <- getMeanMatrix(model,transpose=TRUE)
    hasLM <- FALSE
    if (!is.null(lmDataset) && !is.null(lmModel))
        hasLM <- TRUE
    if (align) {
        if (!hasLM) {
            rotsb <- rotonto(mshape,dataset,scale=model@scale,reflection = F)
            sb <- rotsb$yrot
        } else {
            rotsb <- rotonto(lmModel,lmDataset,scale=model@scale,reflection=F)
            sb <- rotonmat(dataset,lmDataset,rotsb$yrot)
            lmDataset <- rotsb$yrot
        }
    } else
        sb <- dataset
   
    if (hasLM && posteriorMean) 
        alpha <- ComputeCoefficientsForPointValues(model,lmDataset,lmModel,ptNoise = ptValueNoise)
    else 
        alpha <- ComputeCoefficientsForDataset(model,sb)
    sdl <- length(model@PCA$sdev)
    if (!is.null(sdmax)) {
        alpha <- constrainParams(alpha,sdmax=sdmax,mahaprob=mahaprob)
    }
    if ("coeffs" %in% names(list(...))) {
        return(alpha)
        
    } else {
        if (representer)
            estim <- DrawSample(model,coefficients=alpha,addNoise = addNoise)
        else
            estim <- t(matrix(DrawSampleVector(model,coefficients=alpha,addNoise = addNoise),ncol(dataset),nrow(dataset)))
        if (origSpace && align)
            estim <- rotreverse(estim,rotsb)
        return(estim)
    }
})


#' @rdname PredictSample
#' @export
setMethod("PredictSample",signature(model="pPCA",dataset="mesh3d"), function(model,dataset,representer=TRUE,origSpace=TRUE, lmDataset=NULL, lmModel=NULL,sdmax=NULL,mahaprob=c("none","chisq","dist"),align=TRUE,addNoise=FALSE,posteriorMean=FALSE,ptValueNoise=1, ...) {
    mat <- t(dataset$vb[1:3,])
    estim <- PredictSample(model,vert2points(dataset),align=align,representer=representer,sdmax=sdmax,origSpace=origSpace,lmDataset=lmDataset, lmModel=lmModel,mahaprob=mahaprob,posteriorMean=posteriorMean,ptValueNoise=ptValueNoise,addNoise=addNoise,...)
    return(estim)
})

#' @rdname PredictSample
#' @export
setMethod("PredictSample",signature(model="pPCA",dataset="missing"), function(model,dataset,representer=TRUE,origSpace=TRUE, lmDataset=NULL, lmModel=NULL,sdmax=NULL,mahaprob=c("none","chisq","dist"),align=TRUE,addNoise=FALSE,posteriorMean=FALSE,ptValueNoise=1,...) {
    hasLM <- FALSE
    if (!is.null(lmDataset) && !is.null(lmModel))
        hasLM <- TRUE
    if (!hasLM)
        stop("if dataset is missing you need to provide landmarks")
    else if (align) {
        rotsb <- rotonto(lmModel,lmDataset,scale=model@scale,reflection=F)
        lmDataset <- rotsb$yrot
    }
    posteriorMean <- TRUE
    alpha <- ComputeCoefficientsForPointValues(model,lmDataset,lmModel,ptNoise = ptValueNoise)
    sdl <- length(model@PCA$sdev)
    if (!is.null(sdmax)) {
        alpha <- constrainParams(alpha,sdmax=sdmax,mahaprob=mahaprob)
    }
    if ("coeffs" %in% names(list(...))) {
        return(alpha)
        
    } else {
        if (representer)
            estim <- DrawSample(model,coefficients=alpha,addNoise = addNoise)
        else {
            vec <- DrawSampleVector(model,coefficients=alpha,addNoise = addNoise)
            estim <- t(matrix(vec,ncol(lmDataset),length(vec)/ncol(lmDataset)))
        }
        if (origSpace && align)
            estim <- rotreverse(estim,rotsb)
        return(estim)
    }
        
    })

constrainParams <- function(alpha,sdmax=3,mahaprob=c("none","chisq","dist")) {
    
    mahaprob <- match.arg(mahaprob,c("none","chisq","dist"))
    if (mahaprob != "none") {
        sdl <- length(alpha)
        probs <- sum(alpha^2)
        if (mahaprob == "chisq") {
            Mt <- qchisq(1-2*pnorm(sdmax,lower.tail=F),df=sdl)
            probs <- sum(alpha^2)
        } else if (mahaprob == "dist") {
            Mt <- sdmax
            probs <- sqrt(probs)
        }
        if (probs > Mt ) {
            sca <- Mt/probs
            alpha <- alpha*sca
        }
    } else { 
        signalpha <- sign(alpha)
        alpha <- abs(alpha)
        outlier <- which(alpha > sdmax)
        alpha[outlier] <- sdmax
        alpha <- alpha*signalpha
    }
    return(alpha)
}

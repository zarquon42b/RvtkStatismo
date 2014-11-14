#' predict or restrict a mesh or matrix based on a statistical model
#'
#' predict or restrict a mesh or matrix based on a statistical model

#' @param model model of class \code{pPCA}
#' @param dataset a matrix or a mesh3d
#' @param representer if TRUE and the model contains a representer mesh, a surface mesh will be returned, coordinate matrix otherwise.
#' @param origSpace logical: rotate the estimation back into the original coordinate system.
#' @param lmDataset optional: landmarks on the dataset used for alignment.
#' @param lmModel optional: landmarks on the model's mean used for alignment.
#' @param sdmax maximum allowed standard deviation (per Principal axis) within the model space. Defines the probabilistic boundaries.
#' @param mahaprob character: if != "none", use mahalanobis-distance to determine overall probability (of the shape projected into the model space."chisq" uses the Chi-Square distribution of the squared Mahalanobisdistance, while "dist" restricts the values to be within a multi-dimensional sphere of radius \code{sdmax}. If FALSE the probability will be determined per PC separately.
#' @param align if TRUE, the sample will be aligned to the mean.
#' @param addNoise re-add noise while reprojecting from latent into shape space.
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
setMethod("PredictSample", signature(model="pPCA",dataset="matrix"),function(model, dataset,representer=TRUE,origSpace=TRUE,lmDataset=NULL, lmModel=NULL,sdmax=NULL,mahaprob=c("none","chisq","dist"),align=TRUE, addNoise=FALSE, ...) {
    mahaprob <- substr(mahaprob[1],1L,1L)
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
        }
    } else
        sb <- dataset

    alpha <- ComputeCoefficientsForDataset(model,sb)
    sdl <- length(model@PCA$sdev)
    if (!is.null(sdmax)) {
        if (mahaprob != "n") {
            sdl <- length(model@PCA$sdev)
            probs <- sum(alpha^2)
            if (mahaprob == "c") {
                Mt <- qchisq(1-2*pnorm(sdmax,lower.tail=F),df=sdl)
                probs <- sum(alpha^2)
            } else if (mahaprob == "d") {
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
setMethod("PredictSample",signature(model="pPCA",dataset="mesh3d"), function(model,dataset,representer=TRUE,origSpace=TRUE, lmDataset=NULL, lmModel=NULL,sdmax=NULL,mahaprob=c("none","chisq","dist"),align=TRUE,...) {
    mat <- t(dataset$vb[1:3,])
    estim <- PredictSample(model,vert2points(dataset),align=align,representer=representer,sdmax=sdmax,origSpace=origSpace,lmDataset=lmDataset, lmModel=lmModel,mahaprob=mahaprob,...)
    return(estim)
})


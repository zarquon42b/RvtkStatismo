
#' @rdname PredictSample
#' @export
setMethod("PredictSample", signature(model="pPCA_pointer",dataset="matrix"),function(model, dataset,representer=TRUE,origSpace=TRUE,lmDataset=NULL, lmModel=NULL,sdmax=NULL,mahaprob=c("none","chisq","dist"),align=TRUE, addNoise=FALSE,posteriorMean=FALSE,ptValueNoise=1, ...) {
    #mahaprob <- substr(mahaprob[1],1L,1L)
    mshape <- GetDomainPoints(model)
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
        alpha <- ComputeCoefficients(model,sb)
    sdl <- GetNumberOfPrincipalComponents(model)
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
setMethod("PredictSample",signature(model="pPCA_pointer",dataset="mesh3d"), function(model,dataset,representer=TRUE,origSpace=TRUE, lmDataset=NULL, lmModel=NULL,sdmax=NULL,mahaprob=c("none","chisq","dist"),align=TRUE,addNoise=FALSE,posteriorMean=FALSE,ptValueNoise=1, ...) {
    mat <- t(dataset$vb[1:3,])
    estim <- PredictSample(model,vert2points(dataset),align=align,representer=representer,sdmax=sdmax,origSpace=origSpace,lmDataset=lmDataset, lmModel=lmModel,mahaprob=mahaprob,posteriorMean=posteriorMean,ptValueNoise=ptValueNoise,addNoise=addNoise,...)
    return(estim)
})

#' @rdname PredictSample
#' @export
setMethod("PredictSample",signature(model="pPCA_pointer",dataset="missing"), function(model,dataset,representer=TRUE,origSpace=TRUE, lmDataset=NULL, lmModel=NULL,sdmax=NULL,mahaprob=c("none","chisq","dist"),align=TRUE,addNoise=FALSE,posteriorMean=FALSE,ptValueNoise=1,...) {
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
    alpha <- ComputeCoefficientsForPointValuesWithCovariance(model,lmDataset,lmModel,ptNoise = ptValueNoise)
    sdl <- GetNumberOfPrincipalComponents(model)
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

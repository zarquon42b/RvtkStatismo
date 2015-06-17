#' constrain a model of class pPCA
#'
#' constrain a model of class pPCA
#' @param model object of class pPCA
#' @param sample k x 3 matrix containing coordinates to constrain model to
#' @param pt either a matrix with each row containing points on the model's domain corresponding to the row in \code{sample} or an integer vector specifying the coordinates of the sample's mean corresponding to \code{sample} 
#' @param ptValueNoise numeric: specify noise on constraints.
#' @return a constrained model
#' @rdname statismoConstrainModel
#' @name statismoConstrainModel
#' @docType methods
#' @export
setGeneric("statismoConstrainModel",function(model,sample,pt,ptValueNoise){
    standardGeneric("statismoConstrainModel")})

#' @rdname statismoConstrainModel
setMethod("statismoConstrainModel",signature(model="pPCA",sample="matrix",pt="matrix"), function(model,sample,pt,ptValueNoise) {
    ptValueNoise <- max(1e-7,ptValueNoise)
    mean <- t(pt)
    sample <- t(sample)
    out <- .Call("PosteriorModel",model,sample, mean,ptValueNoise)
    return(out)
})
#' @rdname statismoConstrainModel
setMethod("statismoConstrainModel",signature(model="pPCA",sample="matrix",pt="numeric"), function(model,sample,pt,ptValueNoise) {
    ptValueNoise <- max(1e-12,ptValueNoise)
    mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    sample <- t(sample)
    out <- .Call("PosteriorModel",model,sample, mean,ptValueNoise)
    return(out)
})
#' @rdname statismoConstrainModel
setMethod("statismoConstrainModel",signature(model="pPCA",sample="numeric",pt="numeric"), function(model,sample,pt,ptValueNoise) {
    ptValueNoise <- max(1e-12,ptValueNoise)
    sample <- matrix(sample,3,1)
    if (length(pt) == 3)
        mean <- matrix(pt,3,1)
    else if (length(pt) == 1)
        mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    else
        stop("in this case pt must be a vector of length 3 or an integer")
    out <- .Call("PosteriorModel",model,sample, mean,ptValueNoise)
    return(out)
})

#' calculate a posterior model but only use likely correspondences
#'
#' calculate a posterior model but only use likely correspondences
#' @param model object of class pPCA
#' @param sample matrix containing coordinates to constrain model to
#' @param pt either a k x 3 matrix with each row containing points on the model's domain corresponding to the row in \code{sample} or an integer vector specifying the coordinates of the sample's mean corresponding to \code{sample} 
#' @param ptValueNoise numeric: specify noise on constraints.
#' @param sdmax a measure in standard deviations to allow the likelihood of the correspondeces between sample and model. (using chi-square distribution)
#' @return a constrained model
#' @rdname statismoConstrainModelSafe
#' @name statismoConstrainModelSafe
#' @export
setGeneric("statismoConstrainModelSafe",function(model,sample,pt,ptValueNoise,sdmax=5){
    standardGeneric("statismoConstrainModelSafe")})

#' @rdname statismoConstrainModelSafe
setMethod("statismoConstrainModelSafe",signature(model="pPCA",sample="matrix",pt="numeric"), function(model,sample,pt,ptValueNoise,sdmax=5) {
    ptValueNoise <- max(1e-12,ptValueNoise)
    mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    sample <- t(sample)
    mahamax <- sqrt(qchisq(1-2*pnorm(sdmax,lower.tail=F),df=3))
    out <- .Call("PosteriorModelSafe",model,sample, mean,ptValueNoise,mahamax)
})

#' @rdname statismoConstrainModelSafe
setMethod("statismoConstrainModelSafe",signature(model="pPCA",sample="matrix",pt="matrix"), function(model,sample,pt,ptValueNoise,sdmax=5) {
    ptValueNoise <- max(1e-12,ptValueNoise)
    mean <- t(pt)
    sample <- t(sample)
    mahamax <- sqrt(qchisq(1-2*pnorm(sdmax,lower.tail=F),df=3))
    out <- .Call("PosteriorModelSafe",model,sample, mean,ptValueNoise,mahamax)
})

#' @rdname statismoConstrainModelSafe
setMethod("statismoConstrainModelSafe",signature(model="pPCA",sample="numeric",pt="numeric"), function(model,sample,pt,ptValueNoise,sdmax=5) {
    ptValueNoise <- max(1e-12,ptValueNoise)
    sample <- matrix(sample,3,1)
    if (length(pt) == 3)
        mean <- matrix(pt,3,1)
    else if (length(pt) == 1)
        mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    else
        stop("in this case pt must be a vector of length 3 or an integer")
    mahamax <- sqrt(qchisq(1-2*pnorm(sdmax,lower.tail=F),df=3))
    out <- .Call("PosteriorModelSafe",model,sample, mean,ptValueNoise,mahamax)
})

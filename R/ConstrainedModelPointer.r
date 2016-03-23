
#' @rdname statismoConstrainModel
setMethod("statismoConstrainModel",signature(model="pPCA_pointer",sample="matrix",pt="matrix"), function(model,sample,pt,ptValueNoise, computeScores=TRUE, pointer=TRUE) {
    ptValueNoise <- checkpointValueNoise(ptValueNoise,sample)
    mean <- t(pt)
    sample <- t(sample)
     out <- .Call("PosteriorModel",model,sample, mean,ptValueNoise,computeScores,pointer)
    return(out)
})

#' @rdname statismoConstrainModel
setMethod("statismoConstrainModel",signature(model="pPCA_pointer",sample="matrix",pt="numeric"), function(model,sample,pt,ptValueNoise, computeScores=TRUE, pointer=TRUE) {
    ptValueNoise <- checkpointValueNoise(ptValueNoise,sample)
    mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    sample <- t(sample)
     out <- .Call("PosteriorModel",model,sample, mean,ptValueNoise,computeScores,pointer)
    return(out)
})

#' @rdname statismoConstrainModel
setMethod("statismoConstrainModel",signature(model="pPCA_pointer",sample="numeric",pt="numeric"), function(model,sample,pt,ptValueNoise, computeScores=TRUE, pointer=TRUE) {
    ptValueNoise <- checkpointValueNoise(ptValueNoise,sample)
              sample <- matrix(sample,3,1)
              if (length(pt) == 3)
                  mean <- matrix(pt,3,1)
              else if (length(pt) == 1)
                  mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
              else
                  stop("in this case pt must be a vector of length 3 or an integer")
    out <- .Call("PosteriorModel",model,sample, mean,ptValueNoise,computeScores,pointer)
    return(out)
          })


#' @rdname statismoConstrainModelSafe
setMethod("statismoConstrainModelSafe",signature(model="pPCA_pointer",sample="matrix",pt="numeric"), function(model,sample,pt,ptValueNoise,sdmax=5,computeScores=TRUE, pointer=TRUE) {
    ptValueNoise <- checkpointValueNoise(ptValueNoise,sample)
    mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    sample <- t(sample)
    mahamax <- sqrt(qchisq(1-2*pnorm(sdmax,lower.tail=F),df=3))
    out <- .Call("PosteriorModelSafe",model,sample, mean,ptValueNoise,mahamax,computeScores,pointer)
})

#' @rdname statismoConstrainModelSafe
setMethod("statismoConstrainModelSafe",signature(model="pPCA_pointer",sample="matrix",pt="matrix"), function(model,sample,pt,ptValueNoise,sdmax=5,computeScores=TRUE, pointer=TRUE) {
    ptValueNoise <- checkpointValueNoise(ptValueNoise,sample)
    mean <- t(pt)
    sample <- t(sample)
    mahamax <- sqrt(qchisq(1-2*pnorm(sdmax,lower.tail=F),df=3))
    out <- .Call("PosteriorModelSafe",model,sample, mean,ptValueNoise,mahamax,computeScores,pointer)
})

#' @rdname statismoConstrainModelSafe
setMethod("statismoConstrainModelSafe",signature(model="pPCA_pointer",sample="numeric",pt="numeric"), function(model,sample,pt,ptValueNoise,sdmax=5,computeScores=TRUE, pointer=TRUE) {
    ptValueNoise <- checkpointValueNoise(ptValueNoise,sample)               
    sample <- matrix(sample,3,1)
    if (length(pt) == 3)
        mean <- matrix(pt,3,1)
    else if (length(pt) == 1)
        mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
    else
        stop("in this case pt must be a vector of length 3 or an integer")
    mahamax <- sqrt(qchisq(1-2*pnorm(sdmax,lower.tail=F),df=3))
   out <- .Call("PosteriorModelSafe",model,sample, mean,ptValueNoise,mahamax,computeScores,pointer)
})

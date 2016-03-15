
#' @rdname statismoConstrainModel
setMethod("statismoConstrainModel",signature(model="pPCA_pointer",sample="matrix",pt="matrix"), function(model,sample,pt,ptValueNoise,pointer=TRUE) {
             
              if (length(ptValueNoise) == 1) {
                  ptValueNoise <- max(1e-7,ptValueNoise)
              } else {
                  if (length(ptValueNoise) != nrow(sample))
                      stop("each entries in ptValueNoise != number of sample points")
                  
                  ptValueNoise[which(ptValueNoise < 1e-7)] <- 1e-7
              }
              ptValueNoise <- as.matrix(ptValueNoise)
              mean <- t(pt)
              sample <- t(sample)
              out <- .Call("PosteriorModel",model,sample, mean,ptValueNoise,pointer)
              return(out)
          })
#' @rdname statismoConstrainModel
setMethod("statismoConstrainModel",signature(model="pPCA_pointer",sample="matrix",pt="numeric"), function(model,sample,pt,ptValueNoise,pointer=TRUE) {
              if (length(ptValueNoise) == 1) {
                  ptValueNoise <- max(1e-7,ptValueNoise)
              } else {
                  if (length(ptValueNoise) != nrow(sample))
                      stop("each entries in ptValueNoise != number of sample points")
                  ptValueNoise[which(ptValueNoise < 1e-7)] <- 1e-7
              }
              ptValueNoise <- as.matrix(ptValueNoise)
              mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
              sample <- t(sample)
              out <- .Call("PosteriorModel",model,sample, mean,ptValueNoise)
              return(out)
          })
#' @rdname statismoConstrainModel
setMethod("statismoConstrainModel",signature(model="pPCA_pointer",sample="numeric",pt="numeric"), function(model,sample,pt,ptValueNoise,pointer=TRUE) {
              if (length(ptValueNoise) == 1) {
                  ptValueNoise <- max(1e-7,ptValueNoise)
              } else {
                  if (length(ptValueNoise) != nrow(sample))
                      stop("each entries in ptValueNoise != number of sample points")
                  ptValueNoise[which(ptValueNoise < 1e-7)] <- 1e-7
              }
              ptValueNoise <- as.matrix(ptValueNoise)
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


#' @rdname statismoConstrainModelSafe
setMethod("statismoConstrainModelSafe",signature(model="pPCA_pointer",sample="matrix",pt="numeric"), function(model,sample,pt,ptValueNoise,sdmax=5,pointer=TRUE) {
              if (length(ptValueNoise) == 1) {
                  ptValueNoise <- max(1e-7,ptValueNoise)
              } else {
                  if (length(ptValueNoise) != nrow(sample))
                      stop("each entries in ptValueNoise != number of sample points")
                  ptValueNoise <- as.matrix(ptValueNoise)
                  ptValueNoise[which(ptValueNoise < 1e-7)] <- 1e-7
              }
              ptValueNoise <- as.matrix(ptValueNoise)
              mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
              sample <- t(sample)
              mahamax <- sqrt(qchisq(1-2*pnorm(sdmax,lower.tail=F),df=3))
              out <- .Call("PosteriorModelSafe",model,sample, mean,ptValueNoise,mahamax,pointer)
          })

#' @rdname statismoConstrainModelSafe
setMethod("statismoConstrainModelSafe",signature(model="pPCA_pointer",sample="matrix",pt="matrix"), function(model,sample,pt,ptValueNoise,sdmax=5,pointer=TRUE) {
              if (length(ptValueNoise) == 1) {
                  ptValueNoise <- max(1e-7,ptValueNoise)
              } else {
                  if (length(ptValueNoise) != nrow(sample))
                      stop("each entries in ptValueNoise != number of sample points")
                  ptValueNoise[which(ptValueNoise < 1e-7)] <- 1e-7
              }
              ptValueNoise <- as.matrix(ptValueNoise)
              mean <- t(pt)
              sample <- t(sample)
              mahamax <- sqrt(qchisq(1-2*pnorm(sdmax,lower.tail=F),df=3))
              out <- .Call("PosteriorModelSafe",model,sample, mean,ptValueNoise,mahamax,pointer)
          })

#' @rdname statismoConstrainModelSafe
setMethod("statismoConstrainModelSafe",signature(model="pPCA_pointer",sample="numeric",pt="numeric"), function(model,sample,pt,ptValueNoise,sdmax=5,pointer=TRUE) {
              if (length(ptValueNoise) == 1) {
                  ptValueNoise <- max(1e-7,ptValueNoise)
              } else {
                  if (length(ptValueNoise) != nrow(sample))
                      stop("each entries in ptValueNoise != number of sample points")
                  ptValueNoise[which(ptValueNoise < 1e-7)] <- 1e-7
              }
               ptValueNoise <- as.matrix(ptValueNoise)
               
              sample <- matrix(sample,3,1)
              if (length(pt) == 3)
                  mean <- matrix(pt,3,1)
              else if (length(pt) == 1)
                  mean <- t(GetDomainPoints(model))[,pt,drop=FALSE]
              else
                  stop("in this case pt must be a vector of length 3 or an integer")
              mahamax <- sqrt(qchisq(1-2*pnorm(sdmax,lower.tail=F),df=3))
              out <- .Call("PosteriorModelSafe",model,sample, mean,ptValueNoise,mahamax,pointer)
          })

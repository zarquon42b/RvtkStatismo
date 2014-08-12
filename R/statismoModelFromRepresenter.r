#' generate model from a representer using gaussian kernels
#'
#' generate model from a representer using gaussian kernels
#'
#' @param representer mesh3d or matrix used as representer
#' @param kernel a list containing two valued vectors containing with the first entry specifiying the bandwidth and the second the scaling of the Gaussian kernels.
#' @param ncomp integer: number of PCs to approximate
#' @param nystroem number of samples to compute Nystroem approximation of eigenvectors
#' @return returns a shape model of class \code{\link{pPCA}}
#' @examples
#' require(Rvcg)
#' data(humface)
#' hummodel <- statismoModelFromRepresenter(humface)
#' require(rgl)
#' for (i in 1:5) wire3d(DrawSample(hummodel),col=i)
#' @export
statismoModelFromRepresenter <- function(representer,kernel=list(c(100,70)),ncomp=10,nystroem=500) {
    representer <- dataset2representer(representer)
    center <- as.vector(representer$vb[1:3,])
    pp <- new("pPCA")
    pp@sigma <- 0
    pp@PCA$center <- center
    pp@PCA$sdev <- 1
    pp@representer <- representer
    pp@PCA$rotation <- matrix(0,length(pp@PCA$center),1)
    out <- statismoGPmodel(pp,useEmpiric=FALSE,kernel=kernel,ncomp=ncomp,nystroem=nystroem)
    return(out)
}
    

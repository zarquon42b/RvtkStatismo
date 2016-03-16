#' generate model from a representer using gaussian kernels
#'
#' generate model from a representer using gaussian kernels
#'
#' @param representer mesh3d or matrix used as representer
#' @param kernel an object of class matrixKernel  
#' @param ncomp integer: number of PCs to approximate
#' @param nystroem number of samples to compute Nystroem approximation of eigenvectors
#' @param pointer if TRUE an object of class pPCA_pointer is returned
#' @return returns a shape model of class \code{\link{pPCA}}
#' @examples
#' require(Rvcg)
#' data(humface)
#' kernel1 <- GaussianKernel(80,50)
#' kernel2 <- GaussianKernel(20,20)
#' combinedKernel <- SumKernels(kernel1,kernel2)
#' hummodel <- statismoModelFromRepresenter(humface,combinedKernel)
#' \dontrun{
#' require(rgl)
#' for (i in 1:5) wire3d(DrawSample(hummodel),col=i)
#' }
#' @export
statismoModelFromRepresenter <- function(representer,kernel=GaussianKernel(100,50),ncomp=10,nystroem=500,pointer=FALSE) {
    representer <- dataset2representer(representer)
    center <- as.vector(representer$vb[1:3,])
    pp <- new("pPCA")
    pp@sigma <- 0
    pp@PCA$center <- center
    pp@PCA$sdev <- 1
    pp@representer <- representer
    pp@PCA$rotation <- matrix(0,length(pp@PCA$center),1)
    centroid <- apply(GetDomainPoints(pp),2,mean)
    out <- statismoGPmodel(pp,kernel=kernel,ncomp=ncomp,nystroem=nystroem,pointer=pointer,empiric = "none")
    return(out)
}
    

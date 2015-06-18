#' generate model from a representer using gaussian kernels
#'
#' generate model from a representer using gaussian kernels
#'
#' @param representer mesh3d or matrix used as representer
#' @param kernel a list containing two valued vectors containing with the first entry specifiying the bandwidth and the second the scaling of the Gaussian kernels.
#' @param ncomp integer: number of PCs to approximate
#' @param nystroem number of samples to compute Nystroem approximation of eigenvectors
#' @param combine character determining how to combine the kernels: "sum" or "product" are supported.
#' @param isoScale standard deviation of isotropic scaling. 
#' @param centroid specify the center of scaling. If NULL, the centroid will be used.
#' @return returns a shape model of class \code{\link{pPCA}}
#' @examples
#' require(Rvcg)
#' data(humface)
#' hummodel <- statismoModelFromRepresenter(humface)
#' \dontrun{
#' require(rgl)
#' for (i in 1:5) wire3d(DrawSample(hummodel),col=i)
#' }
#' @export
statismoModelFromRepresenter <- function(representer,kernel=list(c(100,70)),ncomp=10,nystroem=500,combine="sum",isoScale=0, centroid=NULL) {
    representer <- dataset2representer(representer)
    center <- as.vector(representer$vb[1:3,])
    pp <- new("pPCA")
    pp@sigma <- 0
    pp@PCA$center <- center
    pp@PCA$sdev <- 1
    pp@representer <- representer
    pp@PCA$rotation <- matrix(0,length(pp@PCA$center),1)
    centroid <- apply(GetDomainPoints(pp),2,mean)
    out <- statismoGPmodel(pp,useEmpiric=FALSE,kernel=kernel,ncomp=ncomp,nystroem=nystroem,combine = combine,combineEmp=0,isoScale=isoScale,centroid=centroid)
    return(out)
}
    

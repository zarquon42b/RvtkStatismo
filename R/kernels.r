#' create a scalar valued kernel of type MultiscaleBsplineKernel
#'
#' create a scalar valued kernel of type MultiscaleBsplineKernel
#' @param support suppor value for B-spline
#' @param levels number of levels
#' @return object of class scalarKernel
#' @examples
#' kernel1 <- MultiscaleBsplineKernel(100,5)
#' @export
MultiscaleBsplineKernel <- function(support=100,levels=2) {
    out <- new("scalarKernel")
    kernel <- .Call("RscalarValuedKernel",1,support,levels);
    out@pointer <- kernel
    out@kerneltype="MultiscaleBsplineKernel"
    return(out)
}

#' create a scalar valued kernel of type GaussianKernel
#'
#' create a scalar valued kernel of type GaussianKernel
#' @param sigma  bandwidth of Gausian kernel
#' @param levels number of levels
#' @return object of class scalarKernel
#' @examples
#' gkernel <- GaussianKernel(2)
#' @export
GaussianKernel <- function(sigma=50) {
    out <- new("scalarKernel")
    kernel <- .Call("RscalarValuedKernel",0,sigma,0);
    out@pointer <- kernel
    out@kerneltype="GaussianKernel"
    return(out)
}

#' create a matrix valued kernel
#'
#' create a matrix valued kernel
#' @param scalarKernel object of class scalarKernel
#' @param scale scale factor of kernel
#' @return object of class matrixKernel
#' @examples
#' gkernel <- GaussianKernel(2)
#' mvkernel <- MatrixValuedKernel(gkernel)
#' @export
MatrixValuedKernel <- function(scalarKernel,scale=1) {
    if (!inherits(scalarKernel,"scalarKernel"))
        stop("scalarKernel must be of class scalarKernel")
    out <- new("matrixKernel")
    mkernel <- .Call("RMatrixValuedKernel",scalarKernel@pointer,scale);
    # out@pointer <- mkernel
   # mkernel@kerneltype <- "MatrixValuedKernel"
    return(mkernel)
}
#' create an isotropic kernel
#'
#' create an isotropic kernel
#' @param x matrix or mesh based on which the scaling is centered
#' @param scale scale factor
#' @return object of class matrixKernel
#' require(Rvcg)
#' data(humface)
#' isokernel <- IsoKernel(humface,scale=0.01)
#' @export
IsoKernel <- function(x, scale=0.01) {
    if (inherits(x,"mesh3d"))
        x <- vert2points(x)
    centroid <- colMeans(x)
    out <- new("matrixKernel")
    isokernel <- .Call("RisoKernel",scale,centroid)
    # out@pointer <- isokernel
    isokernel@kerneltype <- "IsoKernel"
    return(isokernel)
}

GetEmpiricalKernel <- function(pPCA) {
    out <- new("matrixKernel")
    if (!inherits(pPCA,"pPCA"))
        stop("pPCA must be of class pPCA")
    empkern <- .Call("RgetEmpiricalKernel",pPCA)
    out@pointer <- empkern
    out@kerneltype <- "EmpiricalKernel"
    return(out)
}
#' combine (add or multiply) two matrixKernels
#'
#' combine (add or multiply) two matrixKernels
#' @param kernel1 object of class matrixKernel
#' @param kernel2 object of class matrixKernel
#' @param add logical: if TRUE kernels will be added, multiplied otherwise
#' @return object of class matrixKernel
#' @examples
#' isoKernel <- IsoKernel(DrawMean(mod),scale=0.001)
#' kernel1 <- MatrixValuedKernel(GaussianKernel(10),1)
#' combinedKernel <- CombineKernels(kernel1,isoKernel)
#' @export
CombineKernels <- function(kernel1, kernel2, add=TRUE) {
    if (!inherits(kernel1,"matrixKernel") ||!inherits(kernel2,"matrixKernel") )
        stop("kernels must be of class matrixKernel")
    out <- new("matrixKernel")
    combined <- .Call("RcombineKernels",kernel1@pointer,kernel2@pointer,add)
    # out@pointer <- combined
    combined@kerneltype <- "CombinedKernel"
    return(combined)
}



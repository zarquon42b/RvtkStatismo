MultiscaleBsplineKernel <- function(support=100,levels=2) {
    out <- new("scalarKernel")
    kernel <- .Call("RscalarValuedKernel",1,support,levels);
    out@pointer <- kernel
    out@kerneltype="MultiscaleBsplineKernel"
    return(out)
}
GaussianKernel <- function(sigma=50) {
    out <- new("scalarKernel")
    kernel <- .Call("RscalarValuedKernel",0,sigma,0);
    out@pointer <- kernel
    out@kerneltype="GaussianKernel"
    return(out)
}
MatrixValuedKernel <- function(scalarKernel,scale=1) {
    if (!inherits(scalarKernel,"scalarKernel"))
        stop("scalarKernel must be of class scalarKernel")
    out <- new("matrixKernel")
    mkernel <- .Call("RMatrixValuedKernel",scalarKernel@pointer,scale);
    # out@pointer <- mkernel
   # mkernel@kerneltype <- "MatrixValuedKernel"
    return(mkernel)
}

IsoKernel <- function(x, scale=0.1) {
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

CombineKernels <- function(kernel1, kernel2, add=TRUE) {
    if (!inherits(kernel1,"matrixKernel") ||!inherits(kernel2,"matrixKernel") )
        stop("kernels must be of class matrixKernel")
    out <- new("matrixKernel")
    combined <- .Call("RcombineKernels",kernel1@pointer,kernel2@pointer,add)
    # out@pointer <- combined
    combined@kerneltype <- "CombinedKernel"
    return(combined)
}



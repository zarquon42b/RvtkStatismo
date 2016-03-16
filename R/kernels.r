#' create a scalar valued kernel of type MultiscaleBsplineKernel
#'
#' create a scalar valued kernel of type MultiscaleBsplineKernel
#' @param support suppor value for B-spline
#' @param levels number of levels
#' @param scale scale factor
#' @return object of class scalarKernel
#' @examples
#' kernel1 <- MultiscaleBsplineKernel(100,5)
#' @export
MultiscaleBsplineKernel <- function(support=100,levels=2,scale=10) {
    levels <- as.integer(levels)
    out <- new("BsplineKernel")
    out@support <- support
    out@levels <- levels
    out@scale <- scale
    out@kerneltype <- "BsplineKernel"
    return(out)
}

#' create a scalar valued kernel of type GaussianKernel
#'
#' create a scalar valued kernel of type GaussianKernel
#' @param sigma  bandwidth of Gausian kernel
#' @param scale scale factor
#' @return object of class scalarKernel
#' @examples
#' gkernel <- GaussianKernel(2)
#' @export
GaussianKernel <- function(sigma=50,scale=10) {
    out <- new("GaussianKernel")
    out@sigma <- sigma
    out@scale <- scale
    out@kerneltype="GaussianKernel"
    return(out)
}


#' create an isotropic kernel
#'
#' create an isotropic kernel
#' @param scale scale factor
#' @param x matrix or mesh to calculate the centroid from (overrides centroid)
#' @param centroid centroid 
#' @return object of class matrixKernel
#' require(Rvcg)
#' data(humface)
#' isokernel <- IsoKernel(humface,scale=0.01)
#' @export
IsoKernel <- function(scale=0.01,x=NULL, centroid=NULL) {
    if ( is.null(x) && is.null(centroid))
        stop("you need to specify centroid or x")
    if (!is.null(x)) {
        if (inherits(x,"mesh3d"))
            x <- vert2points(x)
        if (is.null(centroid))
            centroid <- colMeans(x)
    }
    out <- new("IsoKernel")
    out@centroid <- centroid
    if (length(centroid) != 3)
        stop("centroid must be a vector of length 3")
    out@scale <- scale
    out@kerneltype <- "IsoKernel"
    return(out)
}


#' Add two kernels
#'
#' combine (add or multiply) two matrixKernels
#' @param kernel1 object of class matrixKernel
#' @param kernel2 object of class matrixKernel
#' @return object of class combinedKernel
#' @export
SumKernels <- function(kernel1, kernel2) {
    typesallowed <- c("BsplineKernel","GaussianKernel","IsoKernel","combinedKernel")
    out <- new("combinedKernel")
    if (!class(kernel1) %in% typesallowed ||!class(kernel2)  %in% typesallowed)
        stop("unkown kernel class")
    if (kernel1@kerneltype == "ProductKernel" || kernel2@kerneltype == "ProductKernel")
        stop("first create all your summed kernels and then subsequently call ProductKernels ")
    out <- new("combinedKernel")
    if (inherits(kernel1,"combinedKernel")) {
        if (inherits(kernel2,"combinedKernel")) {
            out@kernels[[1]] <- append(kernel1@kernels[[1]],kernel2@kernels[[1]])
        } else {
            out@kernels[[1]] <- append(kernel1@kernels[[1]],kernel2)
        }
    } else if (inherits(kernel2,"combinedKernel")) {
        out@kernels[[1]] <- list(kernel2@kernels[[1]],kernel1)
    } else {
        out@kernels[[1]] <- list(kernel1,kernel2)
    }
    out@kerneltype <- "SumKernel"
    validObject(out)
    return(out)
}

#' @export
ProductKernels <- function(kernel1, kernel2) {
    typesallowed <- c("BsplineKernel","GaussianKernel","IsoKernel","combinedKernel")
    out <- new("combinedKernel")
    if (!class(kernel1) %in% typesallowed ||!class(kernel2)  %in% typesallowed)
        stop("unkown kernel class")
   
    out <- new("combinedKernel")
    if (inherits(kernel1,"combinedKernel")) {
        if (inherits(kernel2,"combinedKernel")) {
            out@kernels <- append(kernel1@kernels,kernel2@kernels)
        } else {
            out@kernels <- append(kernel1@kernels,list(list(kernel2)))
        }
    } else if (inherits(kernel2,"combinedKernel")) {
        out@kernels <- append(kernel2@kernels,(kernel1))
    } else {
        out@kernels <- list(list(kernel1),list(kernel2))
    }
    out@kerneltype <- "ProductKernel"
    validObject(out)
    return(out)
}

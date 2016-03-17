#' create a scalar valued kernel of type MultiscaleBsplineKernel
#'
#' create a scalar valued kernel of type MultiscaleBSplineKernel
#' @param support suppor value for B-spline
#' @param levels number of levels
#' @param scale scale factor
#' @return object of class scalarKernel
#' @examples
#' kernel1 <- MultiscaleBSplineKernel(100,5)
#' @export
MultiscaleBSplineKernel <- function(support=100,scale=10, levels=2) {
    if (levels < 1)
        stop("levels must be an integer > 0")
    levels <- as.integer(levels)
    out <- new("MultiscaleBSplineKernel")
    out@support <- support
    out@levels <- levels
    out@scale <- scale
    out@kerneltype <- "MultiscaleBSplineKernel"
    return(out)
}

#' create a scalar valued kernel of type BSplineKernel
#'
#' create a scalar valued kernel of type BSplineKernel
#' @param support support value for B-spline
#' @param scale scale factor
#' @return object of class scalarKernel
#' @examples
#' kernel1 <- BSplineKernel(100,50)
#' @export
BSplineKernel <- function(support=100,scale=10) {
    out <- new("BSplineKernel")
    out@support <- support
    out@scale <- scale
    out@kerneltype <- "BSplineKernel"
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

#' Create empirical StatisticalModelKernel
#'
#' Create empirical StatisticalModelKernel
#' @return returns object of class StatisticalModelKernel
#' @export
StatisticalModelKernel <- function() {
    out <- new("StatisticalModelKernel")
    out@kerneltype <- "StatisticalModelKernel"
    return(out)
}

#' Add two kernels
#'
#' Add two kernels
#' @param kernel1 object of class matrixKernel
#' @param kernel2 object of class matrixKernel
#' @return object of class CombinedKernel
#' @export
SumKernels <- function(kernel1, kernel2) {
    typesallowed <- getValidKernels(TRUE)
    out <- new("CombinedKernel")
    if (!class(kernel1) %in% typesallowed ||!class(kernel2)  %in% typesallowed)
        stop("unkown kernel class")
    if (kernel1@kerneltype == "ProductKernel" || kernel2@kerneltype == "ProductKernel")
        stop("first create all your summed kernels and then subsequently call ProductKernels ")
    out <- new("CombinedKernel")
    if (inherits(kernel1,"CombinedKernel")) {
        if (inherits(kernel2,"CombinedKernel")) {
            out@kernels[[1]] <- append(kernel1@kernels[[1]],kernel2@kernels[[1]])
        } else {
            out@kernels[[1]] <- append(kernel1@kernels[[1]],kernel2)
        }
    } else if (inherits(kernel2,"CombinedKernel")) {
        out@kernels[[1]] <- append(kernel2@kernels[[1]],kernel1)
    } else {
        out@kernels[[1]] <- list(kernel1,kernel2)
    }
    out@kerneltype <- "SumKernel"
    validObject(out)
    return(out)
}

#' Multiply two kernels
#'
#' Multiply two kernels
#' @param kernel1 object of class matrixKernel
#' @param kernel2 object of class matrixKernel
#' @return object of class CombinedKernel
#' @export
#' @export
ProductKernels <- function(kernel1, kernel2) {
    typesallowed <- getValidKernels(TRUE)
    out <- new("CombinedKernel")
    if (!class(kernel1) %in% typesallowed ||!class(kernel2)  %in% typesallowed)
        stop("unkown kernel class")
   
    out <- new("CombinedKernel")
    if (inherits(kernel1,"CombinedKernel")) {
        if (inherits(kernel2,"CombinedKernel")) {
            out@kernels <- append(kernel1@kernels,kernel2@kernels)
        } else {
            out@kernels <- append(kernel1@kernels,list(list(kernel2)))
        }
    } else if (inherits(kernel2,"CombinedKernel")) {
        out@kernels <- append(kernel2@kernels,(kernel1))
    } else {
        out@kernels <- list(list(kernel1),list(kernel2))
    }
    out@kerneltype <- "ProductKernel"
    validObject(out)
    return(out)
}

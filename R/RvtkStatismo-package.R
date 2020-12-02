#' Integrating  statismo and R using the vtkStandardMeshRepresente
#' 
#' Integrating statismo and R using the vtkStandardMeshRepresenter.
#' Statismo shape models will be stored as objects of class "pPCA". (this is work in progress). 
#' 
#' \tabular{ll}{
#' Package: \tab RvtkStatismo\cr
#' Type: \tab Package\cr
#' Version: \tab 0.3.160323\cr
#' Date: \tab 2016-03-23\cr
#' License: \tab GPL\cr
#' LazyLoad: \tab yes\cr }
#' 
#' @name RvtkStatismo-package
#' @aliases RvtkStatismo-package RvtkStatismo
#' @docType package
#' @author Stefan Schlager
#' 
#' Maintainer: Stefan Schlager <zarquon42@@gmail.com>
#' @references To be announced
#' @keywords package
#' @importFrom Rcpp evalCpp
#' @importFrom stats pchisq pnorm prcomp qchisq
#' @importFrom utils write.table
#' @useDynLib RvtkStatismo , .registration=TRUE

NULL

#' document deprecated functions
#'
#' @title deprecated functions of Morpho
#' @name deprecated
#' @rdname RvtkStatismo-deprecated
#' @keywords internal
NULL

#' @rdname RvtkStatismo-deprecated
#' @export
ComputeCoefficientsForDataset <- function(...) {
    .Deprecated("ComputeCoefficients",package="RvtkStatismo")
    ComputeCoefficients(...)
}

#' @rdname RvtkStatismo-deprecated
#' @export
ComputeLogProbabilityOfDataset <- function(...) {
    .Deprecated("ComputeLogProbability",package="RvtkStatismo")
    ComputeLogProbability(...)
}

#' @rdname RvtkStatismo-deprecated
#' @export
ComputeMahalanobisDistanceForDataset <- function(...) {
    .Deprecated("ComputeMahalanobisDistance",package="RvtkStatismo")
    ComputeMahalanobisDistance(...)
}

#' @rdname RvtkStatismo-deprecated
#' @export
read.fcsv <- function(...) {
    .Deprecated("read.fcsv",package="RvtkStatismo")
    Morpho::read.fcsv(...)
}

#' @rdname RvtkStatismo-deprecated
#' @export
write.fcsv <- function(...) {
    .Deprecated("write.fcsv",package="RvtkStatismo")
    Morpho::write.fcsv(...)
}

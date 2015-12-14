#' Integrating  statismo and R using the vtkStandardMeshRepresente
#' 
#' Integrating statismo and R using the vtkStandardMeshRepresenter.
#' Statismo shape models will be stored as objects of class "pPCA". (this is work in progress). 
#' 
#' \tabular{ll}{
#' Package: \tab RvtkStatismo\cr
#' Type: \tab Package\cr
#' Version: \tab 0.2.150716\cr
#' Date: \tab 2015-07-16\cr
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
#' @useDynLib libRvtkStatismo
NULL


#' document deprecated functions
#'
#' @title deprecated functions of RvtkStatismo
#' @name deprecated
#' @rdname RvtkStatismo-deprecated
#' @keywords internal
NULL


#' @rdname RvtkStatismo-deprecated
#' @export 
ComputeProbabilityOfDataset <- function (...)
{
  .Deprecated("ComputeProbabilityOfDataset", package="RvtkStatismo")
  ComputeProbability(...)
}
#' @rdname RvtkStatismo-deprecated
#' @export 
ComputeLogProbabilityOfDataset <- function (...)
{
  .Deprecated("ComputeLogProbabilityOfDataset", package="RvtkStatismo")
  ComputeLogProbability(...)
}
#' @rdname RvtkStatismo-deprecated
#' @export 
ComputeCoefficientsForDataset <- function (...)
{
  .Deprecated("ComputeCoefficientsForDataset", package="RvtkStatismo")
  ComputeCoefficients(...)
}
#' @rdname RvtkStatismo-deprecated
#' @export 
ComputeMahalanobisDistanceForDataset <- function (...)
{
  .Deprecated("ComputeMahalanobisDistanceForDataset", package="RvtkStatismo")
  ComputeMahalanobisDistance(...)
}

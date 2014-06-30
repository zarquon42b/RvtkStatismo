#' Implementation/Emulation of the statismo StatisticalModel class.
#'
#' Implementation/Emulation of the statismo StatisticalModel class.
#' @param model object of class \code{\link{pPCA}}
#' @param dataset an (already aligned) mesh or k x 3 matrix containing the datasets coordinates.
#' @param coefficients specify coefficients in the latent space to draw a sample
#' @param addNoise logical: if TRUE noise as specified in the model will be added to the returned sample
#' @return functions return matrices, (log)-probabilties, coefficients or sample (mesh3d or matrix) for specific dataset
#' @details see \url{http://statismo.github.io/statismo/classdoc/html/classstatismo_1_1StatisticalModel.html} for details.
#' @keywords StatisticalModel<representer>
#' @name StatismoModelMembers
#' @rdname statismoMembers
#' @export
setGeneric("GetPCABasisMatrix", function(model) {
  standardGeneric("GetPCABasisMatrix")
})

#' @rdname statismoMembers
#' @docType methods
#' @export
setGeneric("GetOrthonormalPCABasisMatrix", function(model) {
  standardGeneric("GetOrthonormalPCABasisMatrix")
})

#' @rdname statismoMembers
#' @export
setGeneric("GetNoiseVariance", function(model) {
    standardGeneric("GetNoiseVariance")
})

#' @rdname statismoMembers
#' @export
setGeneric("GetMeanVector", function(model) {
    standardGeneric("GetMeanVector")
})

#' @rdname statismoMembers
#' @export
setGeneric("GetPCAVarianceVector", function(model) {
    standardGeneric("GetPCAVarianceVector")
})

#' @rdname statismoMembers
#' @export
setGeneric("ComputeLogProbabilityOfDataset", function(model,dataset) {
    standardGeneric("ComputeLogProbabilityOfDataset")
})

#' @rdname statismoMembers
#' @export
setGeneric("ComputeProbabilityOfDataset", function(model,dataset) {
    standardGeneric("ComputeProbabilityOfDataset")
})

setGeneric("GetPCABasisMatrixIn", function(model) {
    standardGeneric("GetPCABasisMatrixIn")
})

#' @rdname statismoMembers
#' @export
setGeneric("DrawMean", function(model) {
    standardGeneric("DrawMean")
})

#' @rdname statismoMembers
#' @export
setGeneric("DrawSample", function(model,coefficients=NULL, addNoise=FALSE) {
    standardGeneric("DrawSample")
})

#' @rdname statismoMembers
#' @export
setGeneric("ComputeCoefficientsForDataset", function(model,dataset) {
    standardGeneric("ComputeCoefficientsForDataset")
})

#' @rdname statismoMembers
#' @export
setGeneric("GetDomainPoints",function(model) standardGeneric("GetDomainPoints"))

#' @rdname statismoMembers
#' @export
setGeneric("GetDomainSize",function(model) standardGeneric("GetDomainSize"))

#' @rdname statismoMembers
#' @export
setGeneric("GetCovarianceAtPoint",function(model,pt1,pt2) standardGeneric("GetCovarianceAtPoint"))

#' @rdname statismoMembers
#' @export
setGeneric("GetCovarianceMatrix",function(model) standardGeneric("GetCovarianceMatrix"))

#' @rdname statismoMembers
#' @export
setGeneric("GetJacobian",function(model,pt) standardGeneric("GetJacobian"))

#' @rdname statismoMembers
#' @export
setGeneric("ComputeCoefficientsForPointValues",function(model,sample,ids) standardGeneric("ComputeCoefficientsForPointValues"))

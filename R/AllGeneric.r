#' Implementation/Emulation of the statismo StatisticalModel class.
#'
#' Implementation/Emulation of the statismo StatisticalModel class.
#' @param model object of class \code{\link{pPCA}}
#' @param dataset an (already aligned) mesh or k x 3 matrix containing the datasets coordinates.
#' @param coefficients specify coefficients in the latent space to draw a sample
#' @param addNoise logical: if TRUE noise as specified in the model will be added to the returned sample
#' @param ptNoise specify the noise estimated in the points.
#' @return
#' \item{DrawMean}{Get the mean (either a matrix or a mesh3d)}
#' \item{DrawSample}{Draw a sample from the model (either a matrix or a mesh3d)}
#' \item{DrawMeanAtPoint}{Get a specific point of the  mean (numeric vector)}
#' \item{DrawSampleAtPoint}{Draw a sample of a specific point from the model (numeric vector)  }
#' \item{ComputeCoefficientsForDataset}{Computes the coefficients of the latent variables}
#' \item{ComputeCoefficientsForPointValues}{Returns the coefficients of the latent variables for the given values provided in two k x 3 matrices or two vectors of length 3, or one matrix/vector and a vector containing the indices on the domain  corresponding to these points}
#' \item{GetDomainPoints}{a matrix containing the points of the model's domain}

#' \item{GetDomainSize}{get the size of the model's domain}
#' \item{EvaluateSampleAtPoint}{Returns the value of the given sample at the point specified (either as point on the domain or as an index)}
#' @details see \url{http://statismo.github.io/statismo/classdoc/html/classstatismo_1_1StatisticalModel.html} for details.
#' @keywords StatisticalModel<representer>
#' @name StatismoModelMembers
#' @rdname statismoMembers
#' @rdname statismoMembers
#' @export
setGeneric("DrawMean", function(model) {
    standardGeneric("DrawMean")
})

#' @rdname statismoMembers
#' @export
setGeneric("DrawMeanAtPoint", function(model,pt) {
    standardGeneric("DrawMeanAtPoint")
})

#' @rdname statismoMembers
#' @export
setGeneric("DrawSample", function(model,coefficients=NULL, addNoise=FALSE) {
    standardGeneric("DrawSample")
})

#' @rdname statismoMembers
#' @export
setGeneric("DrawSampleAtPoint", function(model,coefficients,pt,addNoise=FALSE) {
    standardGeneric("DrawSampleAtPoint")
})

#' @rdname statismoMembers
#' @export
setGeneric("ComputeCoefficientsForDataset", function(model,dataset) {
    standardGeneric("ComputeCoefficientsForDataset")
})

#' @rdname statismoMembers
#' @export
setGeneric("ComputeCoefficientsForPointValues",function(model,sample,pt,ptNoise=0) standardGeneric("ComputeCoefficientsForPointValues"))

#' @rdname statismoMembers
#' @export
setGeneric("GetDomainPoints",function(model) standardGeneric("GetDomainPoints"))

#' @rdname statismoMembers
#' @export
setGeneric("GetDomainSize",function(model) standardGeneric("GetDomainSize"))


#' @rdname statismoMembers
#' @export
setGeneric("EvaluateSampleAtPoint",function(model,sample,pt) standardGeneric("EvaluateSampleAtPoint"))


#### MODEL MATRICES #################

#' Get Matrices from StatisticalModel class
#'
#' Get Matrices from StatisticalModel class - such as projection matrices, covariance matrices or Jacobian
#'
#' @param model model of class "pPCA"
#' @param pt either an integer pointing to the index of the domain or a numeric vector of length 3 specifying a point on the domain of the model
#' @param pt1 either an integer pointing to the index of the domain or a numeric vector of length 3 specifying a point on the domain of the model
#' @param pt2 either an integer pointing to the index of the domain or a numeric vector of length 3 specifying a point on the domain of the model
#' @return
#' 
#' \item{GetPCABasisMatrix}{returns the (scaled) Basis of the latent space}
#' \item{GetOrthonormalPCABasisMatrix}{ returns the orthonormal Basis of the latent space}
#'  \item{GetCovarianceAtPoint}{returns the 3 x 3 covariance matrix for \code{pt1} and \code{pt2}}
#' \item{GetJacobian}{ returns the 3 x 3 Jacobian matrix at \code{pt}}
#' \item{GetProjectionMatrix}{ returns matrix to project a sample vector into the latent space (this is not a member function but might prove useful anyway)}
#' 
#' @name StatismoMatrices
#' @rdname StatismoMatrices
#' @export
setGeneric("GetPCABasisMatrix", function(model) {
  standardGeneric("GetPCABasisMatrix")
})

#' @rdname StatismoMatrices
#' @export
setGeneric("GetOrthonormalPCABasisMatrix", function(model) {
  standardGeneric("GetOrthonormalPCABasisMatrix")
})

#' @rdname statismoMembers
#' @export
setGeneric("GetCovarianceAtPoint",function(model,pt1,pt2) standardGeneric("GetCovarianceAtPoint"))

#' @rdname StatismoMatrices
#' @export
setGeneric("GetCovarianceMatrix",function(model) standardGeneric("GetCovarianceMatrix"))

#' @rdname StatismoMatrices
#' @export
setGeneric("GetJacobian",function(model,pt) standardGeneric("GetJacobian"))

#' @rdname StatismoMatrices
#' @export
setGeneric("GetProjectionMatrix", function(model) {
    standardGeneric("GetProjectionMatrix")
})

##### end section Matrices


#### Model params ##################

#' Get model parameters
#'
#' Get model parameters such as variance or noise variance
#' @rdname statismoParameters
#' @param model model of class "pPCA"
#'
#' @return
#'
#' \item{GetNoiseVariance}{returns the estimated noise in the model}
#' \item{GetPCAVarianceVector}{returns the variance in the model}
#' \item{GetMeanVector}{returns the model's mean vector}
#' 
#' @name StatismoParameters
#' @rdname statismoParameters
#' @export
setGeneric("GetNoiseVariance", function(model) {
    standardGeneric("GetNoiseVariance")
})

#' @rdname statismoParameters
#' @export
setGeneric("GetMeanVector", function(model) {
    standardGeneric("GetMeanVector")
})

#' @rdname statismoParameters
#' @export
setGeneric("GetPCAVarianceVector", function(model) {
    standardGeneric("GetPCAVarianceVector")
})


### SAMPLE INFO
#' Retrieve information about a sample from the model
#'
#' @param model model of class "pPCA"
#' @param dataset a matrix or mesh3d aligned to the model's mean
#' @name StatismoSample
#' @rdname StatismoSample
#' @return
#' \item{ComputeLogProbabilityOfDataset}{returns the log-probability density at that point}
#' \item{ComputeProbabilityOfDataset}{returns the probability density at that point}
#' @export
setGeneric("ComputeLogProbabilityOfDataset", function(model,dataset) {
    standardGeneric("ComputeLogProbabilityOfDataset")
})

#' @rdname StatismoSample
#' @export
setGeneric("ComputeProbabilityOfDataset", function(model,dataset) {
    standardGeneric("ComputeProbabilityOfDataset")
})


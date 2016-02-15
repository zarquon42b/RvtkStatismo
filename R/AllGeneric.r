#' Implementation/Emulation of the statismo StatisticalModel class.
#'
#' Implementation/Emulation of the statismo StatisticalModel class.
#' @param model object of class \code{\link{pPCA}}
#' @param dataset an (already aligned) mesh or k x 3 matrix containing the datasets coordinates.
#' @param coefficients specify coefficients in the latent space to draw a sample
#' @param addNoise logical: if TRUE noise as specified in the model will be added to the returned sample
#' @param ptNoise specify the noise estimated in the points.
#' @param sample depending on the function a matrix, a numeric vector or a mesh3d (see methods below)
#' @param pt either an integer pointing to a coordinate or a 3D-vector containing the coordinates of the domain point of interest. For \code{ComputeCoefficientsForPointValues}, this can also specify a matrix of coordinates on the domain.
#' @param scaled logical: if TRUE, the scores are scaled by their standard deviation.
#' @return
#' \item{DrawMean}{Get the mean (either a matrix or a mesh3d)}
#' \item{GetMeanVector}{Get the mean vector}
#' \item{DrawMeanAtPoint}{Get a specific point of the  mean (numeric vector)}
#' \item{DrawSample}{Draw a sample from the model (either a matrix or a mesh3d)}
#' \item{DrawMeanAtPoint}{Get a specific point of the  mean (numeric vector)}
#' \item{DrawSampleAtPoint}{Draw a sample of a specific point from the model (numeric vector)  }
#' \item{ComputeCoefficientsForDataset}{Computes the coefficients of the latent variables}
#' \item{ComputeCoefficientsForPointValues}{Returns the coefficients of the latent variables for the given values provided in two k x 3 matrices or two vectors of length 3, or one matrix/vector and a vector containing the indices on the domain  corresponding to these points}
#'  \item{ComputeCoefficientsForPointValuesWithCovariance}{Returns the coefficients of the latent variables for the given values provided in two k x 3 matrices or two vectors of length 3, or one matrix/vector and a vector containing the indices on the domain  corresponding to these points. In contrast to \code{ComputeCoefficientsForPointValues}, \code{ptNoise} can be set individually, either as vector of length \code{k} (assuming spherical noise) or a \eqn{(k*3) \times 3}{(k*3) x 3} matrix with the i-th \eqn{3 \times 3}{3x3} block containing the covariance matrix for the i-th coordinate.}
#' \item{GetDomainPoints}{a matrix containing the points of the model's domain}

#' \item{GetDomainSize}{get the size of the model's domain}
#' \item{EvaluateSampleAtPoint}{Returns the value of the given sample at the point specified (either as point on the domain or as an index)}
#' \item{GetPCScores}{get model's PC-scores, scaled or unscaled to unit variance, depending on the choice of \code{scaled}}
#' @details see \url{http://statismo.github.io/docs/api/v0.10/html/classstatismo_1_1StatisticalModel.html} for details.
#' @keywords StatisticalModel<representer>
#' @name StatismoModelMembers
#' @docType methods
#' @rdname statismoMembers
NULL

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
setGeneric("DrawSampleVector", function(model,coefficients, addNoise=FALSE) {
    standardGeneric("DrawSampleVector")
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
# # @rdname statismoMembers
# # @export
# setGeneric("RobustlyComputeCoefficientsForDataset", function(model,dataset,niterations=100, nu = 6, sigma2=1) {
#    standardGeneric("RobustlyComputeCoefficientsForDataset")
#})

#' @rdname statismoMembers
#' @export
setGeneric("ComputeCoefficientsForPointValues",function(model,sample,pt,ptNoise=0) standardGeneric("ComputeCoefficientsForPointValues"))
#' @rdname statismoMembers
#' @export
setGeneric("ComputeCoefficientsForPointValuesWithCovariance",function(model,sample,pt,ptNoise=0) standardGeneric("ComputeCoefficientsForPointValuesWithCovariance"))

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
#' \item{GetCovarianceMatrix}{returns the covariance matrix - can be huge!!!}
#'  \item{GetCovarianceAtPoint}{returns the 3 x 3 covariance matrix for \code{pt1} and \code{pt2}}
#' \item{GetJacobian}{ returns the 3 x 3 Jacobian matrix at \code{pt}}
#' \item{GetProjectionMatrix}{ returns matrix to project a sample vector into the latent space (this is not a member function but might prove useful anyway)}
#'
#' @details see \url{http://statismo.github.io/docs/api/v0.10/html/classstatismo_1_1StatisticalModel.html} for details.
#' @name StatismoMatrices
#' @rdname StatismoMatrices
NULL

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

#' @rdname StatismoMatrices
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
#' @details see \url{http://statismo.github.io/docs/api/v0.10/html/classstatismo_1_1StatisticalModel.html} for details.
#' @docType methods
#' @name StatismoParameters
#' @rdname statismoParameters
NULL

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
#' @return
#' \item{ComputeLogProbabilityOfDataset}{returns the log-probability density for the sample}
#' \item{ComputeProbabilityOfDataset}{returns the probability density for the sample}
#' @seealso \code{\link{getDataLikelihood}}
#' @docType methods
#' @name StatismoSample
#' @rdname StatismoSample
NULL

#' @rdname StatismoSample
#' @export
setGeneric("ComputeLogProbabilityOfDataset", function(model,dataset) {
    standardGeneric("ComputeLogProbabilityOfDataset")
})

#' @rdname StatismoSample
#' @export
setGeneric("ComputeProbabilityOfDataset", function(model,dataset) {
    standardGeneric("ComputeProbabilityOfDataset")
})

#' @rdname StatismoSample
#' @export
setGeneric("ComputeMahalanobisDistanceForDataset", function(model,dataset) {
    standardGeneric("ComputeMahalanobisDistanceForDataset")
})


### custom stuff

#' @rdname statismoMembers
#' @export
setGeneric("GetModelInfo", function(model) {
    standardGeneric("GetModelInfo")
})

#' @rdname statismoMembers
#' @export
setGeneric("GetPCScores", function(model,scaled=TRUE) {
    standardGeneric("GetPCScores")
})

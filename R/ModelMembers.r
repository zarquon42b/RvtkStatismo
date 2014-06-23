#' Implementation/Emulation of the statsimo StatisticalModel class.
#'
#' Implementation/Emulation of the statsimo StatisticalModel class.
#' @param model object of class "pPCA"
#' @param dataset an (already aligned) mesh or k x 3 matrix containing the datasets coordinates.
#' @return functions return matrices, (log)-probabilties or coefficients for specific dataset
#' @details see \url{http://statismo.github.io/statismo/classdoc/html/classstatismo_1_1StatisticalModel.html} for details.
#' @name StatismoModelMembers
#' @rdname statismoMembers
#' @export
GetPCABasisMatrix <- function(model) {
    
    W <- t(t(model$PCA$rotation)*model$PCA$sdev) ##Matrix to project scaled PC-scores back into the config space
    return(W)
}
#' @rdname statismoMembers
#' @export
GetOrthonormalPCABasisMatrix <- function(model) {
    return(model$PCA$rotation)
}
#' @rdname statismoMembers
#' @export
GetNoiseVariance <- function(model) {
    return(model$sigma)
}
#' @rdname statismoMembers
#' @export
GetMeanVector <- function(model) {
    return(model$PCA$center)
}
#' @rdname statismoMembers
#' @export
GetPCAVarianceVector <- function(model) {
    return(model$PCA$sdev^2)
}
#' @export
ComputeLogProbabilityOfDataset <- function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),TRUE)
    return(out)
}
#' @rdname statismoMembers
#' @export
ComputeProbabilityOfDataset <- function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),FALSE)
    return(out)
}
GetPCABasisMatrixIn <- function(model) {
    ##this the more complicated version directly from StatisticalModel.txx
    WT <- t(GetPCABasisMatrix(model))
    Mmatrix <- crossprod(GetPCABasisMatrix(model))
    diag(Mmatrix) <- diag(Mmatrix)+model$sigma
    Mmatrixinv <- solve(Mmatrix)
    Win <- Mmatrixinv%*%WT
    #Win <- (t(model$PCA$rotation)*(1/sqrt(model$PCA$sdev^2+model$sigma))) ##Matrix to project scaled PC-scores back into the config space
    return(Win)
}
#' @rdname statismoMembers
#' @export
DrawMean <- function(model) {
    if (!inherits(model,"pPCA"))
        stop("please provide model of class 'pPCA'")
    out <- (.Call("DrawMean",model))
    if (inherits(out,"mesh3d"))
        out$vb <- rbind(out$vb,1)
    else
        out <- t(out$vb)
    return(out)
}
#' @rdname statismoMembers
#' @export
ComputeCoefficientsForDataset <- function(model,dataset) {
    out <- .Call("ComputeCoefficientsForDataset",model,dataset2representer(dataset))
    return(out)
}

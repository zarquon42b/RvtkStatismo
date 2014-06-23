#' @export
GetPCABasisMatrix <- function(model) {
    
    W <- t(t(model$PCA$rotation)*model$PCA$sdev) ##Matrix to project scaled PC-scores back into the config space
    return(W)
}
#' @export
GetOrthonormalPCABasisMatrix <- function(model) {
    return(model$PCA$rotation)
}
#' @export
GetNoiseVariance <- function(model) {
    return(model$sigma)
}
#' @export
GetMeanVector <- function(model) {
    return(model$PCA$center)
}
#' @export
GetPCAVarianceVector <- function(model) {
    return(model$PCA$sdev^2)
}
#' @export
ComputeLogProbabilityOfDataset <- function(model,dataset) {
    out <- .Call("ComputeLogProbabilityOfDataset",model,dataset2representer(dataset),TRUE)
    return(out)
}
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

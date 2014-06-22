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
    
    Win <- (t(model$PCA$rotation)*(1/(model$PCA$sdev+model$sigma))) ##Matrix to project scaled PC-scores back into the config space
    return(Win)
}

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

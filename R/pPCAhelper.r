#' Fast Procrustes align of coordinates
#' 
#' Fast Procrustes align of coordinates
#' @param array array of coordinates
#' @param scale logical: request scaling during alignment
#' @param missingIndex integer vector: specifies which points are missing (for building constrained model)
#' @param deselect logical: if TRUE, missingIndex references the existing coordinates instead of the missing ones.
#' @return a list containing
#' \item{rotated}{array containing registered coordinates}
#' \item{mshape}{matrix containing meanshape}
#'
#' @export
rigidAlign <- function(array,scale=TRUE,missingIndex,deselect=FALSE) {
    k <- dim(array)[1]
    use.lm <- NULL
    if (!missing(missingIndex)) {
        if (deselect) {
            missingIndex <- c(1:k)[-missingIndex]
        }
        use.lm=c(1:k)[-missingIndex]
    }
    out <- partialAlign(array,use.lm = use.lm,scale=scale)
}

partialAlign <- function(array,use.lm=NULL,scale=TRUE) {
    if (!is.null(use.lm)){
        procMod <- ProcGPA(array[use.lm,,],scale=scale,CSinit=F,reflection=F,silent = TRUE)##register all data using Procrustes fitting based on the non-missing coordinates
            tmp <- array
            a.list <-  1:(dim(array)[3])
            tmp <- lapply(a.list, function(i) {mat <- rotonmat(array[,,i],array[use.lm,,i],procMod$rotated[,,i],scale=scale,reflection = F);return(mat)})
            tmp1 <- array
            for (i in 1:length(a.list))
                tmp1[,,i] <- tmp[[i]]
            procMod$rotated <- tmp1
            procMod$mshape <- arrMean3(tmp1)
        } else {
            procMod <- ProcGPA(array,scale=scale,CSinit = F,reflection = F,silent = T)
        }
    return(procMod)
}
## get PCrows associated with missingValues
getSel <- function(missingIndex,shape) {
    m <- ncol(shape)
    sel <- missingIndex*3
    sel <- sort(c(sel, sel-1, sel-2))
    return(sel)
}
    
getUseLM <- function(missingIndex,shape) {
     k <- nrow(shape)
     use.lm=c(1:k)[-missingIndex]
     return(use.lm)
 }


# create a neat variance table
createVarTable <- function(sdev,square=TRUE) {
    if (square)
        sdev <- sdev^2
    sdsum <- sum(sdev)
    sdVar <- sdev/sdsum
    sdCum <- cumsum(sdVar)
    Variance <- data.frame(eigenvalue=sdev,exVar=sdVar, cumVar=sdCum)
    return(Variance)
}
# convert a mesh/matrix to a valid representer
dataset2representer <- function(x) {
    if (is.matrix(x))
        out <- list(vb=t(x),it=matrix(0,0,0))
    else if (inherits(x,"mesh3d"))
        out <- meshintegrity(x)
    else
        stop("unknown representer type")
    return(out)
}
        
# get matrix of mean shape    
getMeanMatrix <- function(model,transpose=TRUE) {
    nvb <- ncol(model$representer$vb)
    
    x <- matrix(model$PCA$center,3,nvb)
    if (transpose)
        x <- t(x)
    
    return(x)
}
    
## get the original standard deviations from a model given model the damped values and the estimated noiseVariance
calcSdev <- function(model) {
    sdevorig <- sqrt(model$PCA$sdev^2+model$sigma)
    return(sdevorig)
}

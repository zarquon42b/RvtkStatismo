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


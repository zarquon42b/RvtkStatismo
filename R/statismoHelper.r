### convert arrays into a list to be converted to vtk-PolyData
array2meshlist <- function(x) {
    n <- dim(x)[3]
    out <- lapply(1:n,function(i) {out <- list(vb=t(x[,,i]),it= matrix(0,0,0))})
    return(out)
}

#' convert meshes to array consisting of vertex coordinates
#'
#' convert meshes to array consisting of vertex coordinates
#' @param meshlist list containing triangular meshes of class "mesh3d"
#' @return returns an array with k x 3 x n dimensions where k=number of vertices, and n=sample size.
#' @importFrom Morpho vert2points
#' @export
meshlist2array <- function(meshlist) {
    n <- length(meshlist)
    k <- ncol(meshlist[[1]]$vb)
    vertarr <- array(NA,dim=c(k,3,n))
    for (i in 1:n)
        vertarr[,,i] <- vert2points(meshlist[[i]])
    dimnames(vertarr)[[3]] <- names(meshlist)
    if (is.null(names(meshlist)))
        dimnames(vertarr)[[3]] <- paste("specimen",1:n,sep="_")
    return(vertarr)
}

## check if the list of meshes is valid to send to statismo
checkmeshlist <- function(x) {
    verts <- unlist(lapply(x,function(y) y <- ncol(y$vb)))
    chk <- prod(verts==verts[1])
    if (!chk)
        stop("all meshes need to have the same amount of vertices")
    else
        return(lapply(x,meshintegrity))
}
##converts the returned model from statismo to class pPCA   
statismo2pPCA <- function(statismodel) {
    
    PCA <- list()
    PCA$sdev <- sqrt(statismodel$PCVariance)
    PCA$rotation <- statismodel$PCBasisOrtho
    PCA$center <- statismodel$mshape
    PCA$x <- t(statismodel$scores)
   
    #ut1$scale <- statismodel$scale
        representer <- statismodel$representer
    if (inherits(representer,"mesh3d"))
        representer$vb <- rbind(representer$vb,1)
    else
        representer$it <- matrix(0,0,0)
    
    out1 <- new("pPCA",PCA=PCA,representer=representer,sigma=statismodel$sigma,Variance=createVarTable(PCA$sdev))
    return(out1)
}

array2meshlist <- function(x) {
    n <- dim(x)[3]
    out <- lapply(1:n,function(i) {out <- list(vb=t(x[,,i]),it= matrix(0,0,0))})
    return(out)
}

#' align meshes stored in a list by their vertices
#' 
#' align meshes stored in a list by their vertices
#' @param meshlist list containing triangular meshes of class "mesh3d"
#' @param scale logical: request scaling during alignment
#' @importFrom Morpho vert2points ProcGPA
#' @export
meshalign <- function(meshlist,scale=TRUE,tol=1e-5) {
    vertarr <- meshlist2array(meshlist)
    out <- ProcGPA(vertarr,scale=scale, CSinit = FALSE,silent = F,tol=tol)$rotated
    return(out)
}

#' convert meshes to array consisting of vertexc coordinates
#'
#' convert meshes to array consisting of vertexc coordinates
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
#' @importFrom Rvcg meshintegrity
checkmeshlist <- function(x) {
    verts <- unlist(lapply(x,function(y) y <- ncol(y$vb)))
    chk <- prod(verts==verts[1])
    if (!chk)
        stop("all meshes need to have the same amount of vertices")
    else
        return(lapply(x,meshintegrity))
}
    

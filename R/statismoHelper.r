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
#' @param array logical: if TRUE the superimposed vertices will be returned as 3D array.
#' @return returns a list of aligned meshes or an array of dimensions k x 3 x n, where k=number of vertices and n=sample size.
#' @importFrom Morpho vert2points ProcGPA
#' @export
meshalign <- function(meshlist,scale=TRUE,array=FALSE) {
    vertarr <- meshlist2array(meshlist)
    out <- rigidAlign(vertarr,scale=scale)$rotated
    if (array) {
        return(out)
    } else {
        out <- lapply(1:length(meshlist),function(i){ res <- meshlist[[i]]
                                                      res$vb[1:3,] <- t(out[,,i])
                                                      res$normals <- NULL
                                                      return(res)})
        names(out) <- names(meshlist)
        return(out)
    }
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

checkmeshlist <- function(x) {
    verts <- unlist(lapply(x,function(y) y <- ncol(y$vb)))
    chk <- prod(verts==verts[1])
    if (!chk)
        stop("all meshes need to have the same amount of vertices")
    else
        return(lapply(x,meshintegrity))
}
    

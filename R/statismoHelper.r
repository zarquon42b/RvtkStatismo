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
    n <- length(meshlist)
    k <- ncol(meshlist[[1]]$vb)
    vertarr <- array(NA,dim=c(k,3,n))
    for (i in 1:n)
        vertarr[,,i] <- vert2points(meshlist[[i]])
    dimnames(vertarr)[[3]] <- names(meshlist)
    out <- ProcGPA(vertarr,scale=scale, CSinit = FALSE,silent = F,tol=tol)$rotated
    return(out)
}

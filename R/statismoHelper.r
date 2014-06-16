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
#' @importFrom Morpho vert2points bindArr ProcGPA
#' @export
meshalign <- function(meshlist,scale=TRUE) {
    vertlist <- lapply(1:length(meshlist),function(x) vert2points(meshlist[[x]]))
    vertlist <- bindArr(vertlist,along=3)
    dimnames(vertlist)[[3]] <- names(meshlist)
    out <- ProcGPA(vertlist,scale=scale, CSinit = FALSE,silent = TRUE)$rotated
    return(out)
}

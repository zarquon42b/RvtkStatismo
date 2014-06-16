array2meshlist <- function(x) {
    n <- dim(x)[3]
    out <- lapply(1:n,function(i) {out <- list(vb=t(x[,,i]),it= matrix(0,0,0))})
    return(out)
}
#' generate a statistical model using an array of superimposed landmarks and save it
#'
#' generate a statistical model using an array of superimposed landmarks and save it
#'
#' @param array array of aligned 3D-coordinates (e.g. the vertices of meshes)
#' @param representer matrix or triangular mesh of class "mesh3d" with vertices corresponding to rows in the array.
#' @param sigma noise in the data
#' @export

statismoBuildModel <- function(array,representer,sigma=0) {
    mylist <- array2meshlist(array)
    names(mylist) <- dimnames(array)[[3]]
    if (is.matrix(representer))
        representer <- list(vb=t(representer),it=matrix(0,0,0))
    else if (is.list(representer)) {
        if (!is.numeric(representer$vb) || !is.integer(representer$it))
            stop("representer needs vertices and faces")
        else
            representer$it <- representer$it-1
    }
    
    out <- .Call("BuildModel",mylist,representer,sigma)
}


#' align meshes stored in a list by their vertices
#' 
#' align meshes stored in a list by their vertices
#' @param meshlist list containing triangular meshes of class "mesh3d"
#' @param scale logical: request scaling during alignment
#' @export
meshalign <- function(meshlist,scale=TRUE) {
    vertlist <- lapply(1:length(meshlist),function(x) vert2points(meshlist[[x]]))
    vertlist <- bindArr(vertlist,along=3)
    dimnames(vertlist)[[3]] <- names(meshlist)
    out <- ProcGPA(vertlist,scale=scale, CSinit = FALSE,silent = TRUE)$rotated
    return(out)
}

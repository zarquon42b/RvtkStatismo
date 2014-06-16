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


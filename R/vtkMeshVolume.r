#' @export
vtkMeshInfo <- function(mesh) {
    out <- .Call("vtkMeshInfo",mesh)
    return(out)
}

#' @export
vtkFillHoles <- function(mesh,holesize=1e6) {
    out <- .Call("vtkFillHole",mesh,holesize)
    out$vb <- rbind(out$vb,1)
    return(out)
}

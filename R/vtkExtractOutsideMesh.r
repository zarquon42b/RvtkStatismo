#' @importFrom Morpho meshcube
vtkExtractOutsideMesh <- function(x) {
    bbox <- as.matrix(meshcube(x))
    out <- .Call("vtkExtractOutsideMesh",x,bbox)
    out$vb <- rbind(out$vb,1)
    return(out)
}
vtkDelaunay3D <- function(x) {
    
    out <- .Call("vtkSurfaceReko",x)
    out$vb <- rbind(out$vb,1)
    return(out)
}

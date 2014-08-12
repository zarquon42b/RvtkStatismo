#' get outer mesh
#'
#' get outer mesh and discard enclosed segments
#' @param x mesh
#' @return returns only those segments visible from the outside of the mesh
#' 
#' @importFrom Morpho meshcube
#' @export
vtkExtractOutsideMesh <- function(x) {
    bbox <- as.matrix(meshcube(x))
    out <- .Call("vtkExtractOutsideMesh",x,bbox)
    out$vb <- rbind(out$vb,1)
    return(out)
}

#' create surface from pointcloud
#'
#' create surface from pointcloud or mesh
#' @param x pointcloud or mesh
#' @return mesh of class mesh3d
#' @export
vtkSurfaceReko <- function(x) {
    if (is.matrix(x))
        x <- list(vb=t(x))
    out <- .Call("vtkSurfaceReko",x)
    out$vb <- rbind(out$vb,1)
    return(out)
}

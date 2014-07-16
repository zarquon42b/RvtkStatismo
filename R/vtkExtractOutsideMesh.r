#' @importFrom Morpho meshcube
vtkExtractOutsideMesh <- function(x) {
    bbox <- as.matrix(meshcube(x))
    out <- .Call("vtkExtractOutsideMesh",x,bbox)
    out$vb <- rbind(out$vb,1)
    return(out)
}

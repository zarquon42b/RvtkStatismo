vtkSimple <- function(file) {
  a <- .Call("vtkSP",as.character(file))
}

vtkSimpleMesh <- function(mesh) {
    vb <- mesh$vb[1:3,]
    it <- mesh$it-1
    storage.mode(it) <- "integer"
    
    b <- .Call("vtkSPMat",vb, it)
}

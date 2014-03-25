vtkSimple <- function(file) {
  a <- .Call("vtkSP",as.character(file))
}
#' @export
vtkSimpleMesh <- function(mesh) {
    vb <- mesh$vb[1:3,]
    it <- mesh$it-1
    storage.mode(it) <- "integer"
    
    b <- .Call("vtkSPMat",vb, it)
}
#' @export
vtkWrite <- function(mesh, filename=dataname) {
    dataname <- deparse(substitute(mesh))
    vb <- mesh$vb[1:3,]
    it <- mesh$it-1
    filename <- path.expand(as.character(filename))
    filename <- paste(filename,".vtp",sep="")
    out <- .Call("vtkWrite",filename,vb,it)
}

vtkSimple <- function(file) {
  a <- .Call("vtkSP",as.character(file))
}
#' @export
vtkRenderMesh <- function(mesh,size=5) {
    if (inherits(mesh,"mesh3d")) {
        vb <- mesh$vb[1:3,]
        if(!is.null(mesh$it))
            it <- mesh$it-1
        else
            it <- matrix(0,0,0)
        
    } else if (is.matrix(mesh)){
        vb <- t(mesh)
        it <- matrix(0,0,0)
    }
    if (!is.numeric(vb) || !is.matrix(vb))
        stop("no vertices to render")
   if (!is.numeric(it) || !is.matrix(it))
        stop("faces must be integer matrix")
    
    b <- .Call("vtkVisualize",vb, it,size)
}

#' exports a triangular mesh of class mesh3d to a vtp file
#'
#' exports a triangular mesh of class mesh3d to a vtp file
#'
#' @param mesh mesh of class mesh3d
#' @param filename character
#' @export
mesh2vtp <- function(mesh, filename=dataname) {
    dataname <- deparse(substitute(mesh))
    vb <- mesh$vb[1:3,]
    it <- mesh$it-1
    filename <- path.expand(as.character(filename))
    filename <- paste(filename,".vtp",sep="")
    out <- .Call("vtkWrite",filename,vb,it)
}
#' imports vtk and vtp files
#'
#' imports vtk and vtp files
#'
#' @param filename character string
#' @return list of class mesh3d
#' 
#' @export
read.vtk <- function(filename) {
    filename <- path.expand(as.character(filename))
    ext <- gsub("(.*)[.](vtk|vtp)$", "\\2", tolower(filename));
    legacy <- FALSE
    if (! ext %in% c("vtk", "vtp"))
        stop("unknown file format")
    else if (ext == "vtk")
        legacy <- TRUE   
    
    out <- .Call("vtkRead",filename,legacy)
    out$vb <- rbind(out$vb,1)
    out$it <- out$it+1
    class(out) <- "mesh3d"
    return(out)
}

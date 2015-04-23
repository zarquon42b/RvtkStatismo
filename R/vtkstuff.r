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
vtkMeshWrite <- function(mesh, filename=dataname,type=c("vtk","vtp")) {
    
    dataname <- deparse(substitute(mesh))
    if(inherits(mesh,"mesh3d")) {
        vb <- mesh$vb[1:3,]
        it <- mesh$it
    } else if (is.matrix(mesh)) {
          it <- NULL
          if (ncol(mesh) == 3)
              vb <- t(mesh)
          else if (ncol(mesh) == 2)
              vb <- t(cbind(mesh,0))
          else
              stop("only 2D and 3D matrices are allowed")
      }
    
    type <- type[1]
    filename <- path.expand(as.character(filename))
    if (type %in% c("vtp","vtk"))
        filename <- paste0(filename,".",type)
    else
        stop("unsupported file format")
    out <- .Call("vtkWrite",filename,vb,it,type)
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
    if (ncol(out$it) == 0)
        out$it <- NULL
    else
        out$it <- out$it+1
    class(out) <- "mesh3d"
    return(out)
}
vtkUpdateNormals <- function(mesh) {
     
        vb <- mesh$vb[1:3,]
        if(!is.null(mesh$it))
            it <- mesh$it-1
        else
            stop("mesh contains no faces")
         out <- .Call("vtkWrite",filename,vb,it)
}

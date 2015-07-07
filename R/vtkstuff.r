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
#' @param type character: file extension. Can be "vtk", "vtp".
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
#' imports vtk, vtp and wrl files containing meshes
#'
#'imports vtk, vtp and wrl files containing meshes
#'
#' @param filename character string
#' @return triangular mesh of class mesh3d
#' @note read.wrl is simply an alias for read.vtk.
#' All structures will be converted to triangular meshes.
#' @rdname vtkIO
#' @export
read.vtk <- function(filename) {
    filename <- path.expand(as.character(filename))
    ext <- gsub("(.*)[.](vtk|vtp|wrl)$", "\\2", tolower(filename));
    type <- 1
    if (! ext %in% c("vtk", "vtp","wrl"))
        stop("unknown file format")
    else if (ext == "vtk")
        type <- 0
    else
        type <- 2

    out <- .Call("vtkRead",filename,type)
    out$vb <- rbind(out$vb,1)
    if (ncol(out$it) == 0)
        out$it <- NULL
    else
        out$it <- out$it+1
    class(out) <- "mesh3d"
    return(out)
}

#' @rdname vtkIO
#' @export
read.wrl <- read.vtk

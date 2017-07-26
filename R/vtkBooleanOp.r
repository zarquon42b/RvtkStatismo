#' run boolean operations on mesh
#'
#' run boolean operations on mesh
#' @param mesh1 triangular mesh
#' @param mesh2 triangular mesh
#' @param type integer: boolean operation. 0=Union, 1=intersection, 2=difference
#' @param reorient logical: Turn on/off cell reorientation of the intersection portion of the surface when the type=2 (DIFFERENCE). Defaults to TRUE.
#' @param tol Set the tolerance used to determine when a point's absolute distance is considered to be zero. Defaults to 1e-6.
#' @return returns manipulated mesh
#' @examples
#' require(Rvcg);require(Morpho)
#' sphere <- vcgSphere()
#' myplane <- list(vb=rbind(cbind(c(1,0,0),c(0,1,0),c(0,0,1)),1),
#'                 it=as.matrix(c(1:3)));class(myplane) <- "mesh3d"
#' myplane <- scalemesh(myplane,5)
#' cutsphere <- vtkBooleanOp(sphere,myplane,type = 2)
#' @export
vtkBooleanOp <- function(mesh1 ,mesh2 ,type=0, reorient=TRUE, tol=1e-6) {

    stopifnot(is.matrix(mesh1$it), is.matrix(mesh2$it))
    if (nrow(mesh1$it) != 3 || nrow(mesh2$it) != 3 || ncol(mesh1$it) < 1 || ncol(mesh2$it) < 1)
        stop("no meshes without triangular faces allowed")
    out <- .Call("vtkBooleanOpCpp",mesh1,mesh2,type,reorient, tol)
    #out$vb <- rbind(out$vb,1)
    return(out)
}

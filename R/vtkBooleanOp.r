#' run boolean operations on mesh
#'
#' run boolean operations on mesh
#' @param mesh1 triangular mesh
#' @param mesh2 triangular mesh
#' @param type integer: boolean operation. 0=Union, 1=intersection, 2=difference
#' @return returns manipulated mesh
#' @examples
#' require(Rvcg);require(Morpho)
#' sphere <- vcgSphere()
#' myplane <- list(vb=rbind(cbind(c(1,0,0),c(0,1,0),c(0,0,1)),1),
#'                 it=as.matrix(c(1:3)));class(myplane) <- "mesh3d"
#' myplane <- scalemesh(myplane,5)
#' cutsphere <- vtkBooleanOp(sphere,myplane,type = 2)
#' @export
vtkBooleanOp <- function(mesh1 ,mesh2 ,type=0) {
    out <- .Call("vtkBooleanOp",mesh1,mesh2,type)
    out$vb <- rbind(out$vb,1)
    return(out)
}

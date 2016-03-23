#' Iteratively closest point registration
#'
#' Iteratively closest point registration
#' @param refmesh moving mesh
#' @param tarmesh fixed mesh
#' @param iterations integer: number of iterations to run
#' @param center logical: rotate meshes around center of gravity.
#' @param type type of transform to be used: "none"=rigid,"scale"=similarity transform, "affine"=affine transform.
#' @param sample number of random samples to use
#' @export
vtkICP <- function(refmesh,tarmesh,iterations=10,center=FALSE,type=c("none","scale", "affine"),sample=200) {
    type <- tolower(substr(type[1],1L,1L))
    
    out <- .Call("vtkICP",refmesh,tarmesh,iterations,center,type,sample)
    #out$vb <- rbind(out$vb,1)
    return(out)
}

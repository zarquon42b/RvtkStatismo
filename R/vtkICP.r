#' Iteratively closest point registration
#'
#' Iteratively closest point registration
#' @param refmesh moving mesh
#' @param tarmesh fixed mesh
#' @param iterations integer: number of iterations to run
#' @param center logical: rotate meshes around center of gravity.
#' @param type type of transform to be used: "none"=rigid,"scale"=similarity transform, "affine"=affine transform.
#' @param sample number of random samples to use
#' @param meandistmode how to compute distance to between two iterations. RMS=square root of the average of the sum of squares of the closest point distance. absolute=the mean of the sum of absolute values of the closest point distance.
#' @param tol convergence threshold. The algorithm stops when the mean distance is below this value.
#' @param matchCentroids logical: if TRUE, the process by is started by translating source centroid to target centroid.
#' @param getTransform logical: if TRUE a list containing the transformed mesh and a 4x4 transformation matrix will be returned
#' @export
vtkICP <- function(refmesh,tarmesh,iterations=10,center=FALSE,type=c("none","scale", "affine"),sample=200,meandistmode=c("RMS","absolute"),tol=0.01,matchCentroids=FALSE,getTransform=FALSE) {
    meandistmode <- match.arg(meandistmode[1],c("RMS","absolute"))
    mdm <- 0
    if (mdm != "RMS")
        mdm <- 1

    type <- tolower(substr(type[1],1L,1L))
    
    out <- .Call("vtkICP",refmesh,tarmesh,iterations,center,type,sample,mdm,tol,matchCentroids)
    if (!getTransform)
        out <- out$mesh
    return(out)
}

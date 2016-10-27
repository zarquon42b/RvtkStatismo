#' Compute Geodesic path between two vertices
#'
#' Compute Geodesic path between two vertices
#' @param x triangular mesh of class mesh3d
#' @param start index of vertex to start
#' @param end index of vertex to end
#' @return
#' list containing
#' \item{distance}{geodesic distance (cummulative distance between all vertices along the way)}
#' \item{index}{vector containing vertex indices from start to end}
#' @examples
#' require(Rvcg)
#' data(humface)
#' gp <- vtkGeodesicPath(humface,1,1000)
#' \dontrun{
#' require(rgl);require(Morpho)
#' points3d(vert2points(humface)[gp$index,])
#' lines3d(vert2points(humface)[gp$index,],col="blue",lwd=2)
#' spheres3d(vert2points(humface)[c(1,1000),],col=2)
#' wire3d(humface,col=3)
#' }
#' @export
vtkGeodesicPath <- function(x,start, end) {
    start <- start-1
    end <- end-1
    out <- .Call("vtkGeodesicPath",x,start,end)
    out$index <- out$index+1
    return(out)
}
#' Compute Geodesic path between two points on a mesh based on the closest vertices
#'
#' Compute Geodesic path between two points on a mesh based on the closest vertices
#'
#' @param x triangular mesh of class mesh3d
#' @param start vector of length 3 containing starting position
#' @param end vector of length 3 containing starting position
#' @importFrom Rvcg vcgKDtree
#' @return
#' list containing
#' \item{distance}{geodesic distance (cummulative distance between all vertices along the way)}
#' \item{index}{vector containing vertex indices from start to end}
#' @examples
#' require(Rvcg)
#' data(humface)
#' gp <- vtkGeodesicPathForPointPair(humface,humface.lm[1,],humface.lm[2,])
#' \dontrun{
#' require(rgl);require(Morpho)
#' points3d(vert2points(humface)[gp$index,])
#' lines3d(vert2points(humface)[gp$index,],col="blue",lwd=2)
#' spheres3d(humface.lm[1:2,],col=2)
#' wire3d(humface,col=3)
#' }
#' @export
vtkGeodesicPathForPointPair <- function(x,start, end) {
    mat <- rbind(start,end)
    inds <- vcgKDtree(x,mat,k=1)$index-1
    
    out <- .Call("vtkGeodesicPath",x,inds[1],inds[2])
    out$index <- out$index+1
    return(out)
}

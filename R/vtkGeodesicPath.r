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
#' @export
vtkGeodesicPath <- function(x,start, end) {
    start <- start-1
    end <- end-1
    out <- .Call("vtkGeodesicPath",x,start,end)
    out$index <- out$index+1
    return(out)
}

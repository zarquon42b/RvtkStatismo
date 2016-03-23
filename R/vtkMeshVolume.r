#' get Infos about a triangular mesh
#'
#' get Infos about a triangular mesh
#' @param mesh triangular mesh
#' @return
#' \item{volume}{enclosed volume}
#' \item{projVol}{enclosed projected volume}
#' \item{surfaceArea}{surface area}
#' @details see documentation of vtkMassProperties for details
#' @export
vtkMeshInfo <- function(mesh) {
    out <- .Call("vtkMeshInfo",mesh)
    return(out)
}

#' fill holes in triangular mesh
#'
#' fill holes in triangular mesh
#' @param mesh triangular mesh
#' @param holesize maximal size of holes to fill
#' @return mesh with filled holes
#' @details see documentation of vtkFillHolesFilter for details
#' @export
vtkFillHoles <- function(mesh,holesize=1e6) {
    out <- .Call("vtkFillHole",mesh,holesize)
    #out$vb <- rbind(out$vb,1)
    return(out)
}

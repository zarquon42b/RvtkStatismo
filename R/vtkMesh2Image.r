#' convert a 3D triangular mesh into a voxel image
#' 
#' convert a 3D triangular mesh into a voxel image
#' @param mesh triangular mesh of class mesh3d
#' @param spacing numeric vector of length 3: voxel spacings
#' @param filename filename to save as. Can be mha/mhd for VTK_VERSION < 6.2 and
#' additionally nii or nii.gz else
#' @param IJK2RAS 3x3 transformation matrix from point to image space
#' @param margin percentage of margin to add around the volume
#' @param col integer: value 0 <= col <= 255. Defines the foreground color.
#' @param tol The tolerance for including a voxel inside the stencil. This is in fractions of a voxel, and must be between 0 and 1. Tolerance is only applied in the x and y directions, not in z. Setting the tolerance to zero disables all tolerance checks and might result in faster performance. 
#' @examples
#' require(Rvcg)
#' data(dummyhead)
#' vtkMesh2Image(dummyhead.mesh)
#' @export
vtkMesh2Image <- function(mesh,spacing=c(0.5,0.5,0.5),filename="default.mha",IJK2RAS=diag(c(-1,-1,1)),margin=0.1,col=255,tol=0) {
    mesh$vb[1:3,] <- IJK2RAS%*%mesh$vb[1:3,]
    out <- .Call("vtkPolyToImageData",mesh,filename,spacing,margin,col,tol)
    return(!as.logical(out))
}

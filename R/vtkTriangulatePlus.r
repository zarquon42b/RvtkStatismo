#' create Isosurface from image
#'
#' create Isosurface from image
#'
#' @param file 3D-image file mha/nii.gz - depending of your system's VTK version
#' @param lower lower threshold value
#' @param upper upper threshold value
#' @param IJK2RAS 4x4 IJK2RAS transform
#' @param kernel a 3 dimensional array defining a morphological opening and closing for x, y ans z axis
#' @param dicom logical: if TRUE, \code{file} points to a directory containing a set of DICOM files belonging to a single series. See notes.
#' @note the dicom functionality only works for unencapsulated raw dicom files. In case of errors, deencapsulate your files first. This can be done, for example, with the command line tool \code{gdcmconv}.
#' @return returns a triangular mesh of class mesh3d
#'
#' @export
vtkTriangulatePlus <- function(file,lower=1,upper=5000,IJK2RAS=diag(c(-1,-1,1,1)),kernel=c(0,0,0),dicom=FALSE) {
    file <- path.expand(file)
    out <- .Call("vtkSegment2PolyDataPlus",file,lower=1,upper=5000,kernel[0],kernel[1],kernel[2],dicom)
    class(out) <- "mesh3d"
    out$vb <- rbind(out$vb,1)
    out <- Morpho::applyTransform(out,IJK2RAS)
    return(out)
}

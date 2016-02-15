#' create Isosurface from image
#'
#' create Isosurface from image
#'
#' @param file 3D-image file mha/nii.gz - depending of your system's VTK version
#' @param value isovalue
#' @param IJK2RAS 4x4 IJK2RAS transform
#' @param dicom logical: if TRUE, \code{file} points to a directory containing a set of DICOM files belonging to a single series. See notes.
#' @note the dicom functionality only works for unencapsulated raw dicom files. In case of errors, deencapsulate your files first. This can be done, for example, with the command line tool \code{gdcmconv}.
#' @return returns a triangular mesh of class mesh3d
#'
#' @export
vtkTriangulate <- function(file,value=1,IJK2RAS=diag(c(-1,-1,1,1)),dicom=FALSE) {
    file <- path.expand(file)
    out <- .Call("vtkSegment2PolyData",file,value,dicom)
    class(out) <- "mesh3d"
    out$vb <- rbind(out$vb,1)
    out <- Morpho::applyTransform(out,IJK2RAS)
    return(out)
}

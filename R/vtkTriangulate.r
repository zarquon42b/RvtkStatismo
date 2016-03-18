#' create Isosurface from image
#'
#' create Isosurface from image
#'
#' @param file 3D-image file mha/nii.gz - depending of your system's VTK version
#' @param value isovalue
#' @param IJK2RAS 4x4 IJK2RAS transform
#' 
#' @param thresholding if TRUE, a thresholding and optional morphological operations are performed prior to surface generation
#' @param lower lower threshold value (only used if \code{thresholding=TRUE})
#' @param upper upper threshold value (only used if \code{thresholding=TRUE})
#' @param kernel a 3 dimensional array defining a morphological opening and closing for x, y ans z axis (only used if \code{thresholding=TRUE})
#' @param dicom logical: if TRUE, \code{file} points to a directory containing a set of DICOM files belonging to a single series. See notes
#' @note the dicom functionality only works for unencapsulated raw dicom files. In case of errors, deencapsulate your files first. This can be done, for example, with the command line tool \code{gdcmconv}.
#' @return returns a triangular mesh of class mesh3d
#'
#' @export
vtkTriangulate <- function(file,value=1,IJK2RAS=diag(c(-1,-1,1,1)),thresholding=FALSE,lower=1,upper=5000,kernel=c(0,0,0),dicom=FALSE) {
    if (!thresholding)
        lower <- value
    file <- path.expand(file)
    if (length(kernel) != 3)
        stop("kernel must be a vector of length 3")
    out <- .Call("vtkSegment2PolyData",file,lower,upper,kernel,dicom,thresholding)
    class(out) <- "mesh3d"
    out$vb <- rbind(out$vb,1)
    out <- Morpho::applyTransform(out,IJK2RAS)
    return(out)
}

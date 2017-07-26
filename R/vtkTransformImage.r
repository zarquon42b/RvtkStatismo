#' Transform an image based on landmark configuration
#'
#' Transform an image based on landmark configuration
#' @param image path to 3D-image (tiff, mhd)
#' @param sourceLM the landmarks on the image
#' @param targetLM the target landmarks
#' @param outname name of the file to be saved (must end with .mha)
#' @param type type of transformation
#' @param RAS2IJK 3x3 transformation matrix from point to image space
#' @param interpolation type of image interpolation 0=Nearest Neighbour, 1=Linear , 2=Cubic
#' @return returns (invisible)  TRUE if successful, else FALSE
#' @export
vtkImageTransform <- function(image,sourceLM,targetLM,outname="default.mha",type=c("none","scale", "affine"),RAS2IJK=diag(c(-1,-1,1)),interpolation=2) {
    interpolation <- interpolation[1]
    image <- path.expand(image)
    type <- tolower(substr(type[1],1L,1L))
    sourceLM <-  sourceLM%*%RAS2IJK
    targetLM <-  targetLM%*%RAS2IJK
    out <- .Call("vtkImageTransformCpp",image,t(sourceLM),t(targetLM),outname,type,interpolation)
    invisible(!as.logical(out))
    
}

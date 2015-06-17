#' Blend two Images using vtkImageBlend
#'
#' Blend two Images using vtkImageBlend
#' @param image1 character: path to first image
#' @param image2 character: path to second image
#' @param outname character: image to save merged image to.
#' @param opacity1 opacity of image1. Accepts values between 0 and 1.
#' @param opacity2 opacity of image2. Accepts values between 0 and 1.
#' @export
vtkImageBlend <- function(image1,image2,outname="out.mha",opacity1=0.5, opacity2=0.5) {
    tmp <- .Call("vtkImageBlender",image1,image2,outname,opacity1,opacity2)
}


#' Resample an image and write to file.
#'
#' Resample an image and write to file.
#' @param image character: path to image file
#' @param outname character: path to write resampled image to.
#' @param spacing integer vector: spacing of resampled image.
#' @param interpolate integer: 0=no interpolation, 1=linear,2=bicubic.
#' @export
vtkImageResize <- function(image,outname,spacing=c(1,1,1),interpolate=2) {
    storage.mode(spacing) <- "double"
    tmp <- .Call("vtkImageReSize",image,outname,spacing,as.integer(interpolate))
}


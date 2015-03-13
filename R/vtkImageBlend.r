#' Blend two Images using vtkImageBlend
#'
#' Blend two Images using vtkImageBlend
#' @param image1 character: path to first image
#' @param image2 character: path to second image
#' @param outname character: image to save merged image to.
#' 
#' @export
vtkImageBlend <- function(image1,image2,outname="out.mha") {
    tmp <- .Call("vtkImageBlender",image1,image2,outname)
}
    

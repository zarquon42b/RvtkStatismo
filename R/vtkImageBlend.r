#' Blend two Images using vtkImageBlend
#'
#' Blend two Images using vtkImageBlend
#' @param image1 character: path to first image
#' @param image2 character: path to second image
#' @param outname character: image to save merged image to.
#' 
#' @export
vtkImageBlend <- function(image1,image2,outname="out.mha",opacity1=0.5, opacity2=0.5) {
    tmp <- .Call("vtkImageBlender",image1,image2,outname,opacity1,opacity2)
}
    
vtkImageResize <- function(image,outname,spacing=c(1,1,1)) {
    storage.mode(spacing) <- "double"
    tmp <- .Call("vtkImageReSize",image,outname,spacing)
}

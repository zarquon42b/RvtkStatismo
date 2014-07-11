#' read fiducials from slicer4
#'
#' read fiducials from slicer4
#' @param x filename
#' @return a k x 3 matrix with landmarks
#' @export
read.fcsv <- function(x) {
    raw <- readLines(x)
    points <- grep("vtkMRMLMarkupsFiducialNode", raw)
    data <- strsplit(raw[points], split = ",")
    subfun <- function(x) {
        tmp <- strsplit(x[2:4], split = "=")
        tmp <- unlist(tmp, recursive = F)
        return(tmp)
    }
    data <- lapply(data, subfun)
    tmp <- as.numeric(unlist(data))
    tmp <- matrix(tmp, length(points), 3, byrow = T)
    return(tmp)
}

#' Transform an image based on landmark configuration
#'
#' Transform an image based on landmark configuration
#' @param image path to 3D-image (tiff, mhd)
#' @param sourceLM the landmarks on the image
#' @param targetLM the target landmarks
#' @param outname name of the file to be saved (must end with .mha)
#' @param type type of transformation
#' @param RAS2LPS logical: if TRUE, the landmarks are converted from RAS to LPS coordinate system (necessary when using slicer4 fiducials.
#' @param interpolation type of image interpolation 0=Nearest Neighbour, 1=Linear , 2=Cubic
#' @return returns (invisible)  TRUE if successful, else FALSE
#' @export
vtkTransformImage <- function(image,sourceLM,targetLM,outname="default.mha",type=c("none","scale", "affine"),RAS2LPS=TRUE,interpolation=2) {
    interpolation <- interpolation[1]
    image <- path.expand(image)
    type <- tolower(substr(type[1],1L,1L))
    if (RAS2LPS) {
        sourceLM <-  sourceLM%*%diag(c(-1,-1,1))
        targetLM <-  targetLM%*%diag(c(-1,-1,1))
    }
    out <- .Call("vtkLMTransfrorm",image,t(sourceLM),t(targetLM),outname,type,interpolation)
    invisible(!as.logical(out))
    
}

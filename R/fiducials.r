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


#' generate a statistical model using an array of superimposed landmarks or a list of meshes
#'
#' generate a statistical model using an array of superimposed landmarks
#'
#' @param x array of aligned 3D-coordinates or a list of aligned registered meshes.
#' @param representer matrix or triangular mesh of class "mesh3d" with vertices corresponding to rows in the array.
#' @param sigma noise in the data
#' @param scale logical: set to TRUE, if scaling was involved in the registration.
#' @return an object of class pPCA (\code{\link{pPCA-class}})
#' @examples
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' mymod <- statismoBuildModel(align,representer=align[,,1],sigma=2,scale=TRUE)
#' ## save it
#' statismoSaveModel(mymod,"mymod.h5")
#' @keywords StatisticalModel<representer>
#' @seealso \code{\link{pPCA}, \link{pPCA-class}, \link{rigidAlign}, \link{meshalign}}
#' @importFrom Morpho bindArr
#' 
#' @export

statismoBuildModel <- function(x,representer,sigma=0,scale=FALSE,SelfAdjointEigenSolver=FALSE) {
    if (is.array(x)) {
        m <- dim(x)[2]
        if (m == 2) {
            zeros <- array(0,dim=c(dim(x)[1],1,dim(x)[3]))
            x <- bindArr(x,zeros,along=2)
        } else if (dim(x)[2] != 3)
        stop("only 2D and 3D configs allowed")
    rawdata <- vecx(x,byrow=TRUE)
    rawdata <- sweep(rawdata,2,colMeans(rawdata))
    mylist <- array2meshlist(x)
    if (missing(representer))
        representer <- x[,,1]
    names(mylist) <- dimnames(x)[[3]]
    } else if (is.list(x)) {
        mylist <-checkmeshlist(x)
        if (missing(representer))
            representer <- x[[1]]
        rawdata <- vecx(meshlist2array(mylist),byrow=TRUE)
        rawdata <- sweep(rawdata,2,colMeans(rawdata))
    }
    if (is.null(names(mylist)))
        names(mylist) <- paste("specimen",1:length(mylist),sep="_")
    
    if (is.matrix(representer)) {
        chk <- prod(dim(representer) == dim(x)[1:2])
        if (!chk)
            stop("representer must be of same dimensionality as array")
        representer <- list(vb=t(representer),it=matrix(0,0,0))
    } else if (is.list(representer)) {
        if (!is.numeric(representer$vb) || !is.numeric(representer$it))
            stop("representer needs vertices and faces")
        else if (ncol(representer$it > 0))
            representer$it <- representer$it
    } else {
        stop("representer must be a matrix or a mesh")
    }
    
    out <- .Call("BuildModelExport",mylist,representer,sigma,SelfAdjointEigenSolver)
    SetScale(out) <- scale
    return(out)
    
    
}

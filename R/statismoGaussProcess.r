#' expands a models variability by adding a Gaussian kernel function
#'
#' expands a models variability by adding a Gaussian kernel function to the empiric covariance matrix and builds a low-rank approximation of the resulting PCA
#'
#' @param model shape model of class \code{\link{pPCA}}
#' @param useEmpiric logical: if TRUE, the empiric covariance kernel will be added to the Gaussian ones.
#' @param kernel a list containing numeric vectors of length 2. Except the first entry of this list may be of length 2 and is then interpreted as Multiscale Bspline kernel. For a Gaussian Kernel, the first entry specifies the bandwidth and the second the scaling. For a Multiscale kernel, the additional 3rd entry sets the number of levels. 
#' @param ncomp integer: number of PCs to approximate
#' @param nystroem number of samples to compute Nystroem approximation of eigenvectors
#' @param combine character: determining how to combine the kernels: "sum" or "product" are supported.
#' @param combineEmp character: determining how the newly created kernel is combined to the one from the model: "sum" or "product" are supported.
#' @param isoScale standard deviation of isotropic scaling. 
#' @param centroid specify the center of scaling. If NULL, the centroid will be used.
#' @return returns a shape model of class \code{\link{pPCA}}
#' @examples
#' ### this is a silly example with only 10 landmarks
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' mod <- statismoBuildModel(align)
#' ##extend flexibility using two Gaussian kernels
#' GPmod <- statismoGPmodel(mod,kernel=list(c(10,1),c(1,1)))
#' ##extend flexibility using two Gaussian kernels but ignoring empiric covariance.
#' GPmodNoEmp <- statismoGPmodel(mod,kernel=list(c(10,1),c(1,1)),useEmpiric = FALSE)
#' PC1orig <- DrawSample(mod,2)# get shape in 2sd of first PC of originial model
#' PC1 <- DrawSample(GPmod,2)# get shape in 2sd of first PC of the extended model
#' PC1NoEmp <- DrawSample(GPmodNoEmp,2)# get shape in 2sd of first PC
#' ##visualize the differences from the mean (green spheres)
#' \dontrun{
#' deformGrid3d(PC1,DrawMean(GPmod),ngrid=0)##
#' ##only deviates in 5 landmarks from the mean (dark blue)
#' deformGrid3d(PC1NoEmp,DrawMean(GPmod),ngrid=0,col1=4,add=TRUE)
#' deformGrid3d(PC1orig,DrawMean(GPmod),ngrid=0,col1=5,add=TRUE)
#' }
#' @seealso \code{\link{pPCA}, \link{pPCA-class}}
#' @keywords StatisticalModel<representer>
#' @export
statismoGPmodel <- function(model,useEmpiric=TRUE,kernel=list(c(100,70)),ncomp=10,nystroem=500, combine=c("sum","product"),combineEmp="sum", isoScale=0, centroid=NULL) {
    combine <- combine[1]
    if (combine == "sum")
        combine <- 0
    else
        combine <- 1
    combineEmp <- combineEmp[1]
    if (combineEmp == "sum")
        combineEmp <- 0
    else
        combineEmp <- 1
    
    ncomp <- as.integer(ncomp)
    if (!inherits(model,"pPCA"))
        stop("please provide model of class 'pPCA'")
    if (!is.list(kernel))
        stop("kernel needs to be a list of two-entry vectors")
    k <- nrow(GetDomainPoints(model))
    nystroem <- min(k,nystroem)
    ncomp <- min(ncomp,floor(k/2))
    storage.mode(nystroem) <- "integer"
    chk <- lapply(kernel,length)
    kernelVec <- unlist(kernel)
    chkZeroScale <- prod(sapply(kernel,function(x) x <- x[1]))
    chkZeroSigma <- prod(sapply(kernel,function(x) x <- x[2]))
    if (chkZeroSigma == 0)
        stop("kernels with zero sigma are not allowed. For using dummy kernels set scale=0")
    if (chkZeroScale == 0 && !useEmpiric) {
        if (isoScale > 0) {
            message("found dummykernel: ncomp is set to 1")
            ncomp <- 1
        } else {
            stop("dummy kernels are only allowed if isoScale > 0")
        }
    }
    if (is.null(centroid)) {
        if (isoScale == 0) {
            centroid <- rep(0,3)
        } else {
            centroid <- apply(GetDomainPoints(model),2,mean)
        }
    }
    useEmpiric <- as.logical(useEmpiric)
    if (!(prod(unlist(chk) >= 2) * is.numeric(unlist(kernel))))
        stop("only provide two-valued numeric vectors in kernel")
    
    
    out <- .Call("BuildGPModelExport",model,kernel,ncomp,nystroem,useEmpiric,combine, combineEmp,isoScale=isoScale,centroid=centroid)
    SetScale(out) <- model@scale
    return(out)
                         
}




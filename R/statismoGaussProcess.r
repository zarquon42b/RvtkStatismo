#' expands a models variability by adding a Gaussian kernel function
#'
#' expands a models variability by adding a Gaussian kernel function to the empiric covariance matrix and builds a low-rank approximation of the resulting PCA
#'
#' @param model shape model of class \code{\link{pPCA}}
#' @param useEmpiric logical: if TRUE, the empiric covariance kernel will be added to the Gaussian ones.
#' @param kernel a list containing two valued vectors containing with the first entry specifiying the bandwidth and the second the scaling of the Gaussian kernels.
#' @param ncomp integer: number of PCs to approximate
#' @param nystroem number of samples to compute Nystroem approximation of eigenvectors
#' @param combine character: determining how to combine the kernels: "sum" or "product" are supported.
#' @param combineEmp character: determining how the newly created kernel is combined to the one from the model: "sum" or "product" are supported.
#' @return returns a shape model of class \code{\link{pPCA}}
#' @examples
#' ### this is a silly example with only 10 landmarks
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' mod <- statismoBuildModel(align)
#' GPmod <- statismoGPmodel(mod,kernel=list(c(10,1),c(1,1)))##extend flexibility using two Gaussian kernels
#' GPmodNoEmp <- statismoGPmodel(mod,kernel=list(c(10,1),c(1,1)),useEmpiric = FALSE)##extend flexibility using two Gaussian kernels but ignoring empiric covariance.
#' PC1orig <- DrawSample(mod,2)# get shape in 2sd of first PC of originial model
#' PC1 <- DrawSample(GPmod,2)# get shape in 2sd of first PC of the extended model
#' PC1NoEmp <- DrawSample(GPmodNoEmp,2)# get shape in 2sd of first PC
#' ##visualize the differences from the mean (green spheres)
#' deformGrid3d(PC1,DrawMean(GPmod),ngrid=0)##
#' deformGrid3d(PC1NoEmp,DrawMean(GPmod),ngrid=0,col1=4,add=TRUE)##only deviates in 5 landmarks from the mean (dark blue)
#' deformGrid3d(PC1orig,DrawMean(GPmod),ngrid=0,col1=5,add=TRUE)
#' @seealso \code{\link{pPCA}, \link{pPCA-class}}
#' @keywords StatisticalModel<representer>
#' @export
statismoGPmodel <- function(model,useEmpiric=TRUE,kernel=list(c(100,70)),ncomp=10,nystroem=500, combine=c("sum","product"),combineEmp="sum") {
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
    k <- ncol(model@representer$vb)
    nystroem <- min(k,nystroem)
    ncomp <- min(ncomp,floor(k/2))
    storage.mode(nystroem) <- "integer"
    chk <- lapply(kernel,length)
    useEmpiric <- as.logical(useEmpiric)
    if (!(prod(unlist(chk) == 2) * is.numeric(unlist(kernel))))
        stop("only provide two-valued numeric vectors in kernel")
    out <- .Call("BuildGPModelExport",model,kernel,ncomp,nystroem,useEmpiric,combine, combineEmp)
    SetScale(out) <- model@scale
    return(out)
                         
}




#' expands a models variability by adding a Gaussian kernel function
#'
#' expands a models variability by adding a Gaussian kernel function to the empiric covariance matrix and builds a low-rank approximation of the resulting PCA
#'
#' @param model shape model of class \code{\link{pPCA}}
#' @param kernel an object of class matrixKernel 
#' @param ncomp integer: number of PCs to approximate
#' @param nystroem number of samples to compute Nystroem approximation of eigenvectors
#' @return returns a shape model of class \code{\link{pPCA}}
#' @examples
#' ### this is a silly example with only 10 landmarks
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' mod <- statismoBuildModel(align)
#' ##extend flexibility using two Gaussian kernels
#' ## Get the empirical kernel first
#' isoKernel <- IsoKernel(DrawMean(mod),scale=0.001)
#' kernel1 <- GaussianKernel(10,1)
#' kernel2 <- GaussianKernel(1,1)
#' combinedKernel <- SumKernels(kernel1,kernel2)
#' combinedKernelNoEmp <- SumKernels(combinedKernel,isoKernel)
#' combinedKernel <- SumKernels(combinedKernelNoEmp,StatisticalModelKernel())
#' GPmod <- statismoGPmodel(mod,kernel=combinedKernel)
#' ##extend flexibility using two Gaussian kernels but ignoring empiric covariance.
#' GPmodNoEmp <- statismoGPmodel(mod,kernel=combinedKernelNoEmp)
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
statismoGPmodel <- function(model,kernel=GaussianKernel(50,10),ncomp=10,nystroem=500) {
    #gc()
    ## empargs <- c("none","sum","product")
    ## empiric <- match.arg(empiric[1],empargs)
    ## empiric <- match(empiric,empargs)-1L
    kernel <- MatrixKernel2CombinedKernel(kernel)
    nystroemnew <- max(nystroem,ncomp*2)
    if (nystroemnew > nystroem)
        cat(paste("nystroem set to",nystroemnew),"\n")
    nystroem <- nystroemnew
    
    ncomp <- as.integer(ncomp)
    if (!inherits(model,"pPCA") )
        stop("please provide model of class 'pPCA'")
    ## if (!inherits(kernel,"matrixKernel"))
    ##    stop("kernel needs to be of class matrixKernel")
    k <- nrow(GetDomainPoints(model))
    nystroem <- min(k*3,nystroem)
    ncomp <- min(ncomp,floor(k/2))
    storage.mode(nystroem) <- "integer"
    if (inherits(kernel,"IsoKernel"))
        ncomp <- 1
    out <- .Call("BuildGPModelExport",model,kernel,ncomp,nystroem)
    gc()
    SetScale(out) <- model@scale
    return(out)
                         
}




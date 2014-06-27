
#' save and load a statistical model of class pPCA to statismo hdf5 format
#'
#' save and load a statistical model of class pPCA to statismo hdf5 format
#' @param model object of class \code{\link{pPCA}}
#' @param modelname filename to read/save
#' @return statismoLoadModel returns an object of class \code{\link{pPCA}} while statismoSaveModel saves an object of class \code{\link{pPCA}} to disk in the statismo file format.
#' @name statismoLoadModel/statismoSaveModel
#' @seealso \code{\link{pPCA}}
#' @keywords StatisticalModel<representer>
#' @rdname statismoIO
#' @export
statismoSaveModel <- function(model, modelname=dataname) {
    dataname <- deparse(substitute(model))
    dataname <- paste0(dataname,".h5")
    storage.mode(modelname) <- "character"
    if (!inherits(model,"pPCA"))
        stop("model must be of class pPCA")
    out <- .Call("SaveModel",model,modelname)
}

#' @rdname statismoIO
#' @export
statismoLoadModel <- function(modelname,scale=TRUE) {
    modelname <- path.expand(modelname)
    if (length(modelname) != 1)
        stop("only one file at a time please")
    if (! file.exists(modelname))
        stop(paste0("file ", modelname," does not exist"))
    storage.mode(modelname) <- "character"
    
    out <- (.Call("LoadModel",modelname))
    return(out)
}

#' expands a models variability by adding a Gaussian kernel function
#'
#' expands a models variability by adding a Gaussian kernel function to the empiric covariance matrix and builds a low-rank approximation of the resulting PCA
#'
#' @param model shape model of class \code{\link{pPCA}}
#' @param useEmpiric logical: if TRUE, the empiric covariance kernel will be added to the Gaussian ones.
#' @param kernel a list containing two valued vectors containing with the first entry specifiying the bandwidth and the second the scaling of the Gaussian kernels.
#' @param ncomp integer: number of PCs to approximate
#' @param nystroem number of samples to compute Nystroem approximation of eigenvectors
#' @return returns a shape model of class \code{\link{pPCA}}
#' @examples
#' ### this is a silly example with only 10 landmarks
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' mod <- statismoBuildModel(align)
#' GPmod <- statismoGPmodel(mod,kernel=list(c(10,1),c(1,1)))##extend flexibility using two Gaussian kernels
#' GPmodNoEmp <- statismoGPmodel(mod,kernel=list(c(10,1),c(1,1)),useEmpiric = FALSE)##extend flexibility using two Gaussian kernels but ignoring empiric covariance.
#' PC1orig <- predictpPCA(2,mod)# get shape in 2sd of first PC of originial model
#' PC1 <- predictpPCA(2,GPmod)# get shape in 2sd of first PC of the extended model
#' PC1NoEmp <- predictpPCA(2,GPmodNoEmp)# get shape in 2sd of first PC
#' ##visualize the differences from the mean (green spheres)
#' deformGrid3d(PC1,DrawMean(GPmod),ngrid=0)##
#' deformGrid3d(PC1NoEmp,DrawMean(GPmod),ngrid=0,col1=4,add=TRUE)##only deviates in 5 landmarks from the mean (dark blue)
#' deformGrid3d(PC1orig,DrawMean(GPmod),ngrid=0,col1=5,add=TRUE)
#' @seealso \code{\link{pPCA}, \link{pPCA-class}}
#' @keywords StatisticalModel<representer>
#' @export
statismoGPmodel <- function(model,useEmpiric=TRUE,kernel=list(c(100,70)),ncomp=10,nystroem=500) {
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
    out <- .Call("BuildGPModelExport",model,kernel,ncomp,nystroem,useEmpiric)
    out <- UpdateVariance(out)
    return(out)
                         
}




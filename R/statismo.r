#' generate a statistical model using an array of superimposed landmarks or a list of meshes
#'
#' generate a statistical model using an array of superimposed landmarks
#'
#' @param x array of aligned 3D-coordinates or a list of aligned registered meshes.
#' @param representer matrix or triangular mesh of class "mesh3d" with vertices corresponding to rows in the array.
#' @param sigma noise in the data
#' @param scale logical: set to TRUE, if scaling was involved in the registration.
#' @examples
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' mymod <- statismoBuildModel(align,representer=align[,,1],sigma=2,scale=TRUE)
#' ## save it
#' statismoSaveModel(mymod,"mymod.h5")
#'
#' 
#' @importFrom Morpho bindArr
#' 
#' @export

statismoBuildModel <- function(x,representer,sigma=0,scale=TRUE) {
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
    
    out <- .Call("BuildModelExport",mylist,representer,sigma)
    if (is.list(out)) {
        out$scale <- scale
        out1 <- statismo2pPCA(out)
        out1$rawdata <- rawdata
        return(out1)
    } else {
        warning("something went wrong")
    }
}

#' save and load a statistical model of class pPCA to statismo hdf5 format
#'
#' save and load a statistical model of class pPCA to statismo hdf5 format
#' @param model object of class pPCA
#' @param modelname filename to read/save
#' @return statismoLoadModel returns an object of class "pPCA" while statismoSaveModel saves an object of class pPCA to disk in the statismo file format.
#' @name statismoLoadModel/statismoSaveModel
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
    
    out <- statismo2pPCA(.Call("LoadModel",modelname))
    return(out)
}
statismo2pPCA <- function(statismodel) {
    out1 <- list()
    out1$mshape <- matrix(statismodel$mshape,length(statismodel$mshape)/statismodel$dim,byrow = T)
    out1$PCA <- list();class(out1) <- "pPCA"
    out1$PCA$sdev <- sqrt(statismodel$PCVariance)
    out1$PCA$rotation <- statismodel$PCBasisOrtho
    out1$PCA$center <- statismodel$mshape
    out1$PCA$x <- t(statismodel$scores)
    out1$scale <- statismodel$scale
    out1$exVar <- 1
    out1$sigma <- statismodel$sigma
    out1$Variance <- createVarTable(out1$PCA$sdev)
    out1$representer <- statismodel$representer
    if (inherits(out1$representer,"mesh3d"))
        out1$representer$vb <- rbind(out1$representer$vb,1)
    else
        out1$representer$it <- matrix(0,0,0)
    return(out1)
}
#' expands a models variability by adding a Gaussian kernel function
#'
#' expands a models variability by adding a Gaussian kernel function to the empiric covariance matrix and builds a low-rank approximation of the resulting PCA
#'
#' @param model shape model of class "pPCA"
#' @param useEmpiric logical: if TRUE, the empiric covariance kernel will be added to the Gaussian ones.
#' @param kernel a list containing two valued vectors containing with the first entry specifiying the bandwidth and the second the scaling of the Gaussian kernels (currently only the first list entry is used)
#' @param ncomp integer: number of PCs to approximate
#' @param nystroem number of samples to compute Nystroem approximation of eigenvectors
#' @return returns a shape model of class "pPCA"
#' @examples
#' ### this is a silly example with only 10 landmarks
#' require(Morpho)
#' data(boneData)
#' align <- ProcGPA(boneLM,CSinit=FALSE, scale=TRUE,silent = TRUE)$rotated
#' mod <- statismoBuildModel(align)
#' GPmod <- statismoGPmodel(mod,kernel=list(c(10,1),c(1,1)))##extend flexibility using two Gaussian kernels
#' GPmodNoEmp <- statismoGPmodel(mod,kernel=list(c(10,1),c(1,1)),useEmpiric = FALSE)##extend flexibility using two Gaussian kernels but ignoring empiric covariance.
#' PC1orig <- predictpPCA(2,mod)# get shape in 2sd of first PC of originial model
#' PC1 <- predictpPCA(2,GPmod)# get shape in 2sd of first PC of the extended model
#' PC1NoEmp <- predictpPCA(2,GPmodNoEmp)# get shape in 2sd of first PC
#' ##visualize the differences from the mean (green spheres)
#' deformGrid3d(PC1,GPmod$mshape,ngrid=0)##
#' deformGrid3d(PC1NoEmp,GPmod$mshape,ngrid=0,col1=4,add=TRUE)##only deviates in 5 landmarks from the mean (dark blue)
#' deformGrid3d(PC1orig,GPmod$mshape,ngrid=0,col1=5,add=TRUE)
#' @export
statismoGPmodel <- function(model,useEmpiric=TRUE,kernel=list(c(100,70)),ncomp=10,nystroem=500) {
    ncomp <- as.integer(ncomp)
    if (!inherits(model,"pPCA"))
        stop("please provide model of class 'pPCA'")
    if (!is.list(kernel))
        stop("kernel needs to be a list of two-entry vectors")
    k <- ncol(model$representer$vb)
    nystroem <- min(k,nystroem)
    ncomp <- min(ncomp,floor(k/2))
    storage.mode(nystroem) <- "integer"
    chk <- lapply(kernel,length)
    useEmpiric <- as.logical(useEmpiric)
    if (!(prod(unlist(chk) == 2) * is.numeric(unlist(kernel))))
        stop("only provide two-valued numeric vectors in kernel")
    out <- statismo2pPCA(.Call("BuildGPModelExport",model,kernel,ncomp,nystroem,useEmpiric))
    return(out)
                         
}




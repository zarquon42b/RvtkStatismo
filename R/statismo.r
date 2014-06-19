#' generate a statistical model using an array of superimposed landmarks and save it
#'
#' generate a statistical model using an array of superimposed landmarks and save it
#'
#' @param array array of aligned 3D-coordinates (e.g. the vertices of meshes)
#' @param representer matrix or triangular mesh of class "mesh3d" with vertices corresponding to rows in the array.
#' @param sigma noise in the data
#' @examples
#' require(Morpho)
#' data(boneData)
#' align <- ProcGPA(boneLM,CSinit=FALSE, scale=TRUE,silent = TRUE)$rotated
#' mymod <- statismoBuildModel(align,representer=align[,,1],sigma=2,scale=TRUE)
#' ## save it
#' statismoSaveModel(mymod,"mymod.h5")
#'
#' 
#' @importFrom Morpho bindArr
#' 
#' @export

statismoBuildModel <- function(array,representer,sigma=0,scale=TRUE) {
    m <- dim(array)[2]
    if (m == 2) {
        zeros <- array(0,dim=c(dim(array)[1],1,dim(array)[3]))
        array <- bindArr(array,zeros,along=2)
        print(dim(array)[2])
    } else if (dim(array)[2] != 3)
        stop("only 2D and 3D configs allowed")
    rawdata <- vecx(array,byrow=TRUE)
    rawdata <- sweep(rawdata,2,colMeans(rawdata))
    mylist <- array2meshlist(array)
    if (missing(representer))
        representer <- array[,,1]
    names(mylist) <- dimnames(array)[[3]]
    if (is.null(names(mylist)))
        names(mylist) <- paste("specimen",1:length(mylist),sep="_")
    
    if (is.matrix(representer)) {
        chk <- prod(dim(representer) == dim(array)[1:2])
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
#' @return statismoLoadModel returns an object of class "pPCA"
#' @export
statismoSaveModel <- function(model, modelname=dataname) {
    dataname <- deparse(substitute(model))
    dataname <- paste0(dataname,".h5")
    storage.mode(modelname) <- "character"
    if (!inherits(model,"pPCA"))
        stop("model must be of class pPCA")
    out <- .Call("SaveModel",model,modelname)
}

#' @export
statismoLoadModel <- function(modelname) {
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
    out1$scale <- scale
    out1$W <- statismodel$PCBasis
    out1$usePC <- 1:ncol(out1$W)
    out1$sigma <- statismodel$sigma
    Wval <- apply(out1$W,2,function(x) x <- sqrt(sum(crossprod(x))))
    out1$Win <- out1$PCA$rotation[,out1$usePC]
    out1$Win <- (t(out1$Win)*1/Wval)
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
#' @param kernel a list containing two valued vectors containing with the first entry specifiying the bandwidth and the second the scaling of the Gaussian kernels (currently only the first list entry is used)
#' @param ncomp integer: number of PCs to approximate
#' @param nystroem number of samples to compute Nystroem approximation of eigenvectors
#' @return returns a shape model of class "pPCA"
#' @examples
#' require(Morpho)
#' data(boneData)
#' mod <- statismoBuildModel(boneLM)
#' GPmod <- statismoGPmodel(mod,kernel=list(c(1,0.1)))##extend flexibility
#' PC1 <- predictpPCA(2,GPmod)
#' deformGrid3d(PC1,GPmod$mshape,ngrid=0)
#' @export
statismoGPmodel <- function(model,kernel=list(c(100,70)),ncomp=10,nystroem=500) {
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
    if (!(prod(unlist(chk) == 2) * is.numeric(unlist(kernel))))
        stop("only provide two-valued vectors in kernel")
    out <- statismo2pPCA(.Call("BuildGPModelExport",model,kernel,ncomp,nystroem))
    return(out)
                         
}
#' @export
statismoDrawMean <- function(model) {
    if (!inherits(model,"pPCA"))
        stop("please provide model of class 'pPCA'")
    out <- (.Call("DrawMean",model))
    out$vb <- rbind(out$vb,1)
    return(out)
}



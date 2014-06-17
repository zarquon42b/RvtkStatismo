#' generate a statistical model using an array of superimposed landmarks and save it
#'
#' generate a statistical model using an array of superimposed landmarks and save it
#'
#' @param array array of aligned 3D-coordinates (e.g. the vertices of meshes)
#' @param representer matrix or triangular mesh of class "mesh3d" with vertices corresponding to rows in the array.
#' @param sigma noise in the data
#' @export

statismoBuildModel <- function(array,representer,sigma=0,scale=TRUE) {
    m <- dim(array)[2]
    if (m == 2) {
        zeros <- array(0,dim=c(dim(array)[1],1,dim(array)[3]))
        array <- bindArr(array,zeros,along=2)
        print(dim(array)[2])
    } else if (dim(array)[2] != 3)
        stop("only 2D and 3D configs allowed")
    
    mylist <- array2meshlist(array)
    
    names(mylist) <- dimnames(array)[[3]]
    if (is.null(names(mylist)))
        names(mylist) <- paste("specimen",1:length(mylist),sep="_")
    
    if (is.matrix(representer))
        representer <- list(vb=t(representer),it=matrix(0,0,0))
    else if (is.list(representer)) {
        if (!is.numeric(representer$vb) || !is.integer(representer$it))
            stop("representer needs vertices and faces")
        else if (ncol(representer$it > 0))
            representer$it <- representer$it-1
    }
    
    out <- .Call("BuildModel",mylist,representer,sigma)
    if (is.list(out)) {
        out1 <- statismo2pPCA(out)
        return(out1)
    } else {
        warning("something went wrong")
    }
}

#' save a statistical model of class pPCA to statismo hdf5 format
#'
#' save a statistical model of class pPCA to statismo hdf5 format
#' @param model object of class pPCA
#' @param modelname filename to save to
#' @export
statismoSaveModel <- function(model, modelname) {
    storage.mode(modelname) <- "character"
    if (!inherits(model,"pPCA"))
        stop("model must be of class pPCA")
    out <- .Call("SaveModel",model,modelname)
}

statismo2pPCA <- function(statismodel) {
    out1 <- list()
    out1$mshape <- matrix(statismodel$mshape,length(statismodel$mshape)/statismodel$dim,byrow = T)
    out1$PCA <- list();class(out1) <- "pPCA"
    out1$PCA$sdev <- sqrt(statismodel$PCVariance)
    out1$PCA$rotation <- statismodel$PCBasisOrtho
    out1$PCA$center <- statismodel$mshape
    out1$scale <- scale
    out1$W <- statismodel$PCBasis
    out1$usePC <- 1:ncol(out1$W)
    out1$sigma <- statismodel$sigma
    Wval <- apply(out1$W,2,function(x) x <- sqrt(sum(crossprod(x))))
    out1$Win <- out1$PCA$rotation[,out1$usePC]
    out1$Win <- (t(out1$Win)*1/Wval)
    out1$PCA$x <- t(statismodel$scores)
    out1$refmesh <- statismodel$refmesh
    return(out1)
}

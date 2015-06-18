#' generate a statistical model using an array of superimposed landmarks or a list of meshes
#'
#' generate a statistical model using an array of superimposed landmarks
#'
#' @param x array of aligned 3D-coordinates or a list of aligned registered meshes.
#' @param representer matrix or triangular mesh of class "mesh3d" with vertices corresponding to rows in the array.
#' @param sigma noise in the data
#' @param scale logical: set to TRUE, if scaling was involved in the registration.
#' @param trainingData a numeric matrix containing categorial variables (as integers) and continuous variables for each sample in \code{x}
#' @param conditioningData a numeric vector of length \code{ncol(trainingData)} containing the parameters to condition the model to
#' @param surrogateInfo a numeric vector of zeros and ones coding wether the variables in the i-th column of \code{trainingData} is categorial or continuous. 0 = categorial, 1 = continuous.
#' @param exVar with 0 < exVar <= 1. Allows to reduce the model according to the variance explained.
#' @references
#' R. Blanc, M. Reyes, C. Seiler and G. Szekely. 2009. Conditional Variability of Statistical Shape Models Based on Surrogate Variables. In Proc. MICCAI 2009
#' @note If you want to use a data.frame of mixed variables, you can use the helper function manageConditioningData to extract and convert the information to accomodate the necesseties of the function.
#' @return an statistical model of class pPCA (\code{\link{pPCA-class}})
#' @examples
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' pop <- name2factor(boneLM,which=3)
#' ##prepare data
#' conddata <- manageConditioningData(pop)
#' ##now condition the model to Chinese
#' condmod <- statismoBuildConditionalModel(align,trainingData=conddata$trainingData,
#'                                         conditioningData=1, surrogateInfo=conddata$surrogateInfo)
#' ##now the mean of this model corresponds to the shape of the average Chinese
#' apply(align[,,pop=="ch"],1:2,mean)
#' DrawMean(condmod)
#' ## save it
#' statismoSaveModel(condmod,"condmod.h5")
#' 
#' @keywords StatisticalModel<representer>
#' @seealso \code{\link{pPCA}, \link{pPCA-class}, \link{rigidAlign}, \link{meshalign}, \link{manageConditioningData}} 
#' @importFrom Morpho bindArr
#' 
#' @export
statismoBuildConditionalModel <- function(x,representer,sigma=0,scale=FALSE,trainingData,conditioningData,surrogateInfo,exVar=1) {
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
    
    conditioningData <- cbind(1,conditioningData)
    trainingData <- as.matrix(trainingData)
    surrogateInfo <- as.numeric(surrogateInfo);
    out <- .Call("BuildConditionalModelExport",mylist,representer,sigma,trainingData,conditioningData,surrogateInfo,exVar)
    SetScale(out) <- scale
    return(out)
    
    
}

#' convert a data.frame or vector of factors into a format usable by statismoBuildConditionalModel
#'
#' convert a data.frame or vector of factors into a format usable by statismoBuildConditionalModel and extract variable information.
#'
#' @param trainingData training data, can be data.frame, character vector/matrix, etc.
#' @return
#' \item{trainingData}{Data converted into numeric matrix}
#' \item{surrogateInfo}{vector of 0 and 1 indicating which input was categorial/continuous}
#' \item{encode}{in case categorial variables where encoded into integers, this lists the association of the categorial values to the corresponding dummy encoding.}
#' \item{order}{order of variables after resorting}
#' @note the variables will be resorted with the first being continuous variables and then categorial ones.
#' @seealso \code{\link{statismoBuildConditionalModel}}
#' @export
manageConditioningData <- function(trainingData) {
    
    if (is.vector(trainingData)|| is.factor(trainingData))
        trainingData <- as.data.frame(trainingData)
    orig <- trainingData
    if (is.data.frame(trainingData)) {
        surrogateInfo <- as.integer(unlist(lapply(trainingData,is.numeric)))
        surrorder <- order(surrogateInfo,decreasing=TRUE)
        surrogateInfo <- surrogateInfo[surrorder]
        trainingData <- trainingData[,surrorder,drop=FALSE]
        trainingData <- as.data.frame(lapply(trainingData,as.numeric))
        orig <- orig[,surrorder,drop=FALSE]
    } else if (is.matrix(trainingData)) {
        surrogateInfo <- rep(1,ncol(trainingData))
    }  else {
        stop("trainingData must be a vector, matrix or data.frame")
    }
    trainingData <- as.matrix(trainingData)
    out <- list()
    out$trainingData <- trainingData
    out$surrogateInfo <- as.numeric(surrogateInfo)
    out$order <- colnames(trainingData)
    encode <- list()
    catVar <- which(surrogateInfo==0)
    if (length(catVar)) {
        for(i in 1:length(catVar)) {
            encode[[names(orig)[i]]] <- unique(data.frame(orig[,catVar[i]],trainingData[,catVar[i]],row.names = NULL))
        }
        out$encode <- encode
    }
    return(out)
}

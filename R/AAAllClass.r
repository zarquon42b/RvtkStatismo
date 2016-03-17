setOldClass("mesh3d")
setClassUnion("representer",c("list","mesh3d"))

#' Documentation of class modelinfo
#'
#' Documentation of class modelinfo
#'
#' The class has the following slots
#' \describe{
#' \item{datainfo}{a list containing 2-valued character vectors}
#' \item{parminfo}{a list containing 2-valued character vectors}
#' }
#' These can be modified using addParams and setParaminfo
#' with
#' @param x an object of class "modelinfo"
#' @param value  a list of or a single 2-valued character vectors 
#' @name modelinfo-class
#' @rdname modelinfo-class
#' @export
setClass("modelinfo", slots=c(datainfo="list",paraminfo="list"),prototype=list(datainfo=list(),paraminfo=list())
         )

.modelinfo.valid <- function(object) {
    lendat <- lencheck(object@datainfo)
    lenparam <- lencheck(object@paraminfo)
    classdat <- classcheck(object@datainfo)
    classparam <- classcheck(object@paraminfo)
    if (!lendat || !classdat) {
            return("datainfo is invalid")
    }
    else if (!classdat || !classparam) {
            return("paraminfo is invalid")
    }    
    
    else
        return(TRUE)
}

lencheck <- function(x) {
    return((prod(lapply(x,length) == 2)))
}
classcheck <- function(x) return(as.logical(prod(lapply(x,class) =="character")))
setValidity("modelinfo", .modelinfo.valid)


#' Documentation of class pPCA
#'
#' Documentation of class pPCA
#' 
#' The class contains the the follwing slots (still not yet set in stone)
#' \describe{
#' \item{PCA}{a list containing
#' \itemize{
#' \item{\code{sdev}: the square roots of the covariance matrix' eigenvalues}
#' \item{\code{rotation}: matrix containing the orthonormal PCBasis vectors}
#' \item{\code{x}: the scores within the latent space(scaled by 1/sdev)}
#' \item{\code{center}: a vector of the mean shape in with coordinates ordered
#'
#' \code{(x1,y1,z1, x2, y2,z2, ..., xn,yn,zn)}}
#'  }
#' }
#' \item{scale}{logical: indicating if the data was aligned including scaling}
#' \item{representer}{an object of class mesh3d or a list with entry \code{vb} being a matrix with the columns containing coordinates and \code{it} a 0x0 matrix}
#' \item{sigma}{the noise estimation of the data}
#' \item{Variance}{a data.frame containing the Variance, cumulative Variance and Variance explained by each Principal component}
#' \item{rawdata}{optional data: a matrix with rows containing the mean centred coordinates in order \code{(x1,y1,z1, x2, y2,z2, ..., xn,yn,zn)}}
#' }
#' @import methods
#' @name pPCA-class
#' @rdname pPCA-class
#' @export
setClass("pPCA",
         slots= c(PCA="list",scale="logical",representer="representer",rawdata="matrix",sigma="numeric",modelinfo="modelinfo"),
         prototype = list(PCA=list(sdev=0,rotation=as.matrix(0),x=matrix(0,0,0),center=0),scale=FALSE,representer=list(),rawdata=matrix(0,0,0),sigma=numeric(0),modelinfo=new("modelinfo")
             )
         )



.pPCA.valid <- function(object) {
    dimbasis <- dim(object@PCA$rotation)
    sdevlen <- length(object@PCA$sdev)
    centerlen <- length(object@PCA$center)
    representerlen <- length(as.vector(object@representer$vb[1:3,]))
    if (centerlen != dimbasis[1] || centerlen != representerlen)
        return("dimension of meanvector and PC-Basis or Representer coordinates differ ")
    else if (sdevlen != dimbasis[2])
        return("number of standarddeviations and number of Basisvectors differ"
               )
    else
        return(TRUE)
}
setValidity("pPCA", .pPCA.valid)

#' Documentation of pPCA_pointer class
#'
#' Documentation of pPCA_pointer class
#' this class stores a pointer to an object of statismo::StatisticalModel<vtkPolyData>.
#' It has the following slots
#' \itemize{
#'  \item{\code{pointer} - externalptr: contains the external pointer}
#'  \item{\code{scale} - logical: contains information whether the data to build the model was aligned including scaling}
#' }
#' @export
setClass("pPCA_pointer",slots=c(pointer="externalptr",scale="logical"),prototype=list(pointer=NULL,scale=FALSE))


#' Documentation of kernel classes
#'
#' Documentation of kernel classes
#'
#' 
#' Blow are all S4 kernel classes and their containing slots, where the parameters are stored, that later used when passed to statismo. All kernel-classes have a common slot:
#' \itemize{
#'  \item{\code{kerneltype} - character: containing kernel specifics}
#' }
#' 
#' Available kernels are:
#' MultiscaleBSplineKernel-class
#' \itemize{
#'  \item{\code{support} - numeric: the BSpline support}
#'  \item{\code{scale} - numeric: scale factor}
#'  \item{\code{levels} - integer: Number of levels}
#' }
#' 
#' BSplineKernel-class:
#' \itemize{
#'  \item{\code{support} - numeric: the BSpline support}
#' }
#' 
#' GaussianKernel-class
#' \itemize{
#'  \item{\code{sigma} - numeric: kernel bandwidth}
#'  \item{\code{scale} - numeric: scale factor}
#' }
#' 
#' IsoKernel-class
#' \itemize{
#'  \item{\code{centroid} - vector: centroid around which to be scaled}
#'  \item{\code{scale} - numeric: scale factor}
#' }
#' 
#' CombinedKernel-class
#' \itemize{
#'  \item{\code{kernel} - list(s) containing the kernel above}
#' }
#' @name kernel-classes
#' @rdname kernel-classes
NULL

#' @rdname kernel-classes
#' @export
setClass("MultiscaleBSplineKernel",slots=c(support="numeric",scale="numeric",levels="integer", kerneltype="character"))

#' @rdname kernel-classes
#' @export
setClass("BSplineKernel",slots=c(support="numeric",scale="numeric",kerneltype="character"))

#' @rdname kernel-classes
#' @export
setClass("GaussianKernel",slots=c(sigma="numeric",scale="numeric",kerneltype="character"))


#' @rdname kernel-classes
#' @export
setClass("IsoKernel",slots=c(centroid="numeric",scale="numeric",kerneltype="character"))


#' @rdname kernel-classes
#' @export
setClass("StatisticalModelKernel",slots=c(kerneltype="character"))



#' @rdname kernel-classes
#' @export
setClass("CombinedKernel",slots=c(kernels="list",kerneltype="character"))
.CombinedKernel.valid <- function(object) {
    validkernels <-  getValidKernels(FALSE)
    kernels <- object@kernels
    kernclass <- sapply(kernels,class)
    if (length(grep("list",kernclass)) != length(kernclass))
        return("slot kernel must only contain lists")
    kerncheck <- unlist(lapply(kernels,function(x) lapply(x,class)))
    if (prod(kerncheck %in% validkernels) == 0)
        stop("each list in slot kernel must only contain valid kernels")
    else
        return(TRUE)
}
setValidity("CombinedKernel", .CombinedKernel.valid)

containsStatisticalModelKernel <- function(x) {
    if (!inherits(x,"CombinedKernel")) {
        if (inherits(x,"StatisticalModelKernel"))
            return(TRUE)
    } else {
        kernels <- x@kernels
        kerncheck <- unlist(lapply(kernels,function(x) lapply(x,class)))
        if ("StatisticalModelKernel" %in% kerncheck)
            return(TRUE)
    }
    return(FALSE)
}

getValidKernels <- function(combined=TRUE) {
    validkernels <- c("BSplineKernel","GaussianKernel","IsoKernel","StatisticalModelKernel","MultiscaleBSplineKernel")
    if (combined)
        validkernels <- c(validkernels,"CombinedKernel")
    return(validkernels)
}

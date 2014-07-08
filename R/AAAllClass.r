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
    if (centerlen != dimbasis[1])
        return("dimension of meanvector and PC-Basis differ")
    else if (sdevlen != dimbasis[2])
        return("number of standarddeviations and number of Basisvectors differ"
               )
    else
        return(TRUE)
}
setValidity("pPCA", .pPCA.valid)



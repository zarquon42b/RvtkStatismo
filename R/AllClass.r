#' Documentation of class pPCA
#'
#' Documentation of class pPCA
#' 
#' The class contains the the follwing slots (still not yet set in stone)
#' \describe{
#' \item{PCA}{a list containing
#' \itemize{
#' \item{\code{sdev}: the square roots of the covariance matrix' eigenvalues}
#' \item{\code{rotation}: matrix containing the orthonormal PCBasis vectos}
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
         slots= c(PCA="list",scale="logical",representer="list",rawdata="matrix",sigma="numeric",Variance="data.frame"),
         prototype = list(PCA=list(sdev=0,rotation=0,x=0,center=0),scale=logical(),representer=list(),rawdata=matrix(0,0,0),sigma=numeric(0),Variance=data.frame())
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

setOldClass("mesh3d")

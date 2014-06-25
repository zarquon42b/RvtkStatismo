#' calculate or modify a probablistic PCA based on 3D-coordinates
#'
#' calculate or modify a probablistic PCA based on 3D-coordinates
#' 
#' @encoding utf8
#' @param array array of dimensions k x 3 x n, where k=number of coordinates and n=sample size.
#' @param align logical: if TRUE, the data will be aligned first
#' @param use.lm integer vector: specifies the indices of the points that are to be used in the constrained model
#' @param deselect logical: if TRUE, \code{use.lm} references the missing coordinates instead of the present ones.
#' @param sigma estimate of error variance (sensible is a value estimating coordinate error in terms of observer error)
#' @param exVar numeric value with \code{0 < exVar <= 1} specifying the PCs to be included by their cumulative explained Variance
#' @param scale logical: allow scaling in Procrustes fitting
#' @param fullfit logical: if FALSE only the non-missing points will be used for registration.
#' @param representer a triangular mesh, where the vertices correspond to the coordinates in \code{array}, leave NULL for pointclouds.
#' @param model object of class \code{pPCA}
#' @return returns a probabilistic PCA model of class "pPCA".
#' \code{setMod} is used to modify existing models by changing sigma and exVar.
#'
#' 
#' The class \code{"pPCA"} is a list containing the follwing items (still not yet set in stone)
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
#' 
#' 
#' @examples
#' require(Morpho)
#' data(boneData)
#' model <- pPCA(boneLM[,,])
#' ## change parameters without recomputing Procrustes fit
#' model1 <- setMod(model, sigma=1, exVar=0.8)
#'
#' 
#' @references
#' \enc{Lüthi}{Luethi} M, Albrecht T, Vetter T. 2009. Probabilistic modeling and visualization of the flexibility in morphable models. In: Mathematics of Surfaces XIII. Springer. p 251-264
#' 
#' @importFrom Morpho ProcGPA rotonmat arrMean3 vecx rotonto rotreverse
#' @importFrom Rvcg vcgUpdateNormals
#' @name pPCA
#' @rdname pPCA
#' @export
pPCA <- function(array, align=TRUE,use.lm=NULL,deselect=FALSE,sigma=NULL,exVar=1,scale=TRUE,representer=NULL) {
    if (align) {
        procMod <- rigidAlign(array,scale=scale,use.lm=use.lm,deselect=deselect)
    } else {
        procMod <- list(rotated=array)
    }
    procMod$mshape <- NULL
    rawdata <- vecx(procMod$rotated,byrow=T)
    PCA <- prcomp(rawdata,tol = sqrt(.Machine$double.eps)) ## calculate PCA
    PCA$scale <- NULL
    sds <- PCA$sdev^2
    good <- which(sds > 1e-13)
    sds <- sds[good] ## remove PCs with very little variability
    PCA$rotation <- PCA$rotation[,good,drop=FALSE]
    PCA$sdev <- PCA$sdev[good]
    PCA$x <- 0
    procMod$PCA <- PCA
    procMod$scale <- scale
    class(procMod) <- "pPCA"
    if (is.null(representer) || is.matrix(representer))
        representer <- list(vb=t(arrMean3(procMod$rotated)),it=matrix(0,0,0))
    procMod$representer <- representer
    procMod$rotated <- NULL
    procMod <- setMod(procMod,sigma=sigma,exVar=exVar)
    procMod$rawdata <- sweep(rawdata,2,colMeans(rawdata))
    return(procMod)

}


#' @rdname pPCA
#' @export
setMod <- function(procMod, sigma, exVar)UseMethod("setMod")

#' @rdname pPCA
#' @export
setMod.pPCA <- function(model,sigma=NULL,exVar=1) {
    k <- ncol(model$representer$vb)
    PCA <- model$PCA
    if (!is.null(model$sigma))
        sds <- calcSdev(model)^2
    else
        sds <- PCA$sdev^2
    sdsum <- sum(sds)
    sdVar <- sds/sdsum
    sdCum <- cumsum(sdVar)
    usePC <- which(sdCum <= exVar)
    if (is.null(sigma))
        sigma <- 1/(3*k)*sum(sds[-usePC]) ##estimate sigma from remaining Variance
    if (sigma >= sdsum) {
        warning(paste("sigma > overall variance set to",sdsum/2))
        sigma <- sdsum/2
    }
    sigest <- (sds - sigma)
    sigest <- sigest[which(sigest > 0)]
    usePC <- 1:max(1,min(length(usePC),length(sigest)))
    model$sigma <- sigma
    model$PCA$rotation <- PCA$rotation[,usePC,drop=FALSE]
    model$PCA$sdev <- sqrt(sigest[usePC])
    if (!is.null(model$rawdata))
        model$PCA$x <- model$rawdata%*%t(GetPCABasisMatrixIn(model))
    else
        model$PCA$x <- 0
    Variance <- createVarTable(sigest[usePC],square = FALSE) ##make Variance table 
    model$Variance <- Variance
                                        #print(model,Variance=FALSE)
    return(model)
}


#' @export
print.pPCA <- function(x, digits = getOption("digits"), Variance=TRUE,...){
    cat(paste("   sigma =",x$sigma,"\n"))
    cat(paste(" first",length(x$PCA$sdev),"PCs used\n"))
    if (Variance) {
        cat("\n\n Model Variance:\n")
        print(x$Variance)
    }
}
#' predict or restrict a mesh or matrix based on a statistical model
#'
#' predict or restrict a mesh or matrix based on a statistical model
#' @param x a matrix, a mesh3d or a vector (for pPCA models) containing standardized variables within the PC-space
#' @param model model of class \code{pPCA}
#' @param representer if TRUE and the model contains a representer mesh, a surface mesh will be returned, coordinate matrix otherwise.
#' @param origSpace logical: rotate the estimation back into the original coordinate system.
#' @param pPCA logical: if TRUE, a constrained pPCA model is returned.
#' "chisq" uses the Chi-Square distribution of the squared Mahalanobisdistance, while "dist" restricts the values to
#' be within a multi-dimensional sphere of radius \code{sdmax}. If FALSE the probability will be determined per PC separately.
#' @param use.lm optional: integer vector specifying row indices of the coordinates to use for rigid registration on the model's meanshape.
#' @param sdmax maximum allowed standard deviation (per Principal axis) within the model space. Defines the probabilistic boundaries.
#' @param mahaprob character: if != "none", use mahalanobis-distance to determine overall probability (of the shape projected into the model space.
#' @return \code{predictpPCA} returns a matrix/mesh3d restricted to the boundaries given by the modelspace.
#'
#' @name predictpPCA
#' @rdname predictpPCA
#' @export


#' @rdname predictpPCA
#' @export
predictpPCA <- function(x,model,representer=TRUE,...)UseMethod("predictpPCA")

#' @rdname predictpPCA
#' @export
predictpPCA.matrix <- function(x,model,representer=TRUE,origSpace=TRUE,use.lm=NULL,deselect=FALSE,sdmax,mahaprob=c("none","chisq","dist"),align=TRUE,...) {
    mahaprob <- substr(mahaprob[1],1L,1L)
    mshape <- getMeanMatrix(model,transpose=TRUE)
    if (align) {
    if (is.null(use.lm)) {
        rotsb <- rotonto(mshape,x,scale=model$scale,reflection = F)
        sb <- rotsb$yrot
    } else {
        use.lm <- unique(sort(use.lm))
        if (deselect)
            use.lm <- c(1:nrow(mshape))[-use.lm]
        rotsb <- rotonto(mshape[use.lm,],x[use.lm,],scale=model$scale,reflection=F)
        sb <- rotonmat(x,x[use.lm,],rotsb$yrot)
    }
} else
    sb <- x
    sbres <- sb-mshape
    alpha <- GetPCABasisMatrixIn(model)%*%as.vector(t(sbres))
    sdl <- length(model$PCA$sdev)
    
    if (!missing(sdmax)) {
        if (mahaprob != "n") {
            sdl <- length(model$PCA$sdev)
            probs <- sum(alpha^2)
            if (mahaprob == "c") {
                Mt <- qchisq(1-2*pnorm(sdmax,lower.tail=F),df=sdl)
                probs <- sum(alpha^2)
            } else if (mahaprob == "d") {
                Mt <- sdmax
                probs <- sqrt(probs)
            }
            if (probs > Mt ) {
                sca <- Mt/probs
                alpha <- alpha*sca
            }
            
        } else { 
            signalpha <- sign(alpha)
            alpha <- abs(alpha)
            outlier <- which(alpha > sdmax)
            alpha[outlier] <- sdmax
            alpha <- alpha*signalpha
        }
    }
    
    if ("coeffs" %in% names(list(...))) {
        return(alpha)
        
    } else {
        estim <- t(as.vector(GetPCABasisMatrix(model)%*%alpha)+t(mshape))
        if (origSpace)
            estim <- rotreverse(estim,rotsb)
        
        if (!is.null(model$representer) && class(model$representer) == "mesh3d" && representer) {
            estimmesh <- model$representer
            estimmesh$vb[1:3,] <- t(estim)
            estimmesh <- vcgUpdateNormals(estimmesh)
            estim <- estimmesh
        }
        return(estim)
    }
}

#' @rdname predictpPCA
#' @export
predictpPCA.mesh3d <- function(x,model,representer=TRUE,origSpace=TRUE,use.lm=NULL,deselect=FALSE,sdmax,mahaprob=c("none","chisq","dist"),align=TRUE,...) {
    mat <- t(x$vb[1:3,])
    estim <- predictpPCA(vert2points(x),model=model,align=align,representer=representer,sdmax=sdmax,origSpace=origSpace,use.lm=use.lm,deselect=deselect,mahaprob=mahaprob,...)
    return(estim)
}

#' @rdname predictpPCA
#' @export
predictpPCA.numeric <- function(x,model,representer=TRUE,...) {
    W <- GetPCABasisMatrix(model)
    useit <- 1:(min(length(x),length(model$PCA$sdev)))
    if (length(useit) > 1)
        estim <- as.vector(W[,useit]%*%x)
    else
        estim <- as.vector(W[,useit]*x)
    
    estim <- t(estim+getMeanMatrix(model,transpose=FALSE))
    if (!is.null(model$representer) && class(model$representer) == "mesh3d" && representer) {
        estimmesh <- model$representer
        estimmesh$vb[1:3,] <- t(estim)
        estimmesh <- vcgUpdateNormals(estimmesh)
        estim <- estimmesh
    }
    return(estim)
}

#' calculate probability/coefficients for a matrix/mesh given a statistical model
#'
#' calculate probability for a matrix/mesh given a statistical model
#' @param x matrix or mesh3d
#' @param model a model of class pPCA
#' @param align logical: if TRUE the data will be aligned to the model's mean
#' @param use.lm integer vector specifying row indices of the coordinates to use for rigid registration on the model's meanshape.
#' @return \code{getProb} returns a probability, while \code{getCoefficients} returns the (scaled) scores in the pPCA space.
#' @export
getDataLikelihood <- function(x,model,align=FALSE,use.lm) UseMethod("getDataLikelihood")

#' @rdname getDataLikelihood
#' @export
getDataLikelihood.matrix <- function(x,model,align=FALSE,use.lm=NULL) {
    mshape <- getMeanMatrix(model,transpose=TRUE)
    if (align) {
        if (is.null(use.lm)) {
            rotsb <- rotonto(mshape,x,scale=model$scale,reflection = F)
            sb <- rotsb$yrot
        } else {
            rotsb <- rotonto(mshape[use.lm,],x[use.lm,],scale=model$scale,reflection=F)
            sb <- rotonmat(x,x[use.lm,],rotsb$yrot)
        }
    } else {
        sb <- x
    }
    sbres <- sb-mshape
    alpha <- GetPCABasisMatrixIn(model)%*%as.vector(t(sbres))
    sdl <- length(model$PCA$sdev)
    probs <- sum(alpha^2)
    probout <- pchisq(probs,lower.tail = F,df=sdl)
    return(probout)
}

#' @rdname getDataLikelihood
#' @export
getDataLikelihood.mesh3d <- function(x,model,align=FALSE,use.lm=NULL) {
    x <- vert2points(x)
    out <- getProb(x,model=model,align=align,use.lm=use.lm)
    return(out)
}

#' @rdname getDataLikelihood
#' @export
getCoefficients <- function(x, model,align=TRUE, use.lm=NULL) {
    out <- predictpPCA(x,model,use.lm,coeffs=NULL,align=align)
    return(out)
}

#' get per coordinate variance from a statistical model
#'
#' get per coordinate variance from a statistical model
#'
#' @encoding utf8
#' @param model object of class pPCA
#' @note calculates the per-coordinate variance as described in Luethi(2009)
#' @references \enc{Lüthi}{Luethi} M, Albrecht T, Vetter T. 2009. Probabilistic modeling and visualization of the flexibility in morphable models. In: Mathematics of Surfaces XIII. Springer. p 251-264
#' @export
getCoordVar <- function(model) {
    if (!inherits(model,"pPCA"))
        stop("please provide model of class pPCA")
    W <- GetPCABasisMatrix(model)
    m <- ncol(model$representer$vb)
    cov0 <- apply((W*W),1,function(x) sum(x))
    mat <- matrix(cov0,nrow=(length(cov0)/m),m,byrow = T)
    cov0 <- apply(mat,1,function(x) x <- sqrt(sum(x^2)))
    return(cov0)
}


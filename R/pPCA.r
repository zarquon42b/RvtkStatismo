#' calculate or modify a probablistic PCA based on 3D-coordinates
#'
#' calculate or modify a probablistic PCA based on 3D-coordinates
#' 
#' @encoding utf8
#' @param array array of dimensions k x 3 x n, where k=number of coordinates and n=sample size.
#' @param align logical: if TRUE, the data will be aligned first
#' @param missingIndex integer vector: specifies which points are missing in the constrained model
#' @param deselect logical: if TRUE, missingIndex references the existing coordinates instead of the missing ones.
#' @param procMod object of class "procMod" as returned by pPCAconstr or setMod
#' @param sigma estimate of error variance (sensible is a value estimating coordinate error in terms of observer error)
#' @param exVar numeric value with \code{0 < exVar <= 1} specifying the PCs to be included by their cumulative explained Variance
#' @param representer a triangular mesh, where the vertices correspond to the coordinates in \code{array}
#' @param scale logical: allow scaling in Procrustes fitting
#' @param fullfit logical: if FALSE only the non-missing points will be used for registration.
#' @param model probabilistic model of class "pPCA" or "pPCAconstr"
#' @return \code{pPCA} and \code{pPCAconstr} return a probabilistic PCA model of class "pPCA" or "pPCAconstr" respectively. 
#' \code{predictPCA} and \code{predictPCAcond} select the most probable shape within a given model (within defined boundaries),
#' \code{setMod} is used to modify existing models by changing sigma and exVar.
#' @examples
#' require(Morpho)
#' data(boneData)
#' model <- pPCAconstr(boneLM[,,-1],missingIndex=3:4)
#' ## change parameters without recomputing Procrustes fit
#' model1 <- setMod(model, sigma=1, exVar=0.8)
#' 
#'
#' @references
#' \enc{Lüthi}{Luethi} M, Albrecht T, Vetter T. 2009. Probabilistic modeling and visualization of the flexibility in morphable models. In: Mathematics of Surfaces XIII. Springer. p 251-264
#' 
#' @importFrom Morpho ProcGPA rotonmat arrMean3 vecx
#' @importFrom Rvcg vcgUpdateNormals
#' @rdname pPCA
#' @export
pPCA <- function(array, align=TRUE,sigma=NULL,exVar=1,scale=TRUE,representer=NULL) {
    k <- dim(array)[1]
    if (align)
        procMod <- ProcGPA(array,scale=scale,CSinit=F,reflection=F,silent = T) ##register all data using Procrustes fitting
    else
        procMod <- list(rotated=array,mshape=arrMean3(array))

    rawdata <- vecx(procMod$rotated,byrow=T)
    procMod$rawdata <- sweep(rawdata,2,colMeans(rawdata))
    PCA <- prcomp(rawdata,tol = sqrt(.Machine$double.eps)) ## calculate PCA
    sds <- PCA$sdev^2
    good <- which(sds > 1e-13)
    sds <- sds[good] ## remove PCs with very little variability
    PCA$rotation <- PCA$rotation[,good]
    PCA$sdev <- PCA$sdev[good]
    PCA$x <- PCA$x[,good]
    procMod$PCA <- PCA
    procMod$scale <- scale
    class(procMod) <- "pPCA"
    if (is.null(representer))
        representer <- list(vb=t(procMod$mshape),it=matrix(0,0,0))
    procMod$representer <- representer
    procMod <- setMod(procMod,sigma=sigma,exVar=exVar)
    return(procMod)

}


#' @rdname pPCA
#' @export
pPCAconstr <- function(array,align=TRUE, missingIndex,deselect=FALSE,sigma=NULL, exVar=1,representer=NULL,scale=TRUE,fullfit=FALSE) {

    k <- dim(array)[1]
    if (deselect) {
        missingIndex <- c(1:k)[-missingIndex]
    }
    use.lm=c(1:k)[-missingIndex]
    if (align) {
        if (!fullfit) {
            procMod <- ProcGPA(array[use.lm,,],scale=scale,CSinit=F,reflection=F,silent = TRUE)##register all data using Procrustes fitting based on the non-missing coordinates
            tmp <- array
            a.list <-  1:(dim(array)[3])
            tmp <- lapply(a.list, function(i) {mat <- rotonmat(array[,,i],array[use.lm,,i],procMod$rotated[,,i],scale=scale,reflection = F);return(mat)})
            tmp1 <- array
            for (i in 1:length(a.list))
                tmp1[,,i] <- tmp[[i]]
            procMod$rotated <- tmp1
            procMod$mshape <- arrMean3(tmp1)
        } else {
            procMod <- ProcGPA(array,scale=scale,CSinit = F,reflection = F,silent = T)
        }
    } else {
        procMod <- list(rotated=array,mshape=arrMean3(array))
    }
    procMod <- pPCA(procMod$rotated,align = T,sigma=sigma, exVar=exVar,scale=scale,representer = representer)
    
                                        #procMod$use.lm <- use.lm
    procMod$scale <- scale
    procMod$missingIndex <- missingIndex
    class(procMod) <- "pPCAconstr"
    procMod <- setMod(procMod,sigma=sigma,exVar=exVar)
    if (is.null(representer))
        representer <- list(vb=t(procMod$mshape),it=matrix(0,0,0))
    procMod$representer <- representer
    
    return(procMod)
}
#' @rdname pPCA
#' @export
setMod <- function(procMod, sigma, exVar)UseMethod("setMod")

#' @rdname pPCA
#' @export
setMod.pPCA <- function(procMod,sigma=NULL,exVar=1) {
    k <- dim(procMod$mshape)[1]
    PCA <- procMod$PCA
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
    if (sigma == 0)
        siginv <- 1e13
    else
        siginv <- 1/sigma

    sigest <- (sds - sigma)
    sigest <- sigest[which(sigest > 0)]
    usePC <- 1:max(1,min(length(usePC),length(sigest)))
    procMod$exVar <- sdCum[max(usePC)]##calculate variance explained by that model compared to that of the training sample
    procMod$sigma <- sigma
    W <- t(t(PCA$rotation[,usePC])*sqrt(sigest[usePC])) ##Matrix to project scaled PC-scores back into the config space
    Win <- (t(PCA$rotation[,usePC])*(1/sqrt(sigest)[usePC])) ##Matrix to project from config space into the scaled PC-space
    ##Matrix to project from config space into the scaled PC-space
    procMod$W <- W
    procMod$Win <- Win
    procMod$PCA$rotation <- PCA$rotation[,usePC]
    procMod$PCA$sdev <- sqrt(sigest[usePC])
    procMod$PCA$x <- procMod$rawdata%*%t(procMod$Win)
    sds <- sigest[usePC]
    sdsum <- sum(sds)
    sdVar <- sds/sdsum
    sdCum <- cumsum(sdVar)
    Variance <- data.frame(eigenvalue=sds,exVar=sdVar, cumVar=sdCum) ##make Variance table 
    procMod$Variance <- Variance
                                        #print(procMod,Variance=FALSE)
    return(procMod)
}

#' @rdname pPCA
#' @export
setMod.pPCAconstr <- function(procMod,sigma=NULL,exVar=1) {
    k <- dim(procMod$mshape)[1]
    PCA <- procMod$PCA
    sds <- PCA$sdev^2
    sel <- getSel(procMod$missingIndex,procMod$mshape)
    sigma <- procMod$sigma
    if (sigma == 0)
        siginv <- 1e13
    else
        siginv <- 1/sigma
    
    W <- procMod$W
    ## get constrained space
    Wb <- W[-sel,]
    WbtWb <- crossprod(Wb)
    M <- siginv*WbtWb
    diag(M) <- diag(M)+1
    ##Matrix to project from config space into the scaled PC-space
    procMod$Win <- (t(PCA$rotation[,])*(1/sqrt(sds)[])) ##Matrix to project from config space into the scaled PC-space
    procMod$Wb <- Wb
    procMod$WbtWb <- WbtWb
    procMod$M <- M
    stry <- try(Minv <- solve(M)) 
    if (inherits(stry,"try-error")) {
        Minv <- Morpho:::armaGinv(M)
        message("singular Matrix")
    }
    Minv <- (Minv+t(Minv))/2
    procMod$Minv <- Minv ## covariance structure of the alpha under the restriction based on non-missing data.
    procMod$sigma <- sigma
    procMod$alphamean <- siginv*procMod$Minv%*%t(Wb) ## the general mean of the constrained distribution
    print(procMod,Variance=FALSE)
    return(procMod)
}

#' @export
print.pPCAconstr <- function(x, digits = getOption("digits"), Variance=TRUE,...){
    cat(paste("   sigma =",x$sigma,"\n"))
    cat(paste("   exVar =",x$exVar,"\n\n"))
    cat(paste(" first",ncol(x$W),"PCs used\n"))
    if (Variance) {
        cat("\n\n Model Variance:\n")
        print(x$Variance)
    }
}
#' @export
print.pPCA <- function(x, digits = getOption("digits"), Variance=TRUE,...){
    cat(paste("   sigma =",x$sigma,"\n"))
    cat(paste("   exVar =",x$exVar,"\n\n"))
    cat(paste(" first",ncol(x$W),"PCs used\n"))
    if (Variance) {
        cat("\n\n Model Variance:\n")
        print(x$Variance)
    }
}
#' predict or restrict a mesh or matrix based on a statistical model
#'
#' predict or restrict a mesh or matrix based on a statistical model
#' @param x a matrix, a mesh3d or a vector (for pPCA models) containing standardized variables within the PC-space
#' @param model model of class pPCA or pPCAconstr
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
#' \code{predictpPCAconstr} returns a list with
#' \item{estim}{matrix/mesh3d representing the mean of the restricted space}
#'
#' \item{pPCA}{if \code{pPCA = TRUE} a pPCA model representing the gaussian subspace given the constraints is returned}
#' \item{rot}{the transformation of x into the modelspace that can be reverted by calling \code{rotreverse} from the package Morpho} 

#' @rdname predictpPCA
#' @export
predictpPCAconstr <- function(x, model, representer, origSpace=TRUE, pPCA=FALSE,...) UseMethod("predictpPCAconstr")

#' @rdname predictpPCA
#' @export
predictpPCAconstr.matrix <- function(x, model,representer=FALSE,origSpace=TRUE,pPCA=FALSE,...) {
    mshape <- model$mshape
    missingIndex <- model$missingIndex
    use.lm <- getUseLM(missingIndex,mshape)
    rotsb <- rotonto(mshape[use.lm,],x,scale=model$scale,reflection = F)
    sb <- rotsb$yrot
    sbres <- sb-mshape[use.lm,]
    alpha <- model$alphamean%*%as.vector(t(sbres))
    
    ##as.vector(W[,good]%*%alpha)
    estim <- t(as.vector(model$W%*%alpha)+t(mshape))
    if (pPCA)
        procMod <- as.pPCA(model,estim)

    if (origSpace)
        estim <- rotreverse(estim,rotsb)
    
    if (!is.null(model$representer) && class(model$representer) == "mesh3d" && representer) {
        estimmesh <- model$representer
        estimmesh$vb[1:3,] <- t(estim)
        estimmesh <- vcgUpdateNormals(estimmesh)
        estim <- estimmesh
    }
    if (pPCA)
        return(list(estim=estim,pPCA=procMod,rot=rotsb))
    else
        return(estim)
}

#' @rdname predictpPCA
#' @export
predictpPCAconstr.mesh3d <- function(x,model,representer=FALSE, sdmax, origSpace=TRUE,pPCA=FALSE,...) {
    mat <- t(x$vb[1:3,])
    estim <- predictpPCAconstr(x=mat,model=model,representer=representer,sdmax=sdmax,origSpace=origSpace)
    return(estim)
}


#' @rdname predictpPCA
#' @export
predictpPCA <- function(x,model,representer=FALSE,...)UseMethod("predictpPCA")

#' @rdname predictpPCA
#' @export
predictpPCA.matrix <- function(x,model,representer=FALSE,origSpace=TRUE,use.lm=NULL,sdmax,mahaprob=c("none","chisq","dist"),...) {
    mahaprob <- substr(mahaprob[1],1L,1L)
    mshape <- model$mshape
    if (is.null(use.lm)) {
        rotsb <- rotonto(mshape,x,scale=model$scale,reflection = F)
        sb <- rotsb$yrot
    } else {
        rotsb <- rotonto(mshape[use.lm,],x[use.lm,],scale=model$scale,reflection=F)
        sb <- rotonmat(x,x[use.lm,],rotsb$yrot)
    }
    sbres <- sb-mshape
                                        # W <- model$W
    alpha <- model$Win%*%as.vector(t(sbres))
    sdl <- nrow(model$Win)
    
    if (!missing(sdmax)) {
        if (mahaprob != "n") {
            sdl <- nrow(model$W)
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
        estim <- t(as.vector(model$W%*%alpha)+t(mshape))
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
predictpPCA.mesh3d <- function(x,model,representer=FALSE,origSpace=TRUE,use.lm=NULL,sdmax,mahaprob=c("none","chisq","dist"),...) {
    mat <- t(x$vb[1:3,])
    estim <- predictpPCA(vert2points(x),model=model,representer=representer,sdmax=sdmax,origSpace=origSpace,use.lm=use.lm,mahaprob=mahaprob,...)
    return(estim)
}

#' @rdname predictpPCA
#' @export
predictpPCA.numeric <- function(x,model,representer=FALSE,...) {
    W <- model$W
    useit <- 1:(min(length(x),ncol(W)))
    if (length(useit) > 1)
        estim <- as.vector(W[,useit]%*%x)
    else
        estim <- as.vector(W[,useit]*x)
    
    estim <- t(estim+t(model$mshape))
    if (!is.null(model$representer) && class(model$representer) == "mesh3d" && representer) {
        estimmesh <- model$representer
        estimmesh$vb[1:3,] <- t(estim)
        estimmesh <- vcgUpdateNormals(estimmesh)
        estim <- estimmesh
    }
    return(estim)
}

#' @export
as.pPCA <- function(x,..)UseMethod("as.pPCA")

#' @export
as.pPCA.pPCAconstr <- function(x, newMean,...) { #convert a pPCAconstr to a pPCA by adding a new mean config # not to be called directly
    procMod <- x
    procMod$mshape <- newMean
    sds <- procMod$PCA$sdev^2
    Minv <- procMod$Minv
    udut <- t(t(Minv)*sds)
    eigM <- eigen(udut,symmetric = T)
    sds <- Re(eigM$values)
    good <- which(sds > 1e-15)
    sds[-good] <- 0
    procMod$PCA$sdev <- sqrt(sds)
    newW <- procMod$PCA$rotation[,]%*%(Re(eigM$vectors[,]))[,good]
    sds <- sds[good]
    procMod$W <- t(t(newW)*sqrt(sds))
    procMod$Win <- t(newW)/sqrt(sds)
    procMod$PCA$rotation <- newW
    procMod$PCA$center <- as.vector(t(newMean))
    procMod$PCA$x <- 0
    class(procMod) <- "pPCA"
    allNames <- names(procMod)
    rem <- which(allNames %in% c("M","Minv","Wb","WbtWb","missingIndex","alphamean"))
    procMod[rem] <- NULL
    sdsum <- sum(sds)
    sdVar <- sds/sdsum
    sdCum <- cumsum(sdVar)
    Variance <- data.frame(eigenvalues=sds,exVar=sdVar, cumVar=sdCum)
    procMod$Variance <- Variance
    return(procMod)
}

#' @export
as.pPCAconstr <- function(x, missingIndex,...)UseMethod("as.pPCAconstr")#convert a pPCA to a pPCAconstr by providing constrains # not to be called directly

#' @export
as.pPCAconstr.pPCA <- function(x,missingIndex,deselect=FALSE) {
    procMod <- x
    procMod$missingIndex <- missingIndex
    class(procMod) <- "pPCAconstr"
    procMod <- setMod(procMod,sigma=procMod$sigma,exVar=procMod$exVar)
    return(procMod)
}

#' calculate probability/coefficients for a matrix/mesh given a statistical model
#'
#' calculate probability for a matrix/mesh given a statistical model
#' @param x matrix or mesh3d
#' @param model a model of class pPCA
#' @param use.lm integer vector specifying row indices of the coordinates to use for rigid registration on the model's meanshape.
#' @return \code{getProb} returns a probability, while \code{getCoefficients} returns the (scaled) scores in the pPCA space.
#' @export
getProb <- function(x,model,use.lm) UseMethod("getProb")

#' @rdname getProb
#' @export
getProb.matrix <- function(x,model,use.lm=NULL) {
    mshape <- model$mshape
    if (is.null(use.lm)) {
        rotsb <- rotonto(mshape,x,scale=model$scale,reflection = F)
        sb <- rotsb$yrot
    } else {
        rotsb <- rotonto(mshape[use.lm,],x[use.lm,],scale=model$scale,reflection=F)
        sb <- rotonmat(x,x[use.lm,],rotsb$yrot)
    }
    sbres <- sb-mshape
                                        # W <- model$W
    alpha <- model$Win%*%as.vector(t(sbres))
    sdl <- nrow(model$Win)
    probs <- sum(alpha^2)
    probout <- pchisq(probs,lower.tail = F,df=sdl)
    return(probout)
}

#' @rdname getProb
#' @export
getProb.mesh3d <- function(x,model,use.lm=NULL) {
    x <- vert2points(x)
    out <- getProb(x,model=model,use.lm=use.lm)
    return(out)
}

#' @rdname getProb
#' @export
getCoefficients <- function(x, model,use.lm=NULL) {
    out <- predictpPCA(x,model,use.lm,coeffs=NULL)
    return(out)
}

#' get per coordinate variance from a statistical model
#'
#' get per coordinate variance from a statistical model
#'
#' @param model object of class pPCA
#' @export
getCoordVar <- function(model) {
    if (!inherits(model,"pPCA"))
        stop("please provide model of class pPCA")
    W <- model$W
    m <- ncol(model$mshape)
    cov0 <- apply((W*W),1,function(x) sum(x))
    mat <- matrix(cov0,nrow=(length(cov0)/m),m,byrow = T)
    cov0 <- apply(mat,1,function(x) x <- sqrt(sum(x^2)))
    return(cov0)
}


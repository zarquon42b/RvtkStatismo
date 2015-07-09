
ComputeConstrainedModel <- function(x,model,align=FALSE,use.lm,deselect=FALSE,origSpace=FALSE) {
    mshape <- getMeanMatrix(model,transpose=TRUE)
    k <- ncol(model@representer$vb)
    if (missing(use.lm))
        use.lm <- 1:nrow(x)
    if (deselect)
        use.lm <- c(1:nrow(mshape))[-use.lm]
    if (align) {
        rotsb <- rotonto(mshape[use.lm,],x,scale=model@scale,reflection = F)
        sb <- rotsb$yrot
    }  else {
        sb <- x
    }
     if (!deselect) {
            oldsort <- cbind(use.lm,1:nrow(x))
            oldsort <- oldsort[order(oldsort[,1]),]
            sb <- sb[oldsort[,2],]
        }
    sbres <- sb-mshape[sort(use.lm),]
            
            
    subspace <- getSubCov(model,use.lm = use.lm,deselect = F)
    out <- new("pPCA")
    #out <- list(PCA=subspace$PCA)
    alpha <- subspace$alphamean%*%as.vector(t(sbres))
    estim <- t(as.vector(GetPCABasisMatrix(model)%*%alpha)+t(mshape))
    subspace$PCA$x <- 0
    subspace$PCA$center <- as.vector(t(estim))
    SetPCA(out) <- subspace$PCA
    if (align && origSpace)    
        estim <- rotreverse(estim,rotsb)
    SetScale(out) <- model@scale
    SetNoiseVariance(out) <- model@sigma
    out@representer <- model@representer
    out@representer$vb[1:3,] <- t(estim)
    return(out)
}

### get matrices nessesary to calculate a model of the constrained space
getSubCov <- function(model,use.lm,deselect=FALSE) {
    use.lm <- unique(sort(use.lm))
    k <- ncol(model@representer$vb)
    if (!deselect) {
        missingIndex <- c(1:k)[-use.lm]
    } else {
        missingIndex <- use.lm
    }
    if (model@sigma == 0)
        siginv <- 1e13
    else
        siginv <- 1/model@sigma
    sel <- getSel(missingIndex,getMeanMatrix(model))
    W <- GetPCABasisMatrix(model)
    out <- list()
    ## get constrained space
    Wb <- W[-sel,]
    WbtWb <- crossprod(Wb)
    M <- siginv*WbtWb
    diag(M) <- diag(M)+1
        stry <- try(Minv <- solve(M)) 
    if (inherits(stry,"try-error")) {
        Minv <- Morpho::armaGinv(M)
        message("singular Matrix")
    }
    Minv <- (Minv+t(Minv))/2
    out$alphamean <- siginv*Minv%*%t(Wb) ## the general mean of the constrained distribution
    sds <- model@PCA$sdev^2
    udut <- t(t(Minv)*sds)
    eigM <- eigen(udut,symmetric = T)
    sds <- Re(eigM$values)
    good <- which(sds > 1e-15)
    sds[-good] <- 0
    newW <- model@PCA$rotation%*%(Re(eigM$vectors))[,good,drop=FALSE]
    sds <- sds[good]
    out$PCA <- list()
    out$PCA$sdev <- sqrt(sds)
    out$PCA$rotation <- newW
    out$PCA$x <- 0
    return(out)
}

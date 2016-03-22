#' Low level methods to set pPCA class content
#'
#' Low level methods to set pPCA class content - not to be invoked directly
#'
#' @param x of class "pPCA"
#' @param value set the specific value
#' @return returns an updated pPCA object
#' @name Set-pPCA-class
#' @rdname ppcasetters
#' @docType methods
#' @keywords internals
NULL

#' @rdname ppcasetters
#' @export
setGeneric("SetNoiseVariance<-", function(x, value) standardGeneric("SetNoiseVariance<-"))

#' @rdname ppcasetters
setReplaceMethod("SetNoiseVariance", "pPCA",function(x, value) {
                     x@sigma <- value;
                     tmp <- x@modelinfo@paraminfo
                     nn <- lapply(tmp,function(i) i <- i[1])
                     nvind <- grep("NoiseVariance",unlist(nn))
                     if (length(nvind))
                         tmp[[nvind]][2] <- as.character(value)
                     else
                         tmp <- append(list(c("NoiseVariance",as.character(value))),tmp)
                     x@modelinfo@paraminfo <- tmp
                     
                     validObject(x); x
                 })



#' @rdname ppcasetters
#' @export
setGeneric("SetPCBasisMatrix<-", function(x, value) standardGeneric("SetPCBasisMatrix<-"))

#' @rdname ppcasetters
setReplaceMethod("SetPCBasisMatrix", "pPCA",function(x, value) {x@PCA$rotation <- value; validObject(x); x})

#' @rdname ppcasetters
setReplaceMethod("SetPCBasisMatrix", "pPCA_pointer",function(x, value) {
    x <- pointer2pPCA(x)
    SetPCBasisMatrix(x) <- value
    validObject(x);
    x <- pPCA2pointer(x)
    x})

#' @rdname ppcasetters
#' @export
setGeneric("SetPCsdev<-", function(x, value) standardGeneric("SetPCsdev<-"))

#' @rdname ppcasetters
setReplaceMethod("SetPCsdev", "pPCA",function(x, value) {x@PCA$sdev <- value; validObject(x); x})

#' @rdname ppcasetters
setReplaceMethod("SetPCsdev", "pPCA_pointer",function(x, value) {
    x <- pointer2pPCA(x)
    SetPCsdev(x) <- value
    x <- pPCA2pointer(x)
    x
})

#' @rdname ppcasetters
#' @export
setGeneric("SetMeanVector<-", function(x, value) standardGeneric("SetMeanVector<-"))

#' @rdname ppcasetters
setReplaceMethod("SetMeanVector", "pPCA",function(x, value) {x@PCA$center <- value; validObject(x); x})

#' @rdname ppcasetters
#' @export
setGeneric("SetScores<-", function(x, value) standardGeneric("SetScores<-"))

#' @rdname ppcasetters
setReplaceMethod("SetScores", "pPCA",function(x, value) {
    varlen <- length(x@PCA$sdev)
    if (!is.matrix(value))
        stop(paste("scores must be a matrix with", varlen,"or zero (to remove the scores) columns "))
    else if (sum(dim(value) > 0)) {
        varlen <- length(x@PCA$sdev)
        if (ncol(value) != varlen)
            stop(paste("scores must be a matrix with", varlen,"or zero (to remove the scores) columns"))
    }      
    x@PCA$x <- value; validObject(x); x})


#' @rdname ppcasetters
setReplaceMethod("SetScores", "pPCA_pointer",function(x, value) {
    tmp <- pointer2pPCA(x)
    SetScores(tmp) <- value
    return(pPCA2pointer(tmp))
})

#' @rdname ppcasetters
#' @export
setGeneric("SetRepresenter<-", function(x, value) standardGeneric("SetRepresenter<-"))

#' @rdname ppcasetters
setReplaceMethod("SetRepresenter", "pPCA",function(x, value) {x@representer <- dataset2representer(value); validObject(x); x})

#' @rdname ppcasetters
setReplaceMethod("SetRepresenter", "pPCA_pointer",function(x, value) {
    x <- pointer2pPCA(x)
    SetRepresenter(x) <- value
    x <- pPCA2pointer(x)
    x})


#' @rdname ppcasetters
#' @export
setGeneric("SetScale<-", function(x, value) standardGeneric("SetScale<-"))

#' @rdname ppcasetters
setReplaceMethod("SetScale", "pPCA",function(x, value) {
    x@scale <- value;
    chk <- pairNameCheck(x@modelinfo@paraminfo,"scale")
    if (!chk) {
        x <- AddModelInfoParams(x,c("scale",tolower(as.character(value))))
    } else {
        x@modelinfo@paraminfo <- x@modelinfo@paraminfo[-chk]
        x <- AddModelInfoParams(x,c("scale",tolower(as.character(value))))
    }
        
    validObject(x); x})

#' @rdname ppcasetters
setReplaceMethod("SetScale", "pPCA_pointer",function(x, value) {
    x <- pointer2pPCA(x)
    SetScale(x) <- value
    pPCA2pointer(x)
    x})
#' @rdname ppcasetters
#' @export
setGeneric("SetPCA<-", function(x, value) standardGeneric("SetPCA<-"))

#' @rdname ppcasetters
setReplaceMethod("SetPCA", "pPCA",function(x, value) {x@PCA <- value; validObject(x); x})

#' @rdname ppcasetters
#' @export
setGeneric("SetVariance<-", function(x, value) standardGeneric("SetVariance<-"))

#' @rdname ppcasetters
setReplaceMethod("SetVariance", "pPCA",function(x, value) {x@Variance <- value; validObject(x); x})

#' @rdname modelinfo-class
#' @export
setGeneric("AddModelInfoParams", function(x, value) standardGeneric("AddModelInfoParams"))
#' @rdname modelinfo-class
setMethod("AddModelInfoParams", signature("modelinfo"),function(x, value) {
    if (!is.list(value))
        value <- list(value)
    x@paraminfo <-append(x@paraminfo,value); validObject(x);
    return(x)
})

#' @rdname ppcasetters
setMethod("AddModelInfoParams", signature("pPCA"),function(x, value) {
    x@modelinfo <- AddModelInfoParams(x@modelinfo,value);validObject(x)
    return(x)
    
})
#' @rdname ppcasetters
setMethod("AddModelInfoParams", signature("pPCA_pointer"),function(x, value) {
    x <- pointer2pPCA(x)
    AddModelInfoParams(x) <- value
    x <- pPCA2pointer(x)
    x})
    

#' @rdname modelinfo-class
#' @export
setGeneric("SetModelInfoParams<-", function(x, value) standardGeneric("SetModelInfoParams<-"))

#' @rdname modelinfo-class
setReplaceMethod("SetModelInfoParams", signature("modelinfo"),function(x, value) {
    x@paraminfo <-value; validObject(x); x
})
#' @rdname ppcasetters
setMethod("SetModelInfoParams<-", signature("pPCA"),function(x, value) {
              SetModelInfoParams(x@modelinfo) <- value; validObject(x)
              return(x)
    
})
#' @rdname ppcasetters
setMethod("SetModelInfoParams<-", signature("pPCA_pointer"),function(x, value) {
    x <- pointer2pPCA(x)
    SetModelInfoParams(x) <- value
    x <- pPCA2pointer(x)
    x})

#' @rdname modelinfo-class
#' @export
setGeneric("SetModelDataInfo<-", function(x, value) standardGeneric("SetModelDataInfo<-"))
#' @rdname modelinfo-class
setReplaceMethod("SetModelDataInfo", signature("modelinfo"),function(x, value) {
                     x@datainfo <-value; validObject(x); x
})

#' @rdname ppcasetters
setMethod("SetModelDataInfo<-", signature("pPCA"),function(x, value) {
              SetModelDataInfo(x@modelinfo) <- value; validObject(x)
              return(x)
    
})

#' @rdname ppcasetters
setMethod("SetModelDataInfo<-", signature("pPCA_pointer"),function(x, value) {
    x <- pointer2pPCA(x)
    SetModelDataInfo(x) <- value
    x <- pPCA2pointer(x)
    x})

pairNameCheck <- function(x,value) {
    full <- unlist(x)
    full <- full[ (1:length(full)) %% 2 != 0]
    chk <- duplicated(c(value,full))
    if (sum(chk > 0))
        return(which(chk)-1)
    else
        return(FALSE)
}
    

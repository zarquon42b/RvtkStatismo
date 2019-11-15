#' Reduce an existing statistical shape model
#'
#' Reduce an existing statistical shape model either to first n PCs or by explained Variance
#'
#' @param model statistical shape model of class 'pPCA'
#' @param exVar restricts model by explained variance - with \code{0 < exVar < 1}
#' @param npc number of PCs retained in the model (overrides \code{exVar})
#' @param scores logical: if TRUE, the scores for the reduced model will be returned.
#' @param pointer logical: if TRUE an object of class 'pPCA_pointer' is returned
#' @return returns a reduced model
#' @examples
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' mymod <- statismoBuildModel(align,representer=align[,,1],sigma=2,scale=TRUE)
#' reducemod <- statismoReducedVariance(mymod,0.5)
#' @rdname statismoReducedVariance
#' @export
setGeneric("statismoReducedVariance", function(model,exVar=1,npc=0,scores=TRUE,pointer=FALSE){
    standardGeneric("statismoReducedVariance")})


#' @rdname statismoReducedVariance
setMethod("statismoReducedVariance", signature(model="pPCA"), function(model, exVar=1,npc=0,scores=TRUE,pointer=FALSE) {
    modVar <- GetNumberOfPrincipalComponents(model)
    npc <- min(npc,modVar)
    out <- .Call("ReducedModel",model,npc,exVar,pointer)
    if (!scores)
        SetScores(out) <- matrix(0,0,0)
    return(out)
})
    
#' @rdname statismoReducedVariance
setMethod("statismoReducedVariance", signature(model="pPCA_pointer"), function(model, exVar=1,npc=0,scores=TRUE,pointer=TRUE) {
    modVar <- GetNumberOfPrincipalComponents(model)
    npc <- min(npc,modVar)
    out <- .Call("ReducedModel",model,npc,exVar,pointer)
    if (!scores)
        SetScores(out) <- matrix(0,0,0)
    return(out)
})

#' Reduce an existing statistical shape model
#'
#' Reduce an existing statistical shape model either to first n PCs or by explained Variance
#'
#' @param model
#' @param exVar restricts model by explained variance - with \code{0 < exVar < 1}
#' @param npc number of PCs retained in the model (overrides \code{exVar})
#' @examples
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' mymod <- statismoBuildModel(align,representer=align[,,1],sigma=2,scale=TRUE)
#' reducemod <- statismoReducedVariance(mymod,0.5)
#' @rdname statismoReducedVariance
#' @export
setGeneric("statismoReducedVariance", function(model,exVar=1,npc=0,scores=TRUE){
    standardGeneric("statismoReducedVariance")})


#' @rdname statismoReducedVariance
setMethod("statismoReducedVariance", signature(model="pPCA"), function(model, exVar=1,npc=0) {
    modVar <- GetPCAVarianceVector(model)
    npc <- min(npc,length(modVar))
    out <- .Call("ReducedModel",model,npc,exVar)
    return(out)
})
    

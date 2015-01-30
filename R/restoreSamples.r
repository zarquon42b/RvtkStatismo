#' restores original samples from a model with stored PC-scores
#'
#' restores original samples (meshes or pointclouds) from a model with stored PC-scores
#'
#' @param model shape model model of class "pPCA"
#' @return returns a list of restored shapes
#' @note the exact original shapes are only returned in case the model's assumed noise variance sigma = 0 (as eigenvalues might be discarded.
#' @examples
#' require(Morpho)
#' data(boneData)
#' align <- rigidAlign(boneLM)$rotated
#' mymod <- statismoBuildModel(align,representer=align[,,1],scale=FALSE)
#' ##now restore shapes
#' rest <- restoreSamples(mymod)
#' \dontrun{
#' ##visualize (non-existing) differences between restored and original shape
#' deformGrid3d(rest[[1]],align[,,1])
#' }
#' @export
restoreSamples <- function(model) {
    out <- list()
    scores <- GetPCScores(model,FALSE)    
    for (i in 1:nrow(scores))
        out[[i]] <- DrawSample(model,coefficients=scores[i,])
    names(out) <- rownames(scores)
    return(out)
}

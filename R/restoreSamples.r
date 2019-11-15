#' restores original samples from a model with stored PC-scores
#'
#' restores original samples (meshes or pointclouds) from a model with stored PC-scores
#'
#' @param model shape model model of class "pPCA"
#' @param n only use the first n PCscores
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
restoreSamples <- function(model,n=NULL) {
    out <- list()
    scores <- GetPCScores(model,TRUE)
    if (is.null(n))
        n <- 1:nrow(scores)
    else {
        if (max(n) > nrow(scores))
            stop(paste0("n must be in the range of 1:",nrow(scores)))
    }
    for (i in n)
        out[[i]] <- DrawSample(model,coefficients=scores[i,])
    names(out) <- rownames(scores)
    return(out)
}

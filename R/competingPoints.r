#' select the most probable points based on a statistical model
#'
#' select the most probable points based on a statistical model, using the mahalanobisdistance
#' @param model
#' @param sample k x 3 matrix of coordinates
#' @param index integer vector of lenght \code{k} assigning the corresponding index of the model's coordinates to each row of \code{sample}
#' @param mahlanobis logical: if FALSE, Euclideandistance is used.
#' @return
#' \item{mahadistance}{vector containing the mahalanobisdistances of all tested coordinates}
#' \item{goodverts}{the coordinates with the lowest mahalanobisdistance}
#' \item{goodrows}{integer vector containing the rows of \code{sample} that are selected}
#' \item{mahagood}{mahalanobisdistances of the probable coordinates only}
#' @examples
#' require(Morpho)
#'      data(boneData)
#'     align <- rigidAlign(boneLM)$rotated
#'     mymod <- statismoBuildModel(align,representer=align[,,1],sigma=2,scale=TRUE)
#' #add some arbitrary data
#' myconfused <- matrix(rnorm(300),100,3)
#' myconfusedind <- sample(1:10,size=100,replace=T)
#' perturb <- sample(1:110)
#' out <- competingPoints(mymod,rbind(align[,,1],myconfused)[perturb,],c(1:10,myconfusedind)[perturb])
#' ##check if the selected coords are identical to the actual ones
#' all.equal(align[,,1],out$goodverts)
#'
#' @rdname competingPoints
#' @export
setGeneric("competingPoints", function(model,sample,index,mahalanobis=TRUE) {
    standardGeneric("competingPoints")
})

#' @rdname competingPoints
setMethod("competingPoints", signature(model="pPCA",sample="matrix",index="numeric"), function(model,sample,index,mahalanobis=TRUE) {
    if (length(index) != nrow(sample))
        stop("each point needs an index assigned")
    index <- index-1L
    out <- .Call("competingPoints",model,sample,index,mahalanobis)
    return(out)
})

#' save and load a statistical model of class pPCA to statismo hdf5 format
#'
#' save and load a statistical model of class pPCA to statismo hdf5 format
#' @param model object of class \code{\link{pPCA}}
#' @param modelname filename to read/save
#' @param pointer if TRUE an object of class pPCA_pointer is returned
#' @param scale specify if scaling was involved in model generation
#' @return statismoLoadModel returns an object of class \code{\link{pPCA}} while statismoSaveModel saves an object of class \code{\link{pPCA}} to disk in the statismo file format.
#' @name statismoLoadModel/statismoSaveModel
#' @seealso \code{\link{pPCA}}
#' @keywords StatisticalModel<representer>
#' @rdname statismoIO
#' @export
statismoSaveModel <- function(model, modelname=dataname) {
    dataname <- deparse(substitute(model))
    dataname <- paste0(dataname,".h5")
    modelname <- path.expand(modelname)
    storage.mode(modelname) <- "character"
    if (!inherits(model,"pPCA") && !inherits(model,"pPCA_pointer"))
        stop("model must be of class pPCA")
    out <- .Call("SaveModel",model,modelname)
}
#' @rdname statismoIO
#' @export
statismoLoadModel <- function(modelname,pointer=FALSE,scale=FALSE) {
    modelname <- path.expand(modelname)
    if (length(modelname) != 1)
        stop("only one file at a time please")
    if (! file.exists(modelname))
        stop(paste0("file ", modelname," does not exist"))
    storage.mode(modelname) <- "character"
    
    out <- (.Call("LoadModel",modelname,pointer))
    if (!pointer) {
        if (!pairNameCheck(out@modelinfo@paraminfo,"scale"))
            SetScale(out) <- scale
    }
    return(out)
}

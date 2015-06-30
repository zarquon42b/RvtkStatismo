#' Fast Procrustes align of coordinates
#' 
#' Fast Procrustes align of coordinates
#' @param array array of coordinates
#' @param scale logical: request scaling during alignment
#' @param use.lm integer vector: specifies the indices of the points that are to be used in the constrained model
#' @param deselect logical: if TRUE, \code{use.lm} references the missing coordinates instead of the present ones.
#' @param reference integer: instead of a GPA, all specimens are aligned to this specific one.
#' @param reflection logical: if TRUE, reflections are allowed to minimzie squared distance.
#' @return a list containing
#' \item{rotated}{array containing registered coordinates}
#' \item{mshape}{matrix containing meanshape}
#' @export
rigidAlign <- function(array,scale=FALSE,use.lm=NULL,deselect=FALSE,reference=NULL,reflection=FALSE) {
    k <- dim(array)[1]
    if (!is.null(use.lm)) {
        use.lm <- unique(sort(use.lm))
        if (deselect) {
            use.lm <- c(1:k)[-use.lm]
        }
    }
    out <- partialAlign(array,use.lm = use.lm,scale=scale,reference=reference,reflection=reflection)
}

partialAlign <- function(array,use.lm=NULL,scale=FALSE,reference=NULL,reflection=FALSE) {
    if (is.null(reference)){
        if (!is.null(use.lm)){
            procMod <- ProcGPA(array[use.lm,,],scale=scale,CSinit=F,reflection=reflection,silent = TRUE)##register all data using Procrustes fitting based on the non-missing coordinates
            tmp <- array
            a.list <-  1:(dim(array)[3])
            tmp <- lapply(a.list, function(i) {mat <- rotonmat(array[,,i],array[use.lm,,i],procMod$rotated[,,i],scale=scale,reflection = reflection);return(mat)})
            tmp1 <- Morpho::list2array(tmp)
            procMod$rotated <- tmp1
            procMod$mshape <- arrMean3(tmp1)
        } else {
            procMod <- ProcGPA(array,scale=scale,CSinit = F,reflection = reflection,silent = T)
        }
    } else {
        if (is.null(use.lm))
            use.lm <- 1:dim(array)[1]
        a.list <-  1:(dim(array)[3])
        tmp <- lapply(a.list, function(i) {mat <- rotonmat(array[,,i],array[use.lm,,i],array[,,reference],scale=scale,reflection = reflection);return(mat)})
        procMod <- list()
        procMod$rotated <- Morpho::list2array(tmp)
        procMod$mshape <- arrMean3(procMod$rotated)
        
    }
    return(procMod)
}

#' align meshes stored in a list by their vertices
#' 
#' align meshes stored in a list by their vertices
#' @param meshlist list containing triangular meshes of class "mesh3d"
#' @param scale logical: request scaling during alignment
#' @param deselect logical: if TRUE, missingIndex references the existing coordinates instead of the missing ones.
#' @param use.lm integer vector: specifies the indices of the points that are to be used in the constrained model
#' @param array logical: if TRUE the superimposed vertices will be returned as 3D array.
#' @return returns a list of aligned meshes or an array of dimensions k x 3 x n, where k=number of vertices and n=sample size.
#' @param reference integer: instead of a GPA, all specimens are aligned to this specific one.
#' @param reflection logical: if TRUE, reflections are allowed to minimzie squared distance.
#' @examples
#' mod <- statismoLoadModel(system.file("extdata","mandibles.h5",package="RvtkStatismo"))
#' shapes <- restoreSamples(mod)
#' newalign <- meshalign(shapes,reference=1)
#' @importFrom Morpho vert2points ProcGPA
#' @export
meshalign <- function(meshlist,scale=FALSE,use.lm=NULL,deselect=FALSE,array=FALSE,reference=NULL,reflection=FALSE) {
    vertarr <- meshlist2array(meshlist)
    out <- rigidAlign(vertarr,scale=scale,use.lm=use.lm,deselect=FALSE,reference=reference,reflection=reflection)$rotated
    if (array) {
        return(out)
    } else {
        if (is.null(reference)) {
            out <- lapply(1:length(meshlist),function(i){ res <- meshlist[[i]]
                                                          res$vb[1:3,] <- t(out[,,i])
                                                          res$normals <- NULL
                                                          return(res)})
        } else {
            out <- lapply(1:length(meshlist),function(i){ res <- meshlist[[i]]
                                                          res$vb[1:3,] <- t(out[,,i])
                                                          res$it <- meshlist[[reference]]$it
                                                          res$normals <- NULL
                                                          return(res)})
        }
        names(out) <- names(meshlist)
        return(out)
    }
}

#' align a sample to a model
#'
#' align a sample to a model
#'
#' @param model statistical model of class "pPCA"
#' @param sample matrix or mesh3d
#' @param scale logical: request scaling during alignment
#' @param ptDomain integer vector: specifies the indices of the domain points that are to be used for registration (order is important).
#' @param ptSample integer vector: specifies the indices of the sample that are to be used  for registration (order is important).
#' @return a rotated (and scaled) mesh or matrix - depending on the input.
#' @rdname align2domain
#' @export
setGeneric("align2domain", function(model,sample,scale=FALSE,ptDomain=NULL,ptSample=NULL) {
    standardGeneric("align2domain")
})
#' @rdname align2domain
setMethod("align2domain",signature(model="pPCA",sample="matrix"), function(model,sample,scale=FALSE, ptDomain=NULL,ptSample=NULL) {
    domain <- GetDomainPoints(model)
    if (is.null(ptDomain))
        ptDomain <- 1:nrow(domain)
    if (is.null(ptSample))
        ptSample <- 1:nrow(sample)
    rot <- rotonto(domain[ptDomain,],sample[ptSample,],scale=scale)$yrot
    return(rot)
})

#' @importFrom Morpho vert2points rotonto
#' @rdname align2domain
setMethod("align2domain",signature(model="pPCA",sample="mesh3d"), function(model,sample,scale=FALSE, ptDomain=NULL,ptSample=NULL) {
    
    sample0 <- vert2points(sample)
    rot <- align2domain(model,sample0,scale,ptDomain = ptDomain, ptSample = ptSample)
    out <- list(vb=rbind(t(rot),1),it=sample$it)
    class(out) <- "mesh3d"
    return(out)
})

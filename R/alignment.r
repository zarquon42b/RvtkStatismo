#' Fast Procrustes align of coordinates
#' 
#' Fast Procrustes align of coordinates
#' @param array array of coordinates
#' @param scale logical: request scaling during alignment
#' @param use.lm integer vector: specifies the indices of the points that are to be used in the constrained model
#' @param deselect logical: if TRUE, \code{use.lm} references the missing coordinates instead of the present ones.
#' @return a list containing
#' \item{rotated}{array containing registered coordinates}
#' \item{mshape}{matrix containing meanshape}
#'
#' @export
rigidAlign <- function(array,scale=TRUE,use.lm=NULL,deselect=FALSE) {
    k <- dim(array)[1]
    if (!is.null(use.lm)) {
        use.lm <- unique(sort(use.lm))
        if (deselect) {
            use.lm <- c(1:k)[-use.lm]
        }
    }
    out <- partialAlign(array,use.lm = use.lm,scale=scale)
}

partialAlign <- function(array,use.lm=NULL,scale=TRUE) {
    if (!is.null(use.lm)){
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
#' @importFrom Morpho vert2points ProcGPA
#' @export
meshalign <- function(meshlist,scale=TRUE,use.lm=NULL,deselect=FALSE,array=FALSE) {
    vertarr <- meshlist2array(meshlist)
    out <- rigidAlign(vertarr,scale=scale,use.lm=use.lm,deselect=FALSE)$rotated
    if (array) {
        return(out)
    } else {
        out <- lapply(1:length(meshlist),function(i){ res <- meshlist[[i]]
                                                      res$vb[1:3,] <- t(out[,,i])
                                                      res$normals <- NULL
                                                      return(res)})
        names(out) <- names(meshlist)
        return(out)
    }
}

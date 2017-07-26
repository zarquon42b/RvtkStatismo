#' convert an object of class pPCA to pPCA_pointer and vice versa
#'
#' convert an object of class pPCA to pPCA_pointer and vice versa
#' @param x object of class pPCA or pPCA_pointer
#' @return returns an object of class pPCA or pPCA_pointer
#' @rdname pointer2pPCA
#' @export
pPCA2pointer <- function(x) {
    if (!inherits(x,"pPCA"))
        stop("x must be of class pPCA")
    out <- .Call("pPCA2pointerCpp",x)
    out@scale <- x@scale
    return(out)
}

#' @rdname pointer2pPCA
#' @export
pointer2pPCA <- function(x) {
    if (!inherits(x,"pPCA_pointer"))
        stop("x must be of class pPCA_pointer")
    out <- .Call("pointer2pPCACpp",x)
    out@scale <- x@scale
    return(out)
}

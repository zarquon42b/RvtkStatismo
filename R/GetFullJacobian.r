#' get Jacobian for all model points or a subset
#'
#' get Jacobian for all model points or a subset
#'
#' @param model
#' @param pt integer vector specifying which points to use
#' @return
#' returns a matrix with rows n: (n+2) containing the Jacobian for the n-th point
#' @seealso \code{\link{GetJacobian}}
#' @export
#' @rdname GetFullJacobian 
setGeneric("GetFullJacobian", function(model,pt) {
    standardGeneric("GetFullJacobian")
})

#' @rdname GetFullJacobian
setMethod("GetFullJacobian", signature(model="pPCA"), function(model,pt) {
    if (missing(pt))
        pt <- GetDomainPoints(model)
    else
        pt <- GetDomainPoints(model)[pt,]
    # lse
      
    out <- .Call("GetFullJacobian",model,pt)
    return(out)
})

vtkICP <- function(refmesh,tarmesh,iterations=10,scale=FALSE) {
    out <- .Call("vtkICP",refmesh,tarmesh,iterations,scale)
    out$vb <- rbind(out$vb,1)
return(out)
}

vtkICP <- function(refmesh,tarmesh,iterations=10,center=FALSE,type=c("none","scale", "affine"),sample=200) {
    type <- tolower(substr(type[1],1L,1L))
    
    out <- .Call("vtkICP",refmesh,tarmesh,iterations,center,type,sample)
    out$vb <- rbind(out$vb,1)
return(out)
}

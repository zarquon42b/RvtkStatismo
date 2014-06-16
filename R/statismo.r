statismoBuildModel <- function(array,representer,sigma=0) {
    mylist <- array2meshlist(array)
    names(mylist) <- dimnames(array)[[3]]
    if (is.matrix(representer))
        representer <- list(vb=t(representer),it=matrix(0,0,0))
    else if (is.list(representer)) {
        if (!is.numeric(representer$vb) || !is.integer(representer$it))
            stop("representer needs vertices and faces")
        else
            representer$it <- representer$it-1
    }
    
    out <- .Call("BuildModel",mylist,representer,sigma)
}

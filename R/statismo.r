array2meshlist <- function(x) {
    n <- dim(x)[3]
    out <- lapply(1:n,function(i) {out <- list(vb=t(x[,,i]),it= matrix(0,0,0))})
    return(out)
}

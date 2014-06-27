#require(methods)
#' @export
setClass("pPCA",
         slots= c(PCA="list",scale="logical",representer="list",rawdata="matrix",sigma="numeric",Variance="data.frame"),
         prototype = list(PCA=list(sdev=0,rotation=0,x=0,center=0),scale=logical(),representer=list(),rawdata=matrix(0,0,0),sigma=numeric(0),Variance=data.frame())
         )
             

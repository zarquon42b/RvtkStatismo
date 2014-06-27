
setGeneric("SetNoiseVariance<-", function(x, value) standardGeneric("SetNoiseVariance<-"))
setReplaceMethod("SetNoiseVariance", "pPCA",function(x, value) {x@sigma <- value; validObject(x); x})

setGeneric("SetRawdata<-", function(x, value) standardGeneric("SetRawdata<-"))
setReplaceMethod("SetRawdata", "pPCA",function(x, value) {x@rawdata <- value; validObject(x); x})

setGeneric("SetPCBasisMatrix<-", function(x, value) standardGeneric("SetPCBasisMatrix<-"))
setReplaceMethod("SetPCBasisMatrix", "pPCA",function(x, value) {x@PCA$rotation <- value; validObject(x); x})

setGeneric("SetPCsdev<-", function(x, value) standardGeneric("SetPCsdev<-"))
setReplaceMethod("SetPCsdev", "pPCA",function(x, value) {x@PCA$sdev <- value; validObject(x); x})

setGeneric("SetMeanVector<-", function(x, value) standardGeneric("SetMeanVector<-"))
setReplaceMethod("SetMeanVector", "pPCA",function(x, value) {x@PCA$center <- value; validObject(x); x})

setGeneric("SetScores<-", function(x, value) standardGeneric("SetScores<-"))
setReplaceMethod("SetScores", "pPCA",function(x, value) {x@PCA$x <- value; validObject(x); x})

setGeneric("SetScale<-", function(x, value) standardGeneric("SetScale<-"))
setReplaceMethod("SetScale", "pPCA",function(x, value) {x@scale <- value; validObject(x); x})

setGeneric("SetPCA<-", function(x, value) standardGeneric("SetPCA<-"))
setReplaceMethod("SetPCA", "pPCA",function(x, value) {x@PCA <- value; validObject(x); x})

setGeneric("SetVariance<-", function(x, value) standardGeneric("SetVariance<-"))
setReplaceMethod("SetVariance", "pPCA",function(x, value) {x@Variance <- value; validObject(x); x})


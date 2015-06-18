#' remove a set of point from a statismo shape model
#'
#' remove a set of point from a statismo shape model
#' @param model pPCA model
#' @param pointind integer vector indicating which coordinates to remove
#' @return returns and updated model
#' @examples
#' ## create a GP model from a mesh and remove the first 1000 coordinates
#' require(Rvcg)
#' data(humface)
#' hummodel <- statismoModelFromRepresenter(humface,kernel=list(c(50,50)))
#' hummodel0 <- removePointsFromModel(hummodel,1:1000)
#' \dontrun{
#' require(rgl)
#' shade3d(DrawSample(hummodel0),col=3)
#' }
#' @importFrom Morpho rmVertex
#' @export
removePointsFromModel <- function(model,pointind) {
    model@PCA$x <- matrix(0,0,0)
    rminds <- (pointind-1)*3
    rminds <- c(rminds+1,rminds+2,rminds+3)
    model@PCA$center <- model@PCA$center[-rminds]
    model@PCA$rotation <- model@PCA$rotation[-rminds,]
    if (inherits(model@representer,"mesh3d"))
        model@representer <- rmVertex(model@representer,pointind)
    else
        model@representer$vb <-  model@representer$vb[,-pointind]
    validObject(model)
    return(model)
}
    

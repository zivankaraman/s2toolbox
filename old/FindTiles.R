#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param aoi PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sp]{is.projected}},\code{\link[sp]{spTransform}},\code{\link[sp]{c("over-methods", "over")}}
#' @export
#' @source \url{http://somewhere.important.com/}
#' @importFrom sp proj4string spTransform over
FindTiles <-
function(aoi)
{
    # make sure the same CRS is used

    if (sp::proj4string(S2_world_tiles) != sp::proj4string(aoi))
    aoi <- sp::spTransform(aoi, sp::proj4string(S2_world_tiles))
  # get tiles overlapping with aoi
  lst <- sp::over(aoi, S2_world_tiles, returnList = TRUE)
  out <- lapply(lst, FUN = function(x) as.character(x$tile))
  return(out)
}


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
#'  \code{\link[sf]{st_crs}},\code{\link[sf]{st_transform}},\code{\link[sf]{geos_binary_pred}}
#' @rdname FindTiles
#' @export 
#' @importFrom sf st_crs st_transform st_intersects
FindTiles <-
function(aoi)
{
    # make sure the same CRS is used
    if (sf::st_crs(aoi) != sf::st_crs(S2_world_tiles)) {
        aoi <- sf::st_transform(aoi, crs = sf::st_crs(S2_world_tiles))
    }
    # get tiles overlapping with aoi
    lst <- sf::st_intersects(aoi, S2_world_tiles)
    out <- S2_world_tiles$tile[unlist(lst)]
    return(out)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param aoi PARAM_DESCRIPTION
#' @param orbits PARAM_DESCRIPTION, Default: S2_relative_orbits
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[sf]{st_crs}},\code{\link[sf]{st_transform}},\code{\link[sf]{s2}},\code{\link[sf]{geos_binary_pred}},\code{\link[sf]{st_coordinates}},\code{\link[sf]{geos_unary}},\code{\link[sf]{st_geometry}}
#'  \code{\link[lutz]{tz_lookup_coords}}
#' @export 
#' @source \url{http://somewhere.important.com/}
#' @importFrom sf st_crs st_transform sf_use_s2 st_intersects st_coordinates st_centroid st_geometry
#' @importFrom lutz tz_lookup_coords
FindRelativeOrbits <-
function(aoi, orbits = S2_relative_orbits)
{
    # make sure the same CRS is used
    if (sf::st_crs(aoi) != sf::st_crs(S2_world_tiles)) {
        aoi <- sf::st_transform(aoi, crs = sf::st_crs(S2_world_tiles))
    }
    # get tiles overlapping with aoi


    # get orbits completely covering aoi
    old.s2 <- sf::sf_use_s2(FALSE)
    on.exit(sf::sf_use_s2(old.s2))

    lst <- sf::st_intersects(aoi, orbits)
    # lst <- sf::st_intersects(aoi, orbits, sparse = TRUE, duplicate_edges = TRUE, simplify_edge_chains = TRUE)
    # ans <- rgeos::gContainsProperly(orbits, aoi, byid = TRUE, checkValidity = TRUE)
    zone <- orbits[unlist(lst), ]

    # find tiles
    tiles <- FindTiles(aoi)

    # find the time zone
    centre <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(aoi)))
    tz <- lutz::tz_lookup_coords(centre[, "Y"], centre[, "X"], method = "accurate")
    # make "nice" position
    longitude <- gsub("d", "°", as.character(dd2dms(centre[, "X"], NS = FALSE)))
    latitude <- gsub("d", "°", as.character(dd2dms(centre[, "Y"], NS = TRUE)))
    pos <- paste(longitude, latitude, sep = ", ")

    attr(zone, "Tiles") <- tiles
    attr(zone, "TZ") <- tz
    attr(zone, "Position") <- pos

    return(zone)
}


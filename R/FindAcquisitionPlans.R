#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param aoi PARAM_DESCRIPTION
#' @param currentAcquisitionPlans PARAM_DESCRIPTION
#' @param all PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_zm}},\code{\link[sf]{st_crs}},\code{\link[sf]{st_transform}},\code{\link[sf]{geos_binary_pred}},\code{\link[sf]{st_coordinates}},\code{\link[sf]{geos_unary}}
#'  \code{\link[lutz]{tz_lookup_coords}}
#' @rdname FindAcquisitionPlans
#' @export
#' @importFrom sf st_zm st_crs st_transform st_intersects st_coordinates st_centroid
#' @importFrom lutz tz_lookup_coords
FindAcquisitionPlans <-
function(aoi, currentAcquisitionPlans, all = FALSE)
{
    if (missing(currentAcquisitionPlans) & exists("S2_current_acquisition_plans")) {
        # drop st_zm in future
        currentAcquisitionPlans <- sf::st_zm(S2_current_acquisition_plans)
    }
    # make sure the same CRS is used
    if (sf::st_crs(aoi) != sf::st_crs(currentAcquisitionPlans)) {
        aoi <- sf::st_transform(aoi, crs = sf::st_crs(currentAcquisitionPlans))
    }
    # get orbits completely covering aoi
    ans <- sf::st_intersects(aoi, currentAcquisitionPlans, sparse = TRUE)
    # ans <- rgeos::gContainsProperly(currentAcquisitionPlans, aoi, byid = TRUE, checkValidity = TRUE)
    zone <- currentAcquisitionPlans[unlist(ans), ]

    # find tiles
    tiles <- FindTiles(aoi)

    # find the time zone
    centre <- sf::st_coordinates(sf::st_centroid(aoi))
    tz <- lutz::tz_lookup_coords(centre[, "Y"], centre[, "X"], method = "accurate")
    # make "nice" position
    longitude <- gsub("d", "°", as.character(dd2dms(centre[, "X"], NS = FALSE)))
    latitude <- gsub("d", "°", as.character(dd2dms(centre[, "Y"], NS = TRUE)))
    pos <- paste(longitude, latitude, sep = ", ")

    attr(zone, "Tiles") <- unlist(tiles)
    attr(zone, "TZ") <- tz
    attr(zone, "Position") <- pos

    zone <- zone[with(zone, order(ObservationTimeStart)), ]
    row.names(zone) <- NULL

    return(zone)
}


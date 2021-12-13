#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param aoi PARAM_DESCRIPTION
#' @param orbits PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sp]{is.projected}},\code{\link[sp]{spTransform}},\code{\link[sp]{bbox-methods}},\code{\link[sp]{char2dms}}
#'  \code{\link[rgeos]{gContains}}
#'  \code{\link[lutz]{tz_lookup_coords}}
#' @export
#' @source \url{http://somewhere.important.com/}
#' @importFrom sp proj4string spTransform bbox dd2dms
#' @importFrom rgeos gContainsProperly
#' @importFrom lutz tz_lookup_coords
FindRelativeOrbits <-
function(aoi, orbits)
{
  if (missing(orbits) & exists("S2_realtive_orbits"))
    orbits <- S2_realtive_orbits
  # make sure same CRS is used
  if (sp::proj4string(orbits) != sp::proj4string(aoi))
    aoi <- sp::spTransform(aoi, sp::proj4string(orbits))
  # get orbits completely covering aoi
  ans <- rgeos::gContainsProperly(orbits, aoi, byid = TRUE, checkValidity = TRUE)
  zone <- orbits[ans[1, ], ]

  # find tiles
  tiles <- FindTiles(aoi)

  # find the time zone
  centre <- apply(sp::bbox(aoi), 1, mean)
  tz <- lutz::tz_lookup_coords(centre["y"], centre["x"], method = "accurate")
  # make "nice" position
  longitude <- gsub("d", "°", as.character(sp::dd2dms(centre["x"], NS = FALSE)))
  latitude <- gsub("d", "°", as.character(sp::dd2dms(centre["y"], NS = TRUE)))
  pos <- paste(longitude, latitude, sep = ", ")

  attr(zone, "Tiles") <- unlist(tiles)
  attr(zone, "TZ") <- tz
  attr(zone, "Position") <- pos

  return(zone)
}


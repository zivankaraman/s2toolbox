#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param zone PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @source \url{http://somewhere.important.com/}
AcquisitionPlansDates <-
# function(aoi, zone)
function(zone)
{
  # # retrieve the time zone
  # pos <- attr(zone, "Position")
  # tz <- attr(attr(zone, "Position"), "TZ")
  # tiles <- attr(zone, "Tiles")

  # convert to data.frame
  plan <- as(zone, "data.frame")
  # order by date
  plan <- plan[order(plan$ObservationTimeStart), ]

  # # find the time zone
  # centre <- apply(sp::bbox(aoi), 1, mean)
  # tz <- lutz::tz_lookup_coords(centre["y"], centre["x"], method = "accurate")
  # # make "nice" position
  # longitude <- gsub("d", "°", as.character(sp::dd2dms(centre["x"], NS = FALSE)))
  # latitude <- gsub("d", "°", as.character(sp::dd2dms(centre["y"], NS = TRUE)))
  # pos <- paste(longitude, latitude, sep = ", ")

  # retrieve the time zone
  tz <- attr(zone, "TZ")

  # get UTC and local times
  ObservationStartUTC <- as.POSIXct(sub("T", " ", plan$ObservationTimeStart), tz = "UTC")
  ObservationStopUTC <- as.POSIXct(sub("T", " ", plan$ObservationTimeStop), tz = "UTC")
  ObservationStartLocal <- as.POSIXct(format(ObservationStartUTC, "%F %T", tz = tz), tz = tz)
  ObservationStopLocal <- as.POSIXct(format(ObservationStopUTC, "%F %T", tz = tz), tz = tz)
  # build the output
  out <- data.frame(ID = plan$ID, RelativeOrbit = plan$OrbitRelative, Mission  = plan$Mission,
                    ObservationStartUTC, ObservationStopUTC,
                    Date = as.Date(ObservationStartLocal),
                    ObservationStartLocal, ObservationStopLocal, TimzZone = tz,
                    stringsAsFactors = FALSE, row.names = NULL)
  attr(out, "Position") <- attr(zone, "Position")
  attr(out, "Tiles") <- attr(zone, "Tiles")
  return(out)
}


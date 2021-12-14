#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param aoi PARAM_DESCRIPTION
#' @param plan PARAM_DESCRIPTION
#' @param world PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sp]{character(0)}}
#'  \code{\link[maps]{map}}
#' @export
#' @source \url{http://somewhere.important.com/}
#' @importFrom sp plot
#' @importFrom maps map
PlotOverpasses <-
function(overpasses, aoi, world = FALSE, ...)
{
    # plot overpasses first to set the plotting region
    plot(sf::st_geometry(overpasses), lwd = 2, border = "blue", axes = TRUE, cex.axis = 0.75, ...)

    if (world) {
        pkgs <- row.names(installed.packages())
        if (is.na(match("maps", pkgs))) {
            warning("'maps' package not available, can't draw the background world map")
        } else {
            maps::map("world", add = TRUE, col = "cyan")
            # re-plot overpasses over world map
            plot(sf::st_geometry(overpasses), lwd = 2, border = "blue", add = TRUE)
        }
    }
    plot(sf::st_geometry(aoi), col = "red", border = "red", lwd = 5, add = TRUE)
    # re-plot overpasses over world map
    plot(sf::st_geometry(overpasses), lwd = 2, border = "blue", add = TRUE)
}


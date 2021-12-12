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
PlotAcquisitionPlans <-
function(aoi, plan, world = FALSE, ...)
{
  sp::plot(plan, lwd = 2, border = "blue", axes = TRUE, cex.axis = 0.75, ...)
    if (world) {
        pkgs <- row.names(installed.packages())
        if (is.na(match("maps", pkgs))) {
            warning("'maps' package not available, can't draw the background world map")
        } else {
            maps::map("world", add = TRUE, col = "cyan")
            # replot plan over world map
            sp::plot(plan, lwd = 2, border = "blue", add = TRUE)
        }
    }
    sp::plot(aoi, col = "red", border = "red", lwd = 5, add = TRUE)
}


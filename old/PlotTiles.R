#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param aoi PARAM_DESCRIPTION
#' @param tile_col PARAM_DESCRIPTION, Default: NA
#' @param tile_border PARAM_DESCRIPTION, Default: 'blue'
#' @param aoi_col PARAM_DESCRIPTION, Default: 'red'
#' @param aoi_border PARAM_DESCRIPTION, Default: aoi_col
#' @param labels_col PARAM_DESCRIPTION, Default: 'orange'
#' @param labels_cex PARAM_DESCRIPTION, Default: 0.5
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
PlotTiles <-
function(x, aoi, tile_col = NA,
         tile_border = "blue", aoi_col = "red", aoi_border = aoi_col, labels_col = "orange", labels_cex = 0.5,
         world = FALSE, ...)
{
    tiles <- S2_world_tiles[unlist(x), ]
    # plot tiles first to set the plotting region
    sp::plot(tiles, col = tile_col, border = tile_border, ...)

    if (world) {
        pkgs <- row.names(installed.packages())
        if (is.na(match("maps", pkgs))) {
            warning("'maps' package not available, can't draw the background world map")
        } else {
            maps::map("world", add = TRUE, col = "cyan")
            # replot tiles over world map
            sp::plot(tiles, col = tile_col, border = tile_border, add = TRUE, ...)
        }
    }

    if (!missing(aoi)) {
        sp::plot(aoi, add = TRUE, col = aoi_col, border = aoi_border, ...)
        # replot tiles over aoi
        sp::plot(tiles, col = tile_col, border = tile_border, add = TRUE, ...)
    }

    xy <- t(sapply(tiles@polygons, GetBootomRight))
    delta <- diff(par()$usr)[c(1, 3)]
    # xy[, 1] <- xy[, 1] - delta[1] / 80.0
    # xy[, 2] <- xy[, 2] + delta[2] / 50.0
    fact <- Shift(length(tiles))
    xy[, 1] <- xy[, 1] - delta[1] / fact[1]
    xy[, 2] <- xy[, 2] + delta[2] / fact[2]
    Shift(length(tiles))
    text(xy, labels = tiles$tile, cex = labels_cex, adj = c(1, 0), col = labels_col)

}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param n PARAM_DESCRIPTION
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
Shift <-
function(n)
{
    c(n + 100.0, n/2.0 + 50.0)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param p PARAM_DESCRIPTION
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
GetBootomRight <-
function(p)
{
    # take the first Polygon only (ignore the others, if any)
    koords <- p@Polygons[[1]]@coords[-1, ]
    # scale to 0-mean
    zkoords <- scale(koords, center = TRUE, scale = FALSE)
    # get the bottom right vertex - in original coordinates (not scaled)
    koords[which(zkoords[ , "x"] > 0 & zkoords[ ,"y"] < 0), ]
}

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
#'  \code{\link[sf]{st_geometry}}
#'  \code{\link[maps]{map}}
#' @rdname PlotTiles
#' @export 
#' @importFrom sf st_geometry
#' @importFrom maps map
PlotTiles <-
function(x, aoi, tile_col = NA,
         tile_border = "blue", aoi_col = "red", aoi_border = aoi_col, labels_col = "orange", labels_cex = 0.5,
         world = FALSE, ...)
{
    tiles <- S2_world_tiles[x, ]
    # tile_col = NA
    # tile_border = "blue"
    # aoi_col = "red"
    # aoi_border = aoi_col
    # plot tiles first to set the plotting region
    plot(sf::st_geometry(tiles), border = tile_border, col = tile_col, ...)

    if (world) {
        pkgs <- row.names(installed.packages())
        if (is.na(match("maps", pkgs))) {
            warning("'maps' package not available, can't draw the background world map")
        } else {
            maps::map("world", add = TRUE, col = "cyan")
            # re-plot tiles over world map
            plot(sf::st_geometry(tiles), border = tile_border, col = tile_col, add = TRUE, ...)
        }
    }

    if (!missing(aoi)) {
        plot(sf::st_geometry(aoi), border = aoi_border, col = aoi_col, add = TRUE)
        # re-plot tiles over world map
        plot(sf::st_geometry(tiles), border = tile_border, col = tile_col, add = TRUE, ...)
    }

    xy <- matrix(NA, nrow = nrow(tiles), ncol = 2)
    for (i in 1:nrow(tiles)) {
        xy[i, ] <- GetBootomRight(tiles[i, ])
    }
    delta <- diff(par()$usr)[c(1, 3)]
    fact <- Shift(nrow(tiles))
    xy[, 1] <- xy[, 1] - delta[1] / fact[1]
    xy[, 2] <- xy[, 2] + delta[2] / fact[2]
    text(xy, labels = tiles$tile, cex = labels_cex, adj = c(1, 0), col = labels_col)
}


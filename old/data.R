#' @title Sentinel 2 tiles
#' @description A data set containing the polygons of Sentinel 2 tiles.
#'     For Level-1C and Level-2A, the tiles, also called granules, are 100x100 km2 ortho-images in UTM/WGS84 projection.
#' @format Object of class '\code{sf}' and '\code{MULTIPOLYGON}' geometry type, with 56686 features and 1 attribute:
#' \describe{
#'   \item{tile}{name of the tile/granule}
#' }
#' @source \url{https://sentinel.esa.int/web/sentinel/missions/sentinel-2/data-products}
"S2_world_tiles"

#' @title Sentinel 2 relative orbits.
#' @description A data set containing the polygons of Sentinel 2 relative orbits.
#'     Sentinel 2A and 2B follow 143 orbits.
#'     A theoretical overflow calendar for a place on Earth can be computed from these data.
#' @format Object of class '\code{sf}' and '\code{MULTIPOLYGON}' geometry type, with 143 features and 3 attributes:
#' \describe{
#'   \item{OrbitRelative}{relative orbit number}
#'   \item{S2A.Origin}{the first date in 2018 this orbit was flown by Sentinel 2A satellite}
#'   \item{S2B.Origin}{the first date in 2018 this orbit was flown by Sentinel 2B satellite}
#' }
#' @source \url{https://sentinel.esa.int/web/sentinel/user-guides/sentinel-2-msi/revisit-coverage}
"S2_realtive_orbits"



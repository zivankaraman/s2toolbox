
FindRelativeOrbits <-
function(aoi, orbits = S2_relative_orbits)
{
    # make sure the same CRS is used
    if (sf::st_crs(aoi) != sf::st_crs(S2_world_tiles)) {
        aoi <- sf::st_transform(aoi, crs = sf::st_crs(S2_world_tiles))
    }
    # get tiles overlapping with aoi


    # get orbits completely covering aoi
    ans <- sf::sf_use_s2(FALSE)
    lst <- sf::st_intersects(aoi, orbits)
    # lst <- sf::st_intersects(aoi, orbits, sparse = TRUE, duplicate_edges = TRUE, simplify_edge_chains = TRUE)
    # ans <- rgeos::gContainsProperly(orbits, aoi, byid = TRUE, checkValidity = TRUE)
    zone <- orbits[unlist(lst), ]

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


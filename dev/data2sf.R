# relative orbits
load("C:/R/Packages/s2toolbox/data/S2_realtive_orbits.rda")
S2_relative_orbits <- sf::st_as_sf(S2_realtive_orbits)
save(S2_relative_orbits, file = "./data/S2_relative_orbits.rda")

#world tiles
load("C:/R/Packages/s2toolbox/data/S2_world_tiles.rda")
S2_world_tiles <- sf::st_as_sf(S2_world_tiles)
save(S2_world_tiles, file = "./data/S2_world_tiles.rda")

require(s2toolbox)

# get the New York City Central Park shape as area of interest
dsn <- system.file("extdata", "centralpark.geojson", package = "spectator")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
dsn <- system.file("extdata", "luxembourg.geojson", package = "spectator")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
aoi <- sf::read_sf("dev/Untitled Polygon.kml", as_tibble = FALSE)
aoi = sf::st_zm(aoi)
ans = sf::st_intersects(aoi, S2_world_tiles)
x = unlist(ans)
x = S2_world_tiles$tile[unlist(ans)]
plot(aoi)

plot(S2_world_tiles[ans[[1]], ])
library(sf)
PlotTiles(FindTiles(aoi), aoi, main = "title", lwd = 2, world = T)


p = FindAcquisitionPlans(aoi)

fr = sf::read_sf("C:/AzureDevOps/R-scripts-country-maps/Shapes/FR.shp")
p = FindAcquisitionPlans(fr)

for (i in 1:nrow(p)) print(st_intersects(p[i, ], S2_world_tiles[attr(p, "Tiles"), ]))

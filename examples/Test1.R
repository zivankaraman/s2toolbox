setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sp)
library(rgdal)
library(s2toolbox)
library(maps)


poly <- readOGR("Polygon.kml")
poly <- readOGR("Field.kml")
poly <- readOGR("FieldUA.kml")
poly <- readOGR("WallaWalla.kml")



myTiles <- FindTiles(poly)

PlotTiles(myTiles, poly, axes = FALSE, cex.axis = .5, main = "titre", aoi_border = "red", aoi_col = NA, labels_cex = 0.5, world = T)

map("france", add = TRUE)

load("WorldCountries.rda")

# get rid of islands
fr <- sp::disaggregate(subset(WorldCountries, ADMIN == "France"))[1, ]
de <- sp::disaggregate(subset(WorldCountries, ADMIN == "Germany"))[1, ]

myTiles <- FindTiles(rbind(fr, de))
PlotTiles(myTiles, rbind(fr, de), axes = FALSE, cex.axis = .5, main = "FRANCE / GERMANY", aoi_col = "red", labels_col = "green")
PlotTiles(myTiles, rbind(fr, de), axes = FALSE, cex.axis = .5, main = "FRANCE / GERMANY",
          aoi_col = NA, aoi_border = "red", labels_col = "green", world = TRUE)

.S2_world_tiles <- getFromNamespace(".S2_world_tiles", ns = "s2toolbox")
all_tiles <- getFromNamespace(".S2_world_tiles", ns = "s2toolbox")


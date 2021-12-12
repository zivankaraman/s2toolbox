library(sf)
abb <- read_sf("C:/R/Packages/s2toolbox/examples/Abbeville.kml")
plot(abb)
aoi <- as(st_zm(abb),"Spatial")

fic <- "C:/R/Packages/s2toolbox/KML/Sentinel-2B_MP_ACQ_KML_20211209T120000_20211227T150000.kml"
plan <- ParseKML(fic)
fic2 <- "C:/R/Packages/s2toolbox/KML/Sentinel-2B_MP_ACQ_KML_20211125T120000_20211213T150000.kml"
plan2 <- ParseKML(fic2)
intersect(plan$ID, plan2$ID)

fichiers <- list.files("KML")
mission <- substring(fichiers, 1, 11)
timespan <- sub(".kml$", "", sub("^.*MP_ACQ_KML_", "", fichiers))
catalog <- data.frame(kml = fichiers, mission = mission, timespan = timespan, stringsAsFactors = FALSE, row.names = NULL)
catalog[with(catalog, order(mission, timespan)), ]

fic3 <- paste0("KML/", catalog$kml[7])
plan3 <- ParseKML(fic3)


kml <- "C:/R/Packages/s2toolbox/KML/Sentinel-2B_MP_ACQ_KML_20211209T120000_20211227T150000.kml"
kml <- "C:/R/Packages/s2toolbox/KML/Sentinel-2A_MP_ACQ_KML_20211209T120000_20211220T150000.kml"

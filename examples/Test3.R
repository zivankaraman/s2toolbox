setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(s2toolbox)


poly <- rgdal::readOGR("Polygon.kml")
poly <- rgdal::readOGR("Field.kml")
poly <- rgdal::readOGR("FieldUA.kml")
poly <- rgdal::readOGR("WallaWalla.kml")
poly <- rgdal::readOGR("Abbeville.kml")

p <- FindAcquisitionPlans(poly)
plot(p)
p@data
PlotAcquisitionPlans(poly, p, main = "WallaWalla", world = T)

cal <- AcquisitionPlansDates(poly, p)

PlotAcquisitionCalendar(cal, fill = TRUE, title = "cal")

tiles <- FindTiles(poly)
attr(cal, "Tiles") <- unlist(tiles)
kal <- MakeCalendar(cal)

ExportCalendar(cal, "test1a.ics", utc = FALSE, strip_time = FALSE)
ExportCalendar(cal, "test1b.ics", utc = TRUE, strip_time = FALSE)

ExportCalendar(cal, "test2a.ics", utc = FALSE, strip_time = TRUE)
ExportCalendar(cal, "test2b.ics", utc = TRUE, strip_time = TRUE)


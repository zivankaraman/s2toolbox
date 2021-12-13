setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sp)
library(rgdal)
library(s2toolbox)
library(maps)


poly <- rgdal::readOGR("Polygon.kml")
poly <- rgdal::readOGR("Field.kml")
poly <- rgdal::readOGR("FieldUA.kml")
poly <- rgdal::readOGR("WallaWalla.kml")
poly <- rgdal::readOGR("Abbeville.kml")

p <- FindAcquisitionPlans(poly)
Qplot(p)
p@data


cal <- AcquisitionPlansDates(poly, p)

PlotAcquisitionCalendar(cal, fill = TRUE, title = "cal")


PlotAcquisitionPlans(poly, p, main = "WallaWalla", world = T)


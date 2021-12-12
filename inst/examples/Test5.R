setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(s2toolbox)


poly <- rgdal::readOGR("Polygon.kml")
poly <- rgdal::readOGR("Field.kml")
poly <- rgdal::readOGR("FieldUA.kml")
poly <- rgdal::readOGR("WallaWalla.kml")
poly <- rgdal::readOGR("Abbeville.kml")

zone <- FindRelativeOrbits(poly)
PlotAcquisitionPlans(poly, zone, main = "WallaWalla", world = F)

period <- c(as.Date("2019-08-01"), as.Date("2019-09-30"))
period <- c(as.Date("2018-06-01"), as.Date("2018-12-31"))

p <- FindAcquisitionPlans(poly, currentAcquisitionPlans = S2_archive_acquisition_plans, all = TRUE)
p <- FindAcquisitionPlans(poly, all = TRUE)
cal <- AcquisitionPlansDates(p)
cal <- subset(cal, between(Date, period[1], period[2]))
PlotAcquisitionCalendar(cal, fill = TRUE, title = "current")

plan <- ForecastAcquisitionPlans(poly, c(min(cal$Date), max(cal$Date)))
PlotAcquisitionCalendar(plan, fill = TRUE, title = "forecast")


p <- FindAcquisitionPlans(poly, currentAcquisitionPlans = S2_archive_acquisition_plans, all = TRUE)
cal <- AcquisitionPlansDates(p)
cal <- subset(cal, between(Date, period[1], period[2]))
PlotAcquisitionCalendar(cal, fill = TRUE, title = "current")

catalogue <- sinergise::SearchArchive(poly, period)
catalogue[, "Date"] <- catalogue[, "date"]
catalogue[, "Mission"] <- as.character(c(SENTINEL2A = "Sentinel-2A", SENTINEL2B = "Sentinel-2B")[catalogue$satellite])
PlotAcquisitionCalendar(catalogue, fill = TRUE, title = "observed")


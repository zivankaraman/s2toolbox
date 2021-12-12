setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(s2toolbox)


poly <- rgdal::readOGR("Polygon.kml")
poly <- rgdal::readOGR("Field.kml")
poly <- rgdal::readOGR("FieldUA.kml")
poly <- rgdal::readOGR("WallaWalla.kml")
poly <- rgdal::readOGR("Abbeville.kml")

p <- ForecastAcquisitionPlans(poly)
plot(p)
p@data
PlotAcquisitionPlans(poly, p, main = "WallaWalla", world = T)

schedule <- p@data
period <- c(as.Date("2019-09-01"), as.Date("2019-09-30"))
period <- sort(period)
if (min(period) < max(c(schedule$S2A.Origin, schedule$S2B.Origin))) stop()

out <- NULL
for (i in 1:nrow(schedule)) {
    for (miss in c("S2A", "S2B")) {
        s <- seq.Date(from = schedule[i, paste0(miss, ".Origin")], to = period[2], by = "10 days")
        days <- s[between(s, period[1], period[2])]
        nd <- length(days)
        if (nd > 0) {
            tmp <- data.frame(OrbiteRelative = rep(schedule$OrbitRelative[i], nd),
                              Mission = rep(miss, nd),
                              Date = days,
                              stringsAsFactors = FALSE, row.names = NULL)
            if (is.null(out)) {
                out <- tmp
            } else {
                out <- rbind(out, tmp)
            }
        }
    }
}

out$Mission <- as.character(c(S2A = "Sentinel-2A", S2B = "Sentinel-2B")[out$Mission])
out <- out[order(out$Date), ]



# cal <- AcquisitionPlansDates(poly, p)
cal <- AcquisitionPlansDates(p)
PlotAcquisitionCalendar(cal, fill = TRUE, title = "cal")

tiles <- FindTiles(poly)
attr(cal, "Tiles") <- unlist(tiles)
kal <- MakeCalendar(cal)

ExportCalendar(cal, "test1a.ics", utc = FALSE, strip_time = FALSE)
ExportCalendar(cal, "test1b.ics", utc = TRUE, strip_time = FALSE)

ExportCalendar(cal, "test2a.ics", utc = FALSE, strip_time = TRUE)
ExportCalendar(cal, "test2b.ics", utc = TRUE, strip_time = TRUE)


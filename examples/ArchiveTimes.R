setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sp)
library(lutz)
# library(s2toolbox)

load("C:/Users/zivan.karaman/Documents/Sentinel-2_Acquisition_Plans/S2_archive_acquisition_plans.rda")
# get timezones
fun <- function(x) {
    centre <- apply(sp::bbox(x), 1, mean)
    centre["x"] <- centre["x"] %% (sign(centre["x"]) * 180.0)
    tz <- lutz::tz_lookup_coords(centre["y"], centre["x"], method = "accurate")
}


TZ <- sapply(S2_archive_acquisition_plans@polygons, fun)

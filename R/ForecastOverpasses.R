
ForecastOverpasses <-
function(aoi, period)
{
  zone <- FindRelativeOrbits(aoi)

  schedule <- zone@data
  period <- sort(period)
  if (min(period) < max(c(schedule$S2A.Origin, schedule$S2B.Origin))) stop("period not covered")

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

  attr(out, "Position") <- attr(zone, "Position")
  attr(out, "Tiles") <- attr(zone, "Tiles")

  return(out)
}


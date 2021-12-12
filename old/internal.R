Shift <-
function(n)
{
    c(n + 100.0, n/2.0 + 50.0)
}

GetBootomRight <-
function(p)
{
    # take the first Polygon only (ignore the others, if any)
    koords <- p@Polygons[[1]]@coords[-1, ]
    # scale to 0-mean
    zkoords <- scale(koords, center = TRUE, scale = FALSE)
    # get the bottom right vertex - in original coordinates (not scaled)
    koords[which(zkoords[ , "x"] > 0 & zkoords[ ,"y"] < 0), ]
}

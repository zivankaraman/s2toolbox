ReadZipShape <-
function(zipfile)
{
    files <- unzip(zipfile, list = TRUE)[, "Name"]
    filenames <- basename(files)
    idx <- grep("*.\\.shp$", filenames)
    if (length(idx) != 1) {
        stop("zip must contain exactly one shp file")
    }
    layer <- sub("\\.shp$", "", filenames[idx])
    shp <- files[idx]

    dbf <- sub("\\.shp$", ".dbf", shp)
    if (length(grep(dbf, files)) == 0) {
        stop("no dbf file")
    }
    shx <- sub("\\.shp$", ".shx", shp)
    if (length(grep(shx, files)) == 0) {
        stop("no shx file")
    }
    lst <- c(shp, shx, dbf)
    prj <- sub("\\.shp$", ".prj", shp)
    if (length(grep(prj, files)) == 0) {
        warning("no prj file, assuming unprojected WGS84")
    } else {
        lst <- c(lst, prj)
    }

    path <- tempfile("dir")
    dir.create(path)
    unzip(zipfile, files = lst, list = FALSE, overwrite = TRUE, junkpaths = TRUE, exdir = path, unzip = "internal",setTimes = FALSE)
    if (is.na(match("sf", row.names(installed.packages())))) {
        obj <- rgdal::readOGR(dsn = path, layer = layer)
    } else {
        obj <- as(sf::st_read(dsn = path, layer = layer), "Spatial")
    }

    if (length(lst) == 3) { # no prj file
        sp::proj4string(obj) <- sp::CRS("+init=epsg:4326")
    }

    return(obj)

}



# zipfile <- "C:/Users/zivan.karaman/Downloads/Compressed/New_York_City_Borough_Boundaries.zip"
# zipfile <- "poly.zip"

p <- ReadZipShape(zipfile)

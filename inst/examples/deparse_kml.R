library(sf)
library(xml2)

fic <- "C:/R/Packages/s2toolbox/KML/Sentinel-2B_MP_ACQ_KML_20211209T120000_20211227T150000.kml"
layers <- sf::st_layers(fic)


kml <- xml2::read_xml(fic)
xml2::xml_ns_strip(kml)
noeuds1 <- xml2::xml_find_all(kml, ".//name")
noeuds2 <- xml2::xml_parent(xml2::xml_find_all(kml, ".//name"))
chemins <- xml2::xml_path(noeuds2)
noms <- xml2::xml_text(noeuds1)
noms[1:10]
chemins[1:10]

# find the NOBS node
i <- grep("NOBS", noms)

# l = xml2::as_list(noeuds2[i])
# ll = l[[1]]

NOBS <- xml2::as_list(noeuds2[i])[[1]]
# find the names of the sub-folders
ii <- grep("Folder", names(NOBS))
foldnoms <- unlist(sapply(ii, FUN = function(x) NOBS[[x]]$name))
# get the NOBS/NOMINAL folder
fold <- NOBS[[ii[grep("NOMINAL", foldnoms)]]]
# get all the polygons and their attributes
lst <- fold[grep("Placemark", names(fold))]

# get Placemark names
pnames <- unlist(sapply(lst, "[[", "name"))
# get Polygons
poly <- sapply(lst, FUN = function(x) {
    kords <- x$Polygon$outerBoundaryIs$LinearRing$coordinates[[1]]
    koords <- trimws(gsub(",0 ", " ", gsub("[[:blank:]]", " ", gsub("\n", " ", kords))))
    })
# get ExtendedData
ext <- lapply(lst, FUN = function(x) {
    ext <- x$ExtendedData
    vals <- sapply(ext, FUN = function(x) x$value[[1]])
    names(vals) <- sapply(ext, FUN = function(x) attr(x, "name"))
    vals
})

length(pnames)
length(poly)
length(ext)


class(pnames)
class(poly)
class(ext)

SafeNull <-
    function(x)
    {
        ifelse(is.null(x), NA, x)
    }


extdat <- as.data.frame(do.call(rbind, ext), stringsAsFactors = FALSE)
for (j in 1:ncol(extdat)) {
    extdat[, j] <- sapply(extdat[, j], SafeNull)
}
for (j in 7:10) {
    extdat[, j] <- as.integer(extdat[, j])
}

extdat$ID
extdat$Timeliness

class(extdat$Station)
all(extdat$ID == pnames)


head(pnames)
head(poly)
p2 = gsub(" ", "; ", poly)
p2[1]
p3 = gsub(",", " ", p2)
p3[1]
p4 = gsub(";", ",", p3)
p4[1]
p5 <- paste0("POLYGON ((", p4, "))")
p5[1]


extdat$geom = p5

df = sf::st_as_sf(extdat, wkt = "geom", crs = 4326)
df
library(sf)

plot(head(df))



length(lst)
lst[[1]]$name
lst[[1]]$TimeSpan
kords <- lst[[1]]$Polygon$outerBoundaryIs$LinearRing$coordinates[[1]]
koords <- trimws(gsub(",0 ", " ", gsub("[[:blank:]]", " ", gsub("\n", " ", kords))))

ext <- lst[[1]]$ExtendedData
sapply(ext, FUN = function(x) x$value[[1]])
sapply(ext, FUN = function(x) attr(x, "name"))






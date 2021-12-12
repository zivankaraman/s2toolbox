#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param kml PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[xml2]{read_xml}},\code{\link[xml2]{xml_ns_strip}},\code{\link[xml2]{xml_find_all}},\code{\link[xml2]{xml_children}},\code{\link[xml2]{xml_text}},\code{\link[xml2]{as_list}}
#'  \code{\link[sf]{st_as_sf}}
#' @rdname ParseKML
#' @export
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_parent xml_text as_list
#' @importFrom sf st_as_sf
ParseKML <- function(kml) {
    # read as XML
    xml <- xml2::read_xml(kml)
    # strip namespace
    xml2::xml_ns_strip(xml)
    # find folder nodes
    noeuds1 <- xml2::xml_find_all(xml, ".//name")
    noeuds2 <- xml2::xml_parent(xml2::xml_find_all(xml, ".//name"))
    # chemins <- xml2::xml_path(noeuds2)
    noms <- xml2::xml_text(noeuds1)
    # find the NOBS node
    i <- grep("NOBS", noms)
    NOBS <- xml2::as_list(noeuds2[i])[[1]]
    # find the names of the sub-folders
    ii <- grep("Folder", names(NOBS))
    foldnoms <- unlist(sapply(ii, FUN = function(x) NOBS[[x]]$name))
    # get the NOBS/NOMINAL folder
    fold <- NOBS[[ii[grep("NOMINAL", foldnoms)]]]
    # get all the polygons and their attributes
    lst <- fold[grep("Placemark", names(fold))]
    # # get Placemark names
    # pnames <- unlist(sapply(lst, "[[", "name"))
    # get Polygons
    poly <- sapply(lst, FUN = function(x) {
        kords <- x$Polygon$outerBoundaryIs$LinearRing$coordinates[[1]]
        koords <- trimws(gsub("[[:blank:]]", " ", gsub("\n", " ", kords)))
        sub(",0$", "", gsub(",0 ", " ", koords))
    })
    # get ExtendedData
    ext <- lapply(lst, FUN = function(x) {
        ext <- x$ExtendedData
        vals <- sapply(ext, FUN = function(x) ifelse(length(x$value) == 0, NA, x$value[[1]]))
        names(vals) <- sapply(ext, FUN = function(x) attr(x, "name"))
        vals
    })
    SafeNull <- function(x) {
            ifelse(is.null(x), NA, x)
        }
    extdat <- as.data.frame(do.call(rbind, ext), stringsAsFactors = FALSE)
    for (j in 1:ncol(extdat)) {
        extdat[, j] <- sapply(extdat[, j], SafeNull)
    }
    for (j in 7:10) {
        extdat[, j] <- as.integer(extdat[, j])
    }
    extdat[, "Mission"] <- substring(basename(kml), 1, 11)
    extdat[, "KML"] <- basename(kml)
    row.names(extdat) <- NULL
    # p2 = gsub(" ", "; ", poly)
    # p3 = gsub(",", " ", p2)
    # p4 = gsub(";", ",", p3)
    # wkt = paste0("POLYGON ((", p4, "))")
    wkt <- paste0("POLYGON ((", gsub(";", ",", gsub(",", " ", gsub(" ", "; ", poly))), "))")
    extdat$geom <- wkt
    out <- sf::st_as_sf(extdat, wkt = "geom", crs = 4326)
    return(out)
}

# test that all files can be parsed
fichiers <- list.files("KML", full.names = TRUE)
for (i in 1:length(fichiers)) {
    x <- ParseKML(fichiers[i])
    print(sf::st_z_range(x))
}




texte <- readLines("https://sentinel.esa.int/web/sentinel/missions/sentinel-2/acquisition-plans")
texte <- texte[grep('Sentinel-2[AB]_MP_ACQ_KML_.*\\.kml">', texte)]
urls <- sub('^.*href="', '', sub('kml">.*$', 'kml', texte))
fichiers <- basename(urls)
mission <- substring(fichiers, 1, 11)
timespan1 <- sub(".kml$", "", sub("^.*MP_ACQ_KML_", "", fichiers))
timespan2 <- strsplit(timespan1, split = "_", fixed = TRUE)

catalog <- data.frame(url = urls,
                      kml = fichiers,
                      mission = mission,
                      start = as.Date(sapply(timespan2, "[[", 1), format = "%Y%m%dT%H%M%S"),
                      finish = as.Date(sapply(timespan2, "[[", 2), format = "%Y%m%dT%H%M%S"),
                      stringsAsFactors = FALSE, row.names = NULL)
catalog <- catalog[with(catalog, order(mission, -as.integer(start))), ]

iskon <- Sys.Date()

catalog <- subset(catalog, finish >= iskon)
lst <- catalog$url
url_kml_prefix <- 'https://sentinel.esa.int'
from <- paste0(url_kml_prefix, lst)
path <- tempfile("kml")
dir.create(path)
to <- sub('/documents/[[:digit:]]+/[[:digit:]]+/Sentinel', 'Sentinel', lst)
to <- paste(path, to, sep = "/")

N <- length(from)
for (i in 1:N) {
    download.file(from[i], to[i], mode = "wb", quiet = FALSE)
}

S2_current_acquisition_plans <- NULL

for (i in 1:N) {
    fic <- to[i]
    cat(sprintf("Processing file %s ...", basename(fic)), sep = "\n")
    x <- ParseKML(fic)
    if (is.null(S2_current_acquisition_plans)) {
        S2_current_acquisition_plans <- x
    } else {
        # include only missing observations
        ndx <- setdiff(x$ID, S2_current_acquisition_plans$ID)
        S2_current_acquisition_plans <- rbind(S2_current_acquisition_plans, subset(x, ID %in% ndx))
    }
}

table(table(S2_current_acquisition_plans$ID))

# fic <- "C:/R/Packages/s2toolbox/KML/Sentinel-2B_MP_ACQ_KML_20211209T120000_20211227T150000.kml"
# plan <- ParseKML(fic)
# fic2 <- "C:/R/Packages/s2toolbox/KML/Sentinel-2B_MP_ACQ_KML_20211125T120000_20211213T150000.kml"
# plan2 <- ParseKML(fic2)
# intersect(plan$ID, plan2$ID)
#
# fichiers <- list.files("KML")
# mission <- substring(fichiers, 1, 11)
# timespan <- sub(".kml$", "", sub("^.*MP_ACQ_KML_", "", fichiers))
# catalog <- data.frame(kml = fichiers, mission = mission, timespan = timespan, stringsAsFactors = FALSE, row.names = NULL)
# catalog[with(catalog, order(mission, timespan)), ]
#
# fic3 <- paste0("KML/", catalog$kml[7])
# plan3 <- ParseKML(fic3)
#

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param past PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param keep PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname UpdateCurrentAcquisitionPlans
#' @export
UpdateCurrentAcquisitionPlans <-
function(past = FALSE, verbose = FALSE, keep = TRUE)
{
    # scrape the acquisition-plans page at ESA to get list of KML files
    texte <- readLines("https://sentinel.esa.int/web/sentinel/missions/sentinel-2/acquisition-plans")
    texte <- texte[grep('Sentinel-2[AB]_MP_ACQ_KML_.*\\.kml">', texte)]
    urls <- sub('^.*href="', '', sub('kml">.*$', 'kml', texte))
    fichiers <- basename(urls)
    mission <- substring(fichiers, 1, 11)
    timespan1 <- sub(".kml$", "", sub("^.*MP_ACQ_KML_", "", fichiers))
    timespan2 <- strsplit(timespan1, split = "_", fixed = TRUE)
    # buid the catalog of available files
    catalog <- data.frame(url = urls,
                          kml = fichiers,
                          mission = mission,
                          start = as.Date(sapply(timespan2, "[[", 1), format = "%Y%m%dT%H%M%S"),
                          finish = as.Date(sapply(timespan2, "[[", 2), format = "%Y%m%dT%H%M%S"),
                          stringsAsFactors = FALSE, row.names = NULL)
    # sort newest files first, by satellite
    catalog <- catalog[with(catalog, order(mission, -as.integer(start))), ]

    if (!past) { # discard the files that contain only past acquisition plans
        iskon <- Sys.Date()
        catalog <- subset(catalog, finish >= iskon)
    }

    # download the KML files
    lst <- catalog$url
    url_kml_prefix <- 'https://sentinel.esa.int'
    from <- paste0(url_kml_prefix, lst)
    path <- tempfile("kml")
    dir.create(path)
    to <- sub('/documents/[[:digit:]]+/[[:digit:]]+/Sentinel', 'Sentinel', lst)
    to <- paste(path, to, sep = "/")
    N <- length(from)
    for (i in 1:N) {
        download.file(from[i], to[i], mode = "wb", quiet = !verbose)
    }

    # compile all files in single sf data.frame
    S2_current_acquisition_plans <- NULL
    for (i in 1:N) {
        fic <- to[i]
        if (verbose) cat(sprintf("Processing file %s ...", basename(fic)), sep = "\n")
        x <- ParseKML(fic)
        if (is.null(S2_current_acquisition_plans)) {
            S2_current_acquisition_plans <- x
        } else {
            # include only missing observations
            ndx <- setdiff(x$ID, S2_current_acquisition_plans$ID)
            S2_current_acquisition_plans <- rbind(S2_current_acquisition_plans, subset(x, ID %in% ndx))
        }
    }

    # discard the (useless) Z dimension from KML files
    S2_current_acquisition_plans <- sf::st_zm(S2_current_acquisition_plans)

    if (keep) {
        assign("S2_current_acquisition_plans", S2_current_acquisition_plans, envir = globalenv())
    }

    invisible(S2_current_acquisition_plans)
}


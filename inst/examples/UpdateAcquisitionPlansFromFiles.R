UpdateAcquisitionPlansFromFiles <-
function(filename, path = "/home/zivan/Sentinel", quiet = TRUE, keep = TRUE)
{
    if (missing(filename)) {
        filename <- system.file("/datasets/S2_current_acquisition_plans.rda", package = "s2toolbox")
    }
    if (file.access(filename, 2) < 0) {
        stop("can't save result, process aborted")
    }

    if (is.na(match("LIBKML", sf::st_drivers()$name))) {
        stop("LIBKML driver not found, process aborted")
    }

    to <- list.files(path, pattern = "*.kml", full.names = TRUE)

    N <- length(to)

    namespaceImportFrom(.GlobalEnv, vars = "%>%", ns = "magrittr")

    S2_current_acquisition_plans <- NULL

    for (i in 1:N) {
        fic <- to[i]
        out <- sub("Sentinel-", "S", sub("MP_ACQ_KML_", "", fic))
        MassageKML(fic, out)
        layers <- sf::st_layers(out)
        idx <- which(layers$features > 0)
        big <- NULL
        nidx <- length(idx)
        blst <- list()
        # for (j in idx) {
        for (k in 1:nidx) {
            j <- idx[k]
            plan <- sf::st_read(out, layer = layers$name[j], stringsAsFactors = FALSE)
            plan.sp <- sf::as_Spatial(sf::st_zm(plan))
            # fix for the rbin issue with the integer attr(., "row.names")
            attr(plan.sp@data, "row.names") <- row.names(plan.sp@data)
            blst[[k]] <- plan.sp
        }
        names(blst) <- layers$name[idx]
        big <- do.call(rbind, blst)
        # drop useless columns
        big@data <- big@data[, names(big@data)[grepl("^[[:upper:]].*$", names(big@data))]]
        big@data[, "Mission"] <- substring(basename(fic), 1, 11)
        big@data[, "KML"] <- basename(fic)
        if (!all(sapply(big@polygons, slot, "ID") == row.names(big@data))) {
            stop("polygons ID slots and data.frame row.names don't match")
        }
        if (is.null(S2_current_acquisition_plans)) {
            S2_current_acquisition_plans <- big
        } else {
            S2_current_acquisition_plans <- rbind(S2_current_acquisition_plans, big)
        }
    }

    if (keep) {
        assign("S2_current_acquisition_plans", S2_current_acquisition_plans, envir = globalenv())
    }

    save(S2_current_acquisition_plans, file = filename)
}


# library(sf)
# library(xml2)
# library(tidyverse)


MassageKML <-
function(old, new)
{
    kml <- xml2::read_xml(old)
    xml2::xml_ns_strip(kml)
    idx <- 0
    xml2::xml_find_all(kml, ".//Folder/name") %>%
        purrr::walk(~{
            idx <<- idx + 1
            xml2::xml_text(.x) <- sprintf("%s-%s", idx, xml2::xml_text(.x))
        })
    xml2::write_xml(kml, new)
}


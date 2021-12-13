.onLoad <-
function(libname, pkgname)
{
    filename <- "~/.s2toolbox/S2_current_acquisition_plans.rda"
    # if file exists and is readable, load it
    if (file.access(filename, mode = 4) == 0) load(filename, envir = .GlobalEnv)
    # test if the plans are out of date
    lastDay <- max(as.Date(S2_current_acquisition_plans$ObservationTimeStart, format = "%Y-%m-%dT%H:%M:%S"))
    if (lastDay < Sys.Date()) {
        warning(paste(
            "The local acquisition plans seem out of date,",
            "you should get a new version calling UpdateCurrentAcquisitionPlans")
        )
    }
}

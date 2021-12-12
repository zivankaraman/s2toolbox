.onLoad <-
function(libname, pkgname)
{
    filename <- system.file("/datasets/S2_current_acquisition_plans.rda", package = "s2toolbox")
    # if (file.access(filename, mode = 0) == 0) load(filename, envir = .GlobalEnv)
}

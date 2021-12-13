
# start sinew
library(sinew)
sinew::sinew_opts$set(add_fields = c("details", "examples", "seealso", "export", "source"))

# roxygenize source files
# ans <- sinew::makeOxyFile(input = "R", overwrite = TRUE, verbose = FALSE, print = FALSE)

# fichiers <- list.files("R", full.names = TRUE)
# for (i in 1:length(fichiers)) sinew::rmOxygen(fichiers[i])
# sinew::rmOxygen("./R/internals.R")

fichiers <- list.files("R", full.names = TRUE)
fichiers <- setdiff(fichiers, c("R/internals.R", "R/zzz.R"))
for (i in 1:length(fichiers)) {
    ans <- sinew::makeOxyFile(input = fichiers[i], overwrite = TRUE, verbose = FALSE, print = FALSE)
}



# remove old Rd files
rc <- unlink("./man", recursive = TRUE, force = TRUE)

# generate new Rd files
devtools::document()

# build and install initial version of the package to enable self referencing namespace
devtools::install(pkg = ".", dependencies = FALSE)

# build pdf manual
devtools::build_manual()

# build vignettes
devtools::build_vignettes(dependencies = FALSE, quiet = FALSE)

ans <- sinew::makeOxyFile(input = "R/GetHighResolutionImage.R", overwrite = FALSE, verbose = TRUE, print = TRUE)
ans <- sinew::makeOxyFile(input = "R/GetImageryFilesList.R", overwrite = FALSE, verbose = TRUE, print = TRUE)
ans <- sinew::makeOxyFile(input = "R/SearchImages.R", overwrite = FALSE, verbose = TRUE, print = TRUE)

knitr::purl("inst/doc/UsingSpectator.Rmd", output = "inst/doc/UsingSpectator.R", documentation = 1L)


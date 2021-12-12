
# start sinew
library(sinew)
sinew::sinew_opts$set(add_fields = c("details", "examples", "seealso", "export", "source"))

# roxygenize source files
ans <- sinew::makeOxyFile(input = "R", overwrite = TRUE, verbose = FALSE, print = FALSE)
sinew::rmOxygen("./R/internals.R")

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


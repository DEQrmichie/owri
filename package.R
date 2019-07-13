# Run this to update the package

library(devtools)
library(roxygen2)

setwd("E:/GitHub/")
create_package("E:/GitHub/owri")

setwd("E:/GitHub/owri/owri")
devtools::document()
setwd("..")
install("owri", upgrade = "never")

library(owri)

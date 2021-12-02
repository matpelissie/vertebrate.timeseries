#############################
#
# 02/12/2021 mathieu.pelissie@ens-lyon.fr
#
# Development file lines run only once
#
# 00_development.R
#
#############################

renv::init()
renv::install("gsl@2.1.5")


renv::status()
renv::snapshot()
# rrtools::use_compendium("../vertebrate.timeseries/", open=FALSE)

usethis::use_package("here") # add package/dependency as imports in DESCRIPTION
usethis::use_package("readr")
usethis::use_package("dplyr")
usethis::use_package("envirem")
usethis::use_package("gsl")
usethis::use_package("maps")
usethis::use_package("raster")
usethis::use_package("rgdal")
usethis::use_package("rgeos")
usethis::use_package("sp")
usethis::use_package("tidyr")
usethis::use_package("viridis")

renv::restore() # lorsque l'on pull le renv.lock

### climate data folder
dir.create("data/CHELSA")
dir.create("data/CHELSA/global")
dir.create("manuscript")

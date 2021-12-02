#############################
#
# Development file
#
# lines run only once
#
# development.R
#
# mathieu.pelissie@ens-lyon.fr
#
#############################

renv::init()
renv::install("readr")


renv::status()
renv::snapshot()
# rrtools::use_compendium("../vertebrate.timeseries/", open=FALSE)

usethis::use_package("here") # add package/dependency as imports in DESCRIPTION

renv::restore() # lorsque l'on pull le renv.lock


### climate data folder
dir.create("data/CHELSA")

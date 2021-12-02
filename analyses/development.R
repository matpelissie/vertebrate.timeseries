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
renv::install()


renv::status()
renv::snapshot()
rrtools::use_compendium("../vertebrate.timeseries/", open=FALSE)

renv::restore() # lorsque l'on pull le renv.lock

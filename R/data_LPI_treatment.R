################################################################################
## Vertebrate time series
## 29/11/2021
## Fonctions
################################################################################

# Renseigner les packages utilises ----------------------------------------
# usethis::use_package("readr") # implementation automatique dans DESCRIPTION
# usethis::use_package("here")


# Import time series data -------------------------------------------------

data_lpi <- function() {

  readr::read_csv(
    here::here("data","LPIdata_Feb2016.csv") # creation du chemin relatif
  )

}

################################################################################
## Vertebrate time series
## 29/11/2021
## Fonctions
################################################################################

# Renseigner les packages utilises ----------------------------------------
# usethis::use_package("readr") # implementation automatique dans DESCRIPTION
# usethis::use_package("here")


#' Import time series data
#'
#' @return A tibble containing raw data
#' @export
#'
data_lpi <- function() {

  readr::read_csv(
    here::here("data","LPIdata_Feb2016.csv")
  )

}



#' Draw a basic world map
#'
#' @return a ggplot showing the geographical distribution of the time series
#' @export
#'
drawWorld<-function() {

  world_map<-map_data("world")

  g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
  g1<-g1+geom_polygon(data=world_map, aes(x=long, y=lat, group=group), colour="gray60", fill="gray60")
  g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
               panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
               axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
  return(g1)
}


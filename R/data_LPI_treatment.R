################################################################################
## Vertebrate time series
## 29/11/2021
## Fonctions
################################################################################

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
drawWorld <- function() {

  world_map<-map_data("world")

  g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
  g1<-g1+geom_polygon(data=world_map, aes(x=long, y=lat, group=group), colour="gray60", fill="gray60")
  g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
               panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
               axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
  return(g1)

}



#' Adding raster values in the global output dataframe
#'
#' @param ras_dat a dataframe containing raster values
#' @param lpi_dat a dataframe containing models output
#'
#' @return a dataframe compiling models outputs and raster values
#' @export
#'
rasterValue <- function(ras_dat,lpi_dat){

  new_dat <- left_join(lpi_dat,ras_dat,by=c("lat","long"))
  return (new_dat)

}



#' Download a raw temperature file from CHELSA climate
#'
#' @param URL a string corresponding to a URL
#'
#' @return No return value
#' @export
#'

download <- function (URL) {

  download.file(URL, destfile = paste0("data/CHELSA/global/", strsplit(as.character(URL), "/")[[1]][10]),
                method = "wget", extra = "-r -p --random-wait")

  invisible(NULL)
}

#' Define temperature column name
#'
#' @param file a string corresponding to the path of a downloaded temperature file
#'
#' @return A short name of the temperature dataset in the form of tasmax/tasmin.month.year
#' @export
#'
temp_name <- function (file) paste(strsplit(file, "_")[[1]][2:4], collapse = ".")



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
#' @return No return value.
#' @export
#'
download_temp <- function (URL) {

  download.file(URL, destfile = paste0("data/CHELSA/global/", strsplit(as.character(URL), "/")[[1]][10]),
                method = "wget",
                 extra = "-r -p --random-wait")

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


#' Extract temperature values from survey sites only
#'
#' @param file a string corresponding to the path of a downloaded temperature file
#'
#' @return a data.frame with average temperature from survey sites only
#' @export
#'
extract_values <- function (file) {

  r <- raster::raster(file)
  temp <- raster::extract(r, LPI.coord, df = TRUE, cellnumber=TRUE)
  t <- cbind(temp, raster::coordinates(r)[temp[,2],])
  temp_sites <- t %>%
    as.data.frame() %>%
    dplyr::rename(lat = "y",
                  long = "x",
                  temperature = dplyr::starts_with("CHELSA")) %>%
    dplyr::mutate(temperature = (temperature/10)-273.15) %>%
    dplyr::rename_with(.fn = ~temp_name(file), .cols = temperature)

  return(temp_sites)
}

#' Save monthly site temperature in a csv
#'
#' @param file a string corresponding to the path of a downloaded temperature file
#' @param temp_sites the corresponding data frame with average temperature from survey sites only
#'
#' @return No return value.
#' @export

save_temp_file <- function (file, temp_sites) {

  path_to_file <- paste0("data/CHELSA/sites/", temp_name(file), ".csv")

  readr::write_csv(temp_sites, path_to_file)

  return(path_to_file)

}
#' Run download, process, save, and delete loops
#'
#' @param f a list of strings corresponding to URLs of files to download
#'
#' @return No return value.
#' @export
#'
temp_extract <- function (f) {


  files <- NULL

  for (i in 1:length(f)){
    download_temp(f[i])
    file <- list.files("data/CHELSA/global", full.names = TRUE)[1]
    temp_sites <- extract_values(file)
    files [i] <- save_temp_file(file, temp_sites)
    unlink(file)
    gc(verbose = FALSE)
  }

  return(files)
}


#' Merge monthly csv into one
#'
#' @param tas a list of strings corresponding to URLs of files to download
#'
#' @return A dataframe
#' @export
#'
merge_values <- function (tas) {

  if (!tas %in% c("tasmax","tasmin")) {
    stop("tas must be either tasmax or tasmin")
  }

  tabs <- list.files("data/CHELSA/sites", pattern = tas, full.names = TRUE)

  tab_i <- readr::read_csv(tabs[1]) %>%
    dplyr::relocate(dplyr::starts_with("tas"), .after = lat)

  for (j in 2:length(tabs)){
    tab_j <- readr::read_csv(tabs[j]) %>%
      dplyr::select(ID, dplyr::starts_with("tas"))
    merge <- tab_i %>% left_join(tab_j, by = "ID")
    tab_i <- merge
  }

  return(merge)
}


# average the 3 highest or lowest month temperatures
temp_extremes <- function (tas,year) {

  if (deparse(quote(tas))=="tasmax") ord <- TRUE
  if (deparse(quote(tas))=="tasmin") ord <- FALSE

  temp_extremes <- tas %>%
    dplyr::select(dplyr::contains("ID") | dplyr::contains("long") |dplyr::contains("lat") | dplyr::contains(as.character(year))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rows = list(sort(dplyr::c_across(-c(ID, long, lat)), decreasing = ord))) %>%
    dplyr::mutate(top_1 = rows[1], top_2 = rows[2], top_3 = rows[3]) %>%
    dplyr::mutate("mean_tas_{year}" := mean(top_1,top_2,top_3)) %>%
    dplyr::select(ID, long, lat, dplyr::contains("mean"))
  return(temp_extremes)
}

# compute temperature difference between 1980 and 2010 for all sites
temp_diff <- function (tasmax, tasmin) {

  temp_diff_tasmax <- temp_extremes(tasmax,1980) %>%
    dplyr::left_join(dplyr::select(temp_extremes(tasmax, 2010), -long, -lat), by = "ID") %>%
    dplyr::mutate(temp_diff = mean_tasmax_2010 - mean_tasmax_1980) %>%
    dplyr::mutate(temp_diff_sign = sign(temp_diff)) %>%
    dplyr::rename(temp_diff_tasmax = "temp_diff",
                  temp_diff_sign_tasmax = "temp_diff_sign")

  temp_diff_tasmin <- temp_extremes(tasmin,1980) %>%
    dplyr::left_join(dplyr::select(temp_extremes(tasmin, 2010), -long, -lat), by = "ID") %>%
    dplyr::mutate(temp_diff = mean_tasmin_2010 - mean_tasmin_1980) %>%
    dplyr::mutate(temp_diff_sign = sign(temp_diff)) %>%
    dplyr::rename(temp_diff_tasmin = "temp_diff",
                  temp_diff_sign_tasmin = "temp_diff_sign")

  temp_diff <- temp_diff_tasmax %>%
    dplyr::left_join(dplyr::select(temp_diff_tasmin, -long, -lat), by = "ID")
  return(temp_diff)
}

#' Join LPI and temperature dataframes
#'
#' @param LPI.coords a tibble with LPI data for retained sites
#' @param temp_diff a tibble with temperature data and change between 1980 and 2010 for retained sites
#'
#' @return A tibble
#' @export
#'


LPI_env <- function (LPI.coords, temp_diff) {

  # r <- raster::raster("data/CHELSA/global/CHELSA_tasmax_01_1980_V.2.1.tif")
  # r[] <- NA
  # raster::writeRaster(r, "data/CHELSA/template.t"if)
  # unlink("data/CHELSA/global/CHELSA_tasmax_01_1980_V.2.1.tif")
  r <- raster::raster("data/CHELSA/template.tif")
  LPI.coords <- LPI.models %>% dplyr::select(long, lat)
  rast <- raster::rasterize(LPI.coords, r)
  temp <- raster::extract(rast, LPI.coords, df = TRUE, cellnumber=TRUE)
  t <- temp %>%
    cbind(raster::coordinates(rast)[temp[,2],]) %>%
    dplyr::select(x,y) %>%
    dplyr::rename(long_r = "x",
                  lat_r = "y") %>%
    tibble::as_tibble() %>%
    bind_cols(LPI.models)

  LPI_env <- left_join(t, temp_diff, by = c("long_r", "lat_r"))

  return(LPI_env)

}

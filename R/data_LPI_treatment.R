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
data_lpi <- function(path) {

  readr::read_csv(
    path,
    col_types = readr::cols()
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



#' Is useful? Adding raster values in the global output dataframe
#'
#' @param ras_dat a dataframe containing raster values
#' @param lpi_dat a dataframe containing models output
#'
#' @return a dataframe compiling models outputs and raster values
#' @export
#'
# rasterValue <- function(ras_dat, lpi_dat){
#
#   new_dat <- left_join(lpi_dat, ras_dat, by = c("lat", "long"))
#
#   return (new_dat)
# }



#' Define temperature column name
#'
#' @param file a string corresponding to the path of a downloaded temperature file
#'
#' @return A short name of the temperature dataset in the form of tasmax/tasmin.month.year
#' @export
#'
temp_name <- function (file) paste(strsplit(file, "_")[[1]][2:4], collapse = ".")



#' Save monthly site temperature in a csv
#'
#' @param file a string corresponding to the path of a downloaded temperature file
#' @param temp_sites the corresponding data frame with average temperature from survey sites only
#'
#' @return A string corresponding to the path of the csv
#' @export
#'
save_temp_file <- function (file, temp_sites) {

  path_to_file <- paste0("data/CHELSA/sites/", temp_name(file), ".csv")

  readr::write_csv(temp_sites, path_to_file)

  return(path_to_file)
}



#' Download a raw temperature file from CHELSA climate
#'
#' @param URL a string corresponding to a URL
#'
#' @return No return value.
#' @export
#'
download_temp <- function (URL) {

  utils::download.file(URL, destfile = paste0("data/CHELSA/global/",
                                              strsplit(as.character(URL), "/")[[1]][10]),
                       method = "wget",
                       extra = "-r -p --random-wait")

  invisible(NULL)
}



#' Extract temperature values from survey sites only
#'
#' @param file a string corresponding to the path of a downloaded temperature file
#'
#' @return A data.frame with average temperature from survey sites only
#' @export
#'
extract_values <- function (file, LPI.coord) {

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

  if (file == "data/CHELSA/global/CHELSA_tasmax_01_1980_V.2.1.tif"){
    r[] <- NA
    raster::writeRaster(r, "data/CHELSA/template.tif", overwrite = TRUE)
  }

  return(temp_sites)
}



#' Run download, process, save, and delete loops
#'
#' @param f a list of strings corresponding to URLs of files to download
#'
#' @return A list of strings corresponding to the path to the csv files generated
#' @export
#'
temp_extract <- function (f, LPI.coord) {

  files <- NULL

  for (i in 1:length(f)){
    download_temp(f[i])
    file <- list.files("data/CHELSA/global", full.names = TRUE)[1]
    temp_sites <- extract_values(file, LPI.coord)
    files [i] <- save_temp_file(file, temp_sites)
    unlink(file)
    gc(verbose = FALSE)
  }

  return(files)
}



#' Merge monthly csv into one
#'
#' @param tas a string either "tasmin" or "tasmax" to indicate if min or max
#' month temperatures to merge
#'
#' @return A tibble with site IDs, coordinates and min or max month temperature
#' for each site and years
#' @export
#'
merge_values <- function (raw_temp_files, tas) {

  if (!tas %in% c("tasmax","tasmin")) {
    stop("tas must be either tasmax or tasmin")
  }

  # tabs <- list.files("data/CHELSA/sites", pattern = tas, full.names = TRUE)
  tabs <- grep(raw_temp_files, pattern = tas, value = TRUE)

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



#' Average the 3 highest maximum month temperatures
#'
#' @param tasmax a tibble obtained as output of the merge_values function with
#' maximum month temperature for each site and years
#' @param year an integer corresponding to the year for which to average
#' extreme month temperatures
#'
#' @return A tibble with site IDs, coordinates and mean of maximum month
#' temperatures for the year defined
#' @export
#'
temp_max <- function (tasmax, year) {

  temp_max_data <- tasmax %>%
    dplyr::select(dplyr::contains("ID") | dplyr::contains("cells") | dplyr::contains("long") |
                    dplyr::contains("lat") | dplyr::contains(as.character(year))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rows = list(sort(dplyr::c_across(-c(ID, cells, long, lat)), decreasing = TRUE))) %>%
    dplyr::mutate(top_1 = rows[1], top_2 = rows[2], top_3 = rows[3]) %>%
    dplyr::mutate("mean_tasmax_{year}" := mean(top_1,top_2,top_3)) %>%
    dplyr::select(ID, cells, long, lat, dplyr::contains("mean"))

  return(temp_max_data)
}

#' Average the 3 lowest minimum month temperatures
#'
#' @param tasmin a tibble obtained as output of the merge_values function with
#' minimum month temperature for each site and years
#' @param year an integer corresponding to the year for which to average
#' extreme month temperatures
#'
#' @return A tibble with site IDs, coordinates and mean of minimum month
#' temperatures for the year defined
#' @export
#'
temp_min <- function (tasmin, year) {

  temp_min_data <- tasmin %>%
    dplyr::select(dplyr::contains("ID") | dplyr::contains("cells") | dplyr::contains("long") |
                    dplyr::contains("lat") | dplyr::contains(as.character(year))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rows = list(sort(dplyr::c_across(-c(ID, cells, long, lat)), decreasing = FALSE))) %>%
    dplyr::mutate(top_1 = rows[1], top_2 = rows[2], top_3 = rows[3]) %>%
    dplyr::mutate("mean_tasmin_{year}" := mean(top_1,top_2,top_3)) %>%
    dplyr::select(ID, cells, long, lat, dplyr::contains("mean"))

  return(temp_min_data)
}

#' Compute temperature difference between 1980 and 2010 for all sites
#'
#' @param tasmax a tibble obtained as output of the temp_extremes function with
#' site IDs, coordinates and mean of the three highest month temperatures
#' @param tasmin a tibble obtained as output of the temp_extremes function with
#' site IDs, coordinates and mean of the three lowest month temperatures
#'
#' @return A tibble with site IDs, coordinates, averaged minimum and maximum
#' month temperature change between 1980 and 2010
#' @export
#'
temp_diff <- function (tasmax, tasmin) {

  temp_diff_tasmax <- temp_max(tasmax,1980) %>%
    dplyr::left_join(dplyr::select(temp_max(tasmax, 2010), -long, -lat, -cells), by = "ID") %>%
    dplyr::mutate(temp_diff = mean_tasmax_2010 - mean_tasmax_1980) %>%
    dplyr::mutate(temp_diff_sign = sign(temp_diff)) %>%
    dplyr::rename(temp_diff_tasmax = "temp_diff",
                  temp_diff_sign_tasmax = "temp_diff_sign")

  temp_diff_tasmin <- temp_min(tasmin,1980) %>%
    dplyr::left_join(dplyr::select(temp_min(tasmin, 2010), -long, -lat, -cells), by = "ID") %>%
    dplyr::mutate(temp_diff = mean_tasmin_2010 - mean_tasmin_1980) %>%
    dplyr::mutate(temp_diff_sign = sign(temp_diff)) %>%
    dplyr::rename(temp_diff_tasmin = "temp_diff",
                  temp_diff_sign_tasmin = "temp_diff_sign")

  temp_data <- temp_diff_tasmax %>%
    dplyr::left_join(dplyr::select(temp_diff_tasmin, -long, -lat, -cells), by = "ID") %>%
    dplyr::rename(long_r = "long",
                  lat_r = "lat")

  return(temp_data)
}



#' Join LPI and temperature data frames
#'
#' @param LPI.mod a tibble with LPI data for selected sites
#' @param temp_data a tibble with temperature data and change between 1980 and
#' 2010 for selected sites
#'
#' @return A tibble gathering LPI population trends and temperaure change
#' @export
#'
LPI_temp <- function (LPI.mod, temp_data) {

  r <- raster::raster("data/CHELSA/template.tif")
  LPI.coords <- LPI.mod %>% dplyr::select(long, lat)
  rast <- raster::rasterize(LPI.coords, r)
  temp <- raster::extract(rast, LPI.coords, df = TRUE, cellnumber=TRUE)
  LPI <- temp %>%
    cbind(raster::coordinates(rast)[temp[,2],]) %>%
    dplyr::select(x, y, cells) %>%
    dplyr::rename(long_r = "x",
                  lat_r = "y") %>%
    tibble::as_tibble() %>%
    bind_cols(LPI.mod)

  temp_data_filter <- dplyr::select(temp_data, -ID, -long_r, -lat_r) %>%
    dplyr::distinct()

  LPI_temp_data <- dplyr::inner_join(LPI, temp_data_filter, by = "cells")

  return(LPI_temp_data)

}

#' Population vs. temperature change
#'
#' @param LPI_temp_data a tibble gathering LPI population trends and temperaure change
#'
#' @return A summary of the multiple regression analysis performed
#' @export
#'
LPI_temp_model <- function(LPI_temp_data){

  LPI_temp_filt <- LPI_temp_data %>% dplyr::filter(abs(slope)<100
                                                   # , Class == "Amphibia"
                                                   )
    # interpretation very dependent on threshold
  fit <- lm(slope ~ temp_diff_tasmax + temp_diff_tasmin +
               mean_tasmax_2010 + mean_tasmin_2010, data = LPI_temp_filt)
  summ <- anova(fit)

  # summary(fit)
  # fit1 <- lm(slope ~ temp_diff_tasmax, data = LPI_temp_filt)
  # plot(slope ~ temp_diff_tasmax, data = LPI_temp_filt)
  # abline(fit1)

  return(summ)
}

#' Map of temperatures
#'
#' @param temp_var a string corresponding to the temperature variable to display
#' @param LPI_temp_data a tibble gathering LPI population trends and temperaure change
#'
#' @return A world map with the chosen temperature variable displayed
#' @export
#'

map_temp <- function(temp_var, LPI_temp_data){

  if (temp_var == "mean_tasmin_1980") title <- "Figure 2: Coldest minimal temperatures in 1980."
  if (temp_var == "mean_tasmax_1980") title <- "Figure 3: Hottest maximal temperatures in 1980."
  if (temp_var == "temp_diff_tasmin") title <- "Figure 4: Change in coldest minimal temperatures between 1980 and 2010."
  if (temp_var == "temp_diff_tasmax") title <- "Figure 5: Change in hottest maximal temperatures between 1980 and 2010."


  drawWorld()+
    geom_point(data = LPI_temp_data,
               aes(x = long, y = lat, color = get(temp_var)),
               alpha = I(0.7))+
    scale_colour_gradient2(low = "blue", high = "red") +
    labs(color = "Temperature (°C)") +
    theme(plot.title = element_text(size = 12, face = "bold.italic"),
          legend.position = "bottom") +
    guides(size = "none") +
    labs(title = title)

}

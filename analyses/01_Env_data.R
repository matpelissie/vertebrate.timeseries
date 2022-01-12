############################
#
# 02/12/2021 mathieu.pelissie@ens-lyon.fr
#
# Climate data retrieval
#
# 01_Env_data.R
#
############################

source(here::here("R","data_LPI_treatment.R"))
library(tidyverse)

# read URL paths to data
f <- read.table("data/CHELSA/envidatS3paths_light_temp.txt") ; f <- as.character(f$V1)

# coordinates of survey sites
LPI.coord <- readRDS("data/CHELSA/LPI.coord.rds")

# download a raw temperature file
download_temp <- function (URL) {

  download.file(URL, destfile = paste0("data/CHELSA/global/", strsplit(as.character(URL), "/")[[1]][10]),
                method = "wget", extra = "-r -p --random-wait")

  invisible(NULL)
  }

# define column name (max/min.month.year)
temp_name <- function (file) paste(strsplit(file, "_")[[1]][2:4], collapse = ".")

# extract only temperature values from survey sites
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
    dplyr::rename_with(.fn = ~temp_name(file), .cols = temperature) %>%
    dplyr::distinct()

  return(temp_sites)
}

# save monthly site temperature in a csv
save_temp_file <- function (file, temp_sites) {

  path_to_file <- paste0("data/CHELSA/sites/", temp_name(file), ".csv")

  readr::write_csv(temp_sites, path_to_file)

  return(path_to_file)

}

# run download, process, save, and delete loops
temp_extract <- function (f, LPI.coord) {

  files <- NULL

  for (i in 1:length(f)){
    download_temp(f[i])
    file <- list.files("data/CHELSA/global", full.names = TRUE)[1]
    temp_sites <- extract_values(file, LPI.coord)
    files[i] <- save_temp_file(file, temp_sites)
    unlink(file)
    gc(verbose = FALSE)
  }

  return(files)
}

# temp_extract(f)

# merge monthly csv into one
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

tasmax <- merge_values("tasmax")
tasmin <- merge_values("tasmin")

# average the 3 highest or lowest month temperatures
temp_extremes <- function (tas, year) {

  tas_str <- deparse(substitute(tas))
  if (tas_str=="tasmax") ord <- TRUE
  if (tas_str=="tasmin") ord <- FALSE

  temp_extremes_data <- tas %>%
    dplyr::select(dplyr::contains("ID") | dplyr::contains("cells") | dplyr::contains("long") |dplyr::contains("lat") | dplyr::contains(as.character(year))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rows = list(sort(dplyr::c_across(-c(ID, cells, long, lat)), decreasing = ord ))) %>%
    dplyr::mutate(top_1 = rows[1], top_2 = rows[2], top_3 = rows[3]) %>%
    dplyr::mutate("mean_{tas_str}_{year}" := mean(top_1,top_2,top_3)) %>%
    dplyr::select(ID, cells, long, lat, dplyr::contains("mean"))
  return(temp_extremes_data)
}

# compute temperature difference between 1980 and 2010 for all sites
temp_diff <- function () {

  temp_diff_tasmax <- temp_extremes(tasmax,1980) %>%
    dplyr::left_join(dplyr::select(temp_extremes(tasmax, 2010), -long, -lat, -cells), by = "ID") %>%
    dplyr::mutate(temp_diff = mean_tasmax_2010 - mean_tasmax_1980) %>%
    dplyr::mutate(temp_diff_sign = sign(temp_diff)) %>%
    dplyr::rename(temp_diff_tasmax = "temp_diff",
                  temp_diff_sign_tasmax = "temp_diff_sign")

  temp_diff_tasmin <- temp_extremes(tasmin,1980) %>%
    dplyr::left_join(dplyr::select(temp_extremes(tasmin, 2010), -long, -lat, -cells), by = "ID") %>%
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
temp_data <- temp_diff()

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

LPI_temp_data <- LPI_temp(LPI.mod, temp_data)

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

# save monthly site temperature in a csv
save_temp_file <- function (file, temp_sites) {

  readr::write_csv(temp_sites, paste0("data/CHELSA/sites/", temp_name(file), ".csv"))

  invisible(NULL)

}

# run download, process, save, and delete loops
temp_extract <- function (f) {

  for (i in 21:length(f)){ # change back to 1
    download_temp(f[i])
    file <- list.files("data/CHELSA/global", full.names = TRUE)[1]
    temp_sites <- extract_values(file)
    save_temp_file(file, temp_sites)
    unlink(file)
    gc(verbose = FALSE)
  }

  invisible(NULL)
}

temp_extract(f)

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

temp_extremes(tasmax, 1980)

# compute temperature difference between 1980 and 2010 for all sites
temp_diff <- function (tas) {

  temp_diff <- temp_extremes(tas,1980) %>%
    dplyr::left_join(dplyr::select(temp_extremes(tas, 2010), -long, -lat), by = "ID") %>%
    dplyr::mutate(temp_diff = mean_tas_2010 - mean_tas_1980) %>%
    dplyr::mutate(temp_diff_sign = sign(temp_diff))

  return(temp_diff)
}

temp_diff_max <- temp_diff(tasmax)
temp_diff_min <- temp_diff(tasmin)


# plot changes
points<-drawWorld()+
  geom_point(data=temp_diff_min,
             aes(x=long, y=lat, size=temp_diff, col = temp_diff_sign),
             alpha=I(0.7))+
  scale_size_continuous(range=c(0.1,5))
points

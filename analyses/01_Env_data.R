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
destfile <- "data/CHELSA/global/"

# coordinates of survey sites
LPI.coord <- readRDS("data/CHELSA/LPI.coord.rds")

# download a raw temperature file
download <- function (file) {

  download.file(file, destfile = paste0(destfile, strsplit(as.character(file), "/")[[1]][10]),
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

  for (i in 9:length(f)){ # change back to 1
    download(f[i])
    file <- list.files(destfile, full.names = TRUE)[1]
    temp_sites <- extract_values(file)
    save_temp_file(file, temp_sites)
    unlink(file)
    gc(verbose = FALSE)
  }

  invisible(NULL)
}

temp_extract(f)


merge_values <- function () {

  tabs <- list.files("data/CHELSA/sites/", full.names = TRUE)

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

temp_average <- function () {


}


#
points<-drawWorld()+
  geom_point(data=t,
             aes(x=long, y=lat, size=temperature),
             alpha=I(0.7))+
  scale_size_continuous(range=c(1,5))
points

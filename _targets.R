# _targets.R file
library(targets)
source("R/data_LPI_treatment.R")
library(tidyverse)
library(tarchetypes)

path_to_data <- function() {
  here::here("data","LPIdata_Feb2016.csv")
  # "data/LPIdata_Feb2016.csv"
}
list(
  tar_target(raw_data_file,
             path_to_data(), format = "file"
  ), # Define data path

  tar_target(raw_data,
             data_lpi(raw_data_file)
  ), # Read the data, return a data.frame

  tar_target(raw_data_long_format,
        raw_data %>%
        dplyr::filter(Class == "Mammalia" | Class == "Amphibia" | Class == "Aves" | Class == "Reptilia") %>%
        dplyr::rename(lat = 'Decimal Latitude',
        long = 'Decimal Longitude',
        country = 'Country list') %>%
        tidyr::pivot_longer(26:70, names_to = "year", values_to = "pop") %>%
        dplyr::mutate(year = as.numeric(year))
  ), # Transform from wide to long format

  tar_target(data_long_format_col,
        raw_data_long_format%>%
        dplyr::mutate(species = paste(Genus, Species))
  ), # Create a new column for species

  tar_target(data_long,
             data_long_format_col%>%
        tidyr::drop_na(pop) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(meanpop = mean(pop),
                      minyear = min(year),
                      maxyear = max(year),
                      lengthyear = maxyear - minyear) %>%
        dplyr::filter(minyear <= 1980) %>%
        dplyr::ungroup()
  ), # Drop NAs and calculate length of monitoring

  tar_target(data_models,
             data_long %>%
    group_by(biome, system, country, Class, species, lengthyear, meanpop, lat, long, id) %>%
    do(mod = lm(pop ~ year, data = .)) %>%
    mutate(., n = df.residual(mod),
           intercept = summary(mod)$coeff[1],
           slope = summary(mod)$coeff[2],
           intercept_se = summary(mod)$coeff[3],
           slope_se = summary(mod)$coeff[4],
           intercept_p = summary(mod)$coeff[7],
           slope_p = summary(mod)$coeff[8]) %>%
    ungroup() %>%
    mutate(id = id,
           biome = biome,
           system = system,
           country = country,
           Class = Class,
           species = species,
           lengthyear = lengthyear,
           meanpop = meanpop,
           lat = lat,
           long = long)
  ), # Linear models of abundance trends over time for each population and extract model coefficients

  tar_target(data_mod,
             data_models %>%
               tidyr::drop_na(slope) %>%
               tidyr::drop_na(slope_p)
  ), # Count trends

  tar_target(map_color,
             c('#abdda4','#fdae61')
  ), # Color for mapping

  tar_target(map_trends,
      drawWorld()+
      geom_point(data = data_mod,
                 aes(x = long, y = lat, color = slope<0, size = abs(slope)),
                 alpha = I(0.7))+
      scale_size_continuous(range = c(1,5))+
      theme(plot.title = element_text(size = 12, face = "bold.italic"),
            legend.position = "bottom") +
      guides(size = "none") +
      scale_colour_manual(name = "Slope", labels = c("increase", "decrease"), values = map_color)+
      labs(title = "Figure 1: Terrestrial vertebrates population  declines and  increases  worldwide. ")
  ), # Map the trends

  tar_target(mod_biome,
             lm(slope ~ biome, data = data_mod)
  ), # Model for biomes impacts

  tar_target(p_mod_biome,
             anova(mod_biome)[1,5]
  ), # Testing biomes impacts

  tar_target(mod_class,
             lm(slope ~ Class, data = data_mod)
  ), # Model for class impacts

  tar_target(p_mod_class,
             anova(mod_class)[1,5]
  ), # Testing class impacts

  tar_target(f.data,
            read.table("data/CHELSA/envidatS3paths_light_temp.txt")
  ), # Read URL paths to data

  tar_target(f.data_character,
             as.character(f.data$V1)
  ), # Data as character

  tar_target(LPI.coord,
             readRDS("data/CHELSA/LPI.coord.rds")
    ), # Coordinates of survey sites

  tar_target(raw_temp_files,
             temp_extract(f.data_character, LPI.coord),
             format = "file"
  ), # Download a raw temperature file

  tar_target(tasmax,
            merge_values(raw_temp_files, "tasmax")
  ), # Merge maximum monthly temperature csv into one

  tar_target(tasmin,
             merge_values(raw_temp_files, "tasmin")
  ), # Merge minimum monthly temperature csv into one

  tar_target(temp_data,
             temp_diff(tasmax, tasmin)
  ), # Average the 3 highest or lowest month temperatures

  tar_target(LPI_temp_data,
             LPI_temp(data_mod, temp_data)
  ), # Join two env and species datasets

  # tar_target(mod_tmax,
  #            lm(slope ~ temp_diff_tasmax, data = LPI_temp_data)
  # ), #
  #
  # tar_target(p_mod_tmax,
  #            anova(mod_tmax)[1,5]
  # ), #

  # tar_target(mod_tmin,
  #            lm(slope ~ temp_diff_tasmin, data = LPI_temp_data)
  # ), # Tmax impact
  #
  # tar_target(p_mod_tmin,
  #            anova(mod_tmin)[1,5]
  # ), # Tmin impact

  tar_target(map_temp_min,
             map_temp("mean_tasmin_1980", LPI_temp_data)
  ), # Map of minimal temperatures

  tar_target(map_temp_max,
             map_temp("mean_tasmax_1980", LPI_temp_data)
  ), # Map of maximal temperatures

  tar_target(map_temp_diff_min,
             map_temp("temp_diff_tasmin", LPI_temp_data)
  ), # Map of minimal temperatures change

  tar_target(map_temp_diff_max,
             map_temp("temp_diff_tasmax", LPI_temp_data)
  ), # Map of maximal temperatures change

  tar_target(models,
             LPI_temp_model(LPI_temp_data)
  ), # Make multiple regression model

  tarchetypes::tar_render(manuscript, "manuscript.Rmd"
  ) # Update manuscript

)

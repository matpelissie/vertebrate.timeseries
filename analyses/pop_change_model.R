################################################################################
## Vertebrate time series
## 02/12/2021
## Population change models
################################################################################
source(here::here("R","data_LPI_treatment.R"))
library(tidyverse)

# Load data ---------------------------------------------------------------
LPI <- data_lpi(here::here("data","LPIdata_Feb2016.csv"))

# clean names
LPI <- LPI  %>%
  dplyr::rename(lat='Decimal Latitude',
                long='Decimal Longitude',
                country='Country list')


# Transform from wide to long format --------------------------------------

LPI.long <- LPI %>%
  # vertebrates
  dplyr::filter(Class=="Mammalia" | Class=="Amphibia" | Class=="Aves" | Class=="Reptilia") %>%
  # terrestrial
  dplyr::filter(system=="Terrestrial") %>%
  # wide to long
  tidyr::pivot_longer(26:70,names_to="year",values_to="pop") %>%
  dplyr::mutate(year=as.numeric(year))

# Create new column with genus and species together
LPI.long<-dplyr::mutate(LPI.long, species = paste(Genus, Species))

# Survey coordinates
LPI.coord <- LPI %>%
  dplyr::filter(Class=="Mammalia" | Class=="Amphibia" | Class=="Aves" | Class=="Reptilia") %>%
  dplyr::filter(system=="Terrestrial") %>%
  dplyr::select(long, lat)
saveRDS(LPI.coord, "data/CHELSA/LPI.coord.rds")

# Data manipulation -------------------------------------------------------

# Drop NAs and calculate length of monitoring
LPI.long <- LPI.long %>%
  tidyr::drop_na(pop) %>%
  dplyr::group_by(id) %>%   # group rows so that each group is one population
  dplyr::mutate(meanpop = mean(pop),  # Create column for mean population
         minyear = min(year),
         maxyear = max(year),
         lengthyear = maxyear - minyear) %>%
  dplyr::filter(minyear <= 1980, maxyear>=2010) %>%
  dplyr::ungroup()


# Run models --------------------------------------------------------------

# Run linear models of abundance trends over time for each population and extract model coefficients
LPI.models <- LPI.long %>%
  group_by(biome, system, country, Class, species, lengthyear, meanpop, lat, long, id) %>%
  do(mod = lm(pop ~ year, data = .)) %>%  # Create a linear model for each group
  mutate(., n = df.residual(mod),  # Create columns: degrees of freedom
         intercept = summary(mod)$coeff[1],  # intercept coefficient
         slope = summary(mod)$coeff[2],  # slope coefficient
         intercept_se = summary(mod)$coeff[3],  # standard error of intercept
         slope_se = summary(mod)$coeff[4],  # standard error of slope
         intercept_p = summary(mod)$coeff[7],  # p value of intercept
         slope_p = summary(mod)$coeff[8]) %>%  # p value of slope
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



# Data visualization and statistics ---------------------------------------

# Count trends
LPI.mod <- LPI.models %>%
  tidyr::drop_na(slope) %>%
  tidyr::drop_na(slope_p)


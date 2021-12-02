################################################################################
## Vertebrate time series
## 02/12/2021
##
################################################################################
source(here::here("R","data_LPI_treatment.R"))
# Load data ---------------------------------------------------------------
LPI <- data_lpi()

# Transform from wide to long format --------------------------------------
LPI.long <- LPI %>%
  dplyr::filter(Class=="Mammalia" | Class=="Amphibia" | Class=="Aves" | Class=="Reptilia") %>%
  dplyr::rename(lat='Decimal Latitude',
                long='Decimal Longitude',
                country='Country list') %>%
  tidyr::pivot_longer(26:70,names_to="year",values_to="pop") %>%
  dplyr::mutate(year=as.numeric(year))

# Create new column with genus and species together
LPI.long$species <- paste(LPI.long$Genus, LPI.long$Species)

# ** Data manipulation ----

# Calculate length of monitoring and scale population trend data
LPI.long <- LPI.long %>%
  tidyr::drop_na(pop) %>%
  dplyr::group_by(id) %>%   # group rows so that each group is one population
  dplyr::mutate(meanpop = mean(pop),  # Create column for mean population
         minyear = min(year),
         maxyear = max(year),
         lengthyear = maxyear - minyear) %>%
  dplyr::ungroup()

# Number of species = 2275
length(unique(LPI.long$species))

# Number of populations = 11905
length(unique(LPI.long$id))

# ** Models ----
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



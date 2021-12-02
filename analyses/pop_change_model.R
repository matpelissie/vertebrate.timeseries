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
  dplyr::select(id) %>%
  tidyr::gather(key = "year", value = "pop")

# Get rid of the X in front of years
LPI.long$year <- parse_number(LPI.long$year)

# Create new column with genus and species together
LPI.long$species <- paste(LPI.long$Genus, LPI.long$Species)

# ** Data manipulation ----

# Calculate length of monitoring and scale population trend data
LPI.long <- LPI.long %>%
  drop_na(pop) %>%
  group_by(id) %>%   # group rows so that each group is one population
  mutate(scalepop = rescale(pop, to = c(-1, 1))) %>%
  drop_na(scalepop) %>%
  mutate(meanpop = mean(pop),  # Create column for mean population
         minyear = min(year),
         maxyear = max(year),
         lengthyear = maxyear - minyear) %>%
  ungroup()

# Number of species = 2074
length(unique(LPI.long$species))

# Number of populations = 9288
length(unique(LPI.long$id))

# ** Models ----
# Run linear models of abundance trends over time for each population and extract model coefficients
LPI.models <- LPI.long %>%
  group_by(biome, system, Country.list, Class, species, lengthyear, meanpop, Decimal.Latitude, Decimal.Longitude, id) %>%
  do(mod = lm(scalepop ~ year, data = .)) %>%  # Create a linear model for each group
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
         Country.list = Country.list,
         Class = Class,
         species = species,
         lengthyear = lengthyear,
         meanpop = meanpop,
         Decimal.Latitude = Decimal.Latitude,
         Decimal.Longitude = Decimal.Longitude)

# Number of species = 2074
length(unique(LPI.models$species))

# Number of populations = 9284
length(unique(LPI.models$id))

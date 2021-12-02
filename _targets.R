# _targets.R file
library(targets)
source("R/data_LPI_treatment.R")
list(
  tar_target(raw_data_file, "data/LPIdata_Feb2016.csv", format = "file"
),
 #make the workflow depends on the raw data file
  tar_target(raw_data, data_lpi(raw_data_file)
  ), #read the data, return a data.frame
  tar_target(raw_data_long_format,
        raw_data%>%
        dplyr::filter(Class=="Mammalia" | Class=="Amphibia" | Class=="Aves" | Class=="Reptilia") %>%
        dplyr::rename(lat='Decimal Latitude',
        long='Decimal Longitude',
        country='Country list') %>%
        tidyr::pivot_longer(26:70,names_to="year",values_to="pop") %>%
        dplyr::mutate(year=as.numeric(year))
  ), #Transform from wide to long format
  tar_target(data_long_format_col,
        raw_data_long_format%>%
        dplyr::mutate(species = paste(Genus, Species))
  ), #Create a new column for species
  tar_target(data_coord,
        data_long_format_col %>%
        dplyr::select(lat,long)
  ), #Survey coordinates
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
  ), #Drop NAs and calculate length of monitoring
  tar_target(data.models,
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
  ),#linear models of abundance trends over time for each population and extract model coefficients
  tar_target(data.mod,
             datamodels %>%
               tidyr::drop_na(slope) %>%
               tidyr::drop_na(slope_p)
  ),# Count trends
  tar_target(map_color,
             c('#abdda4','#fdae61')
  ), #color for mapping
  tar_target(map_data,
             drawWorld("y")+
               geom_point(data=data.mod,
                          aes(x=long, y=lat, color=slope<0,size=abs(slope)),
                          alpha=I(0.7))+
               scale_size_continuous(range=c(1,5))+
               scale_colour_manual(values= map_color)
  ) #map the trends
)

################################################################################
## Vertebrate time series
## 02/12/2021
## Population change drivers
################################################################################

t <- LPI_env(LPI.mod,temp_data)


# Testing biomes impacts --------------------------------------------------
mod_biome <- lm(slope ~ biome, data = LPI.mod)
p_mod_biome <- anova(mod_biome)[1,5]

# Testing class impacts ---------------------------------------------------
mod_class <- lm(slope ~ Class, data = LPI.mod)
p_mod_class <- anova(mod_class)[1,5]

# Testing climate change impacts ------------------------------------------
mod_tmax <- lm(slope ~ temp_diff_tasmax, data = LPI.mod) # Tmax impact
p_mod_tmax <- anova(mod_tmax)[1,5]

mod_tmin <- lm(slope ~ temp_diff_tasmin, data = LPI.mod) # Tmin impact
p_mod_tmin <- anova(mod_tmin)[1,5]

################################################################################
## Vertebrate time series
## 02/12/2021
## Population change drivers
################################################################################


# Testing biomes impacts --------------------------------------------------
mod_biome <- lm(slope ~ biome, data = LPI.mod)
p_mod_biome <- anova(mod_biome)[1,5]

# Testing class impacts ---------------------------------------------------
mod_class <- lm(slope ~ Class, data = LPI.mod)
p_mod_class <- anova(mod_class)[1,5]

# Testing climate change impacts ------------------------------------------
mod_v1 <- lm(slope ~ v1, data = LPI.mod)
mod_v2 <- lm(slope ~ v2, data = LPI.mod)
mod_v3 <- lm(slope ~ v3, data = LPI.mod)

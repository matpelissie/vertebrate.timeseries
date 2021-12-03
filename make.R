################################################################################
## Vertebrate time series
## 02/12/2021
## 'make.R
################################################################################

devtools::install_deps()
#devtools::load_all()

# rmarkdown::render(here::here("manuscript", "manuscript.Rmd"),
#                   output_dir = here::here("manuscript"))


tar_make()

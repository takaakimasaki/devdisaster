# devdisaster

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

devdisaster is a package that produces a variety of natural disaster and conflict/fragility measures.
## Installation:

    # Install development version from GitHub
    remotes::install_github("takaakimasaki/devdisaster")

## Usage:
There are currently three different functions in the package: `load_vhi()`,  `calc_fathom_flood_risk()`, `calc_fathom_flood_risk_loaded()`, and `calc_fathom_flood_risk()`. 

`load_vhi()` is a function that loads the [Vegetation Health Index](https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_TechniqueBackground.php). The VH are indices, which range from 0 to 100 characterizing changes in vegetation conditions from extremely poor (0) to excellent (100). Fair conditions are coded by green color (50), which changes to brown and red when conditions deteriorate and to blue when they improve. The VH values below 40 are used for identifying vegetation stress which is an indirect drought indicator. The VH is very useful for early drought detection, assessing drought area coverage, duration, and intensity, and for monitoring drought impacts on vegetation and agricultural crops. Use `?devdisaster::load_vhi` to see more details on this function. 

`calc_drought_vhi_risk()` and `calc_drought_vhi_risk_loaded()` are both a function that calculates % of drought areas (in total areas) or total sum of population using the VHI though the former loads VHI data and does the computation at the same time and the latter uses the loaded VHI from `load_vhi()` and does the computation. If you load a large amount of VHI data, we recommend first loading VHI data using `load_vhi()` and then generate drought indices using `calc_drought_vhi_risk_loaded()`. Use `?devdisaster::calc_drought_vhi_risk` and `?devdisaster::calc_drought_vhi_risk_loaded` to see more details on this function.

`calc_fathom_flood_risk()` is a funciton that helps calculate the number and percentage of people exposed to flood risks based on high resolution population data combined with flood hazard data from https://www.fathom.global/. Use `?devdisaster::calc_fathom_flood_risk` to see more details on this function.  



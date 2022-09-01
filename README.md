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
There are currently three different functions in the package: `calc_flood_risk()`, `calc_flood_risk2()` and `calc_vhi_files()`. 

`calc_flood_risk()` is a funciton that helps calculate the number and percentage of people exposed to flood risks based on high resolution population data combined with flood hazard data from https://www.fathom.global/. Use `?devdisaster::calc_flood_risk` to see more details on this function.  

`calc_flood_risk2()` is a variant of `calc_flood_risk()` and accommodates the second layer of flood data from https://www.fathom.global/. Fathom flood data distinguishes fluvial vis-a-vis pluvial flood and at times, users may want to combine these two different layers of flood data to generate a meausre of flood risk that accounts for both types of flood. This function is really meant for this particular purpose. Use `?devdisaster::calc_flood_risk2` to see more details on this function. 

`calc_drought_vhi_risk()` is a function that calculates % of drought areas (in total areas) or total sum of population using the Vegetation Health Index (https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_browse.php). `?devdisaster::calc_drought_vhi_risk` to see more details on this function.

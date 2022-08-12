# devaccess

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

devaccess is a package that produces a variety of natural disaster and conflict/fragility measures.
## Installation:

    # Install development version from GitHub
    remotes::install_github("takaakimasaki/devaccess")

## Usage:
There are currently two different functions in the package: `calc_flood_risk()` and `download_vhi_files()`. 
`calc_flood_risk()` calculates the number and percentage of people exposed to flood risks based on high resolution population data combined with flood hazard data from https://www.fathom.global/.
`download_vhi_files` downloads all the files of the Vegetation Health Index from https://www.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/. 

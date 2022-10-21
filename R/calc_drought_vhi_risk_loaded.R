#' Calculate % of drought areas (in total areas) or total sum of population using the Vegetation Health Index (https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_TechniqueBackground.php).The VH are indices, which range from 0 to 100 characterizing changes in vegetation conditions from extremely poor (0) to excellent (100). Fair conditions are coded by green color (50), which changes to brown and red when conditions deteriorate and to blue when they improve. The VH values below 40 are used for identifying vegetation stress which is an indirect drought indicator. The VH is very useful for early drought detection, assessing drought area coverage, duration, and intensity, and for monitoring drought impacts on vegetation and agricultural crops.
#' @param sf object of class sf containing administrative boundaries.
#' @param r object of class SpatRaster; scontains the VHI data.
#' @param threshold object of class numeric; specify a threshold used to define below what value of VHI is considered to be in drought. For more details on how the VHI is used to define drought conditions, see https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_TechniqueBackground.php.
#' @param pop object of class Raster or SpatRaster; population raster to be used so that total number of drought affected people can be computed.
#' @param ag object of class Raster or SpatRaster; cropland classification coded 1 if a given pixel is cropland, NA otherwise.
#' @param pop_wt TRUE if the size of population affected by droughts - instead of % of land - should be returned.
#' @param ag_wt TRUE if analysis should be done only on agricultural land, as defined by ag.
#' @return `calc_drought_vhi_risk_loaded()` returns a data.frame object with a variable called vhi_yyyy_mm where yyyy corresponds to year and mm month. vhi_yyyy_mm reports the percent of area in drought when ag_wt=FALSE and pop_wt=FALSE; the percent of cropland in drought when ag_wt=TRUE and pop_wt=FALSE; the sum of pop in areas of drought when ag_wt=FALSE and pop_wt=TRUE; and lastly the sum of pop in cropland in drought when ag_wt=TRUE and pop_wt=TRUE.

#' @export
#' @examples
#'\dontrun{
#' calc_drought_vhi_risk(sf,start_year=2001,end_year=2001,threshold=40)
#'}
#' @import sf dplyr
#' @importFrom terra rast crop resample
#' @importFrom stringr str_replace
#' @importFrom exactextractr exact_extract
#' @importFrom httr GET

# download vhi files --------------------------------------
calc_drought_vhi_risk_loaded <-function(sf, r, threshold, ag, pop, ag_wt = FALSE, pop_wt = FALSE){
  # start_year: num. starting year for the range of years to get data for
  # end_year: num. ending year for the range of years to get data for
  # output_folder: character. folder for output
  # set timeout for downloading

  #period = 1
  #threshold = 40
  #pop_wt = TRUE
  #ag_wt = TRUE
  #start_year = 2022
  #end_year = 2022
  #ag = devdisaster::cropland
  #pop = devdisaster::crop_val
  #sf = devdisaster::sf
  options(timeout=1000)
  if(st_crs(sf)[[1]]!="WGS 84") sf <- sf %>% sf::st_transform(4326)
  r <- r %>%
    terra::crop(., sf) %>%
    terra::app(function(x) ifelse(x==-9999 | x > threshold, 0, 1))

  if(ag_wt==TRUE) {
    if(class(ag)=="RasterLayer") ag <- rast(ag)
    r <- terra::resample(r, ag, method="near")
    r <- r*ag ##only consider those areas considered to be agricultural.
  }

  if(pop_wt==TRUE) {
    if(class(pop)=="RasterLayer") pop <- rast(pop)
    r <- terra::resample(r,pop, method="near")
    r <- r*pop ##compute total sum of pop.
    sf <- sf %>%
      exactextractr::exact_extract(r, ., fun="sum") %>%
      cbind(sf,.) %>%
      sf::st_drop_geometry() %>%
      as.data.frame() %>%
      rename_at(vars(contains("sum.")),
                list( ~ stringr::str_replace(., "sum.", "")))
  }

  if(pop_wt==FALSE) {
    sf <- sf %>%
      exactextractr::exact_extract(r, ., fun="mean") %>%
      cbind(sf,.) %>%
      sf::st_drop_geometry() %>%
      as.data.frame() %>%
      rename_at(vars(contains("mean.")),
                list( ~ stringr::str_replace(., "mean.", "")))
  }
  return(sf)
}

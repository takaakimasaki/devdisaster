#' Calculate % of drought areas (in total areas) or total sum of population using the Vegetation Health Index (https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_browse.php).The VH are indices, which range from 0 to 100 characterizing changes in vegetation conditions from extremely poor (0) to excellent (100). Fair conditions are coded by green color (50), which changes to brown and red when conditions deteriorate and to blue when they improve. The VH values below 40 are used for identifying vegetation stress which is an indirect drought indicator. The VH is very useful for early drought detection, assessing drought area coverage, duration, and intensity, and for monitoring drought impacts on vegetation and agricultural crops.
#' @param sf object of class sf containing administrative boundaries.
#' @param start_year object of class integer; specify the start year for which the Vegetation Health Index is loaded.
#' @param end_year object of class integer; specify the end year for which the Vegetation Health Index is loaded.
#' @param threshold object of class numeric; specify a threshold used to define below what value of VHI is considered to be in drought. For more details on how the VHI is used to define drought conditions, 
#' @param pop object of class raster; population raster to be used so that total number of drought affected people can be computed.
#' @param ag object of class raster; cropland classification coded 1 if a given pixel is cropland, NA otherwise.
#' @param pop_wt TRUE if the size of population affected by droughts - instead of % of land - should be returned.
#' @param ag_wt TRUE if analysis should be done only on agricultural land, as defined by ag.
#' @return `calc_drought_vhi_risk()` returns a data.frame object with...

#' @export
#' @examples
#'\dontrun{
#' download_vhi_files(2000,2021,output_folder)
#'}
#' @import sf dplyr
#' @importFrom raster raster crop mask
#' @importFrom exactextractr exact_extract
#' @importFrom httr GET

# download vhi files --------------------------------------
calc_drought_vhi_risk <-function(sf, start_year, end_year, threshold, ag, pop, ag_wt = FALSE, pop_wt = FALSE){
  # start_year: num. starting year for the range of years to get data for
  # end_year: num. ending year for the range of years to get data for
  # output_folder: character. folder for output
  # set timeout for downloading
  options(timeout=1000)

  # make a list of years
  year_list <- seq(start_year, end_year)

  # make a base url
  base_url <- "https://www.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/"
  n <- 0
  for (year in year_list){
   # loop through 1-52 (weeks in the year)
    for (period in seq(1,52)){
      n <- n + 1
      # set Satellite ID
      satellite_id <- dplyr::case_when(
        #(year>=1981) & (year<1985) ~ "NC",
        #(year>=1985) & (year<1989) ~ "NF",
        #(year>=1989) & (year<1995) ~ "NH",
        #(year>=1995) & (year<2001) ~ "NJ",
        (year>=2001) & (year<2004) ~ "NL",
        (year==2004) ~ "VH",
        (year==2005) & (period<24) ~ "NL",
        (year==2005) & (period>=24) ~ "NN",
        (year>=2006) & (year<2011) ~ "NN",
        (year>=2011) & (year<2013) ~ "NP",
        (year>=2012) & (year<2023) ~ "npp",
      )
      # convert period to 3 digits
      period_3_digit <- sprintf("%03d",period)
      # make url
      file_name <- paste0("VHP.G04.C07.",satellite_id,".P",year,period_3_digit,".VH.VHI.tif")
      url <- paste0(base_url, file_name)
      # download files
      if (!file.exists(url)){
        # use try catch to deal with errors
        tryCatch(
          expr = {
            #download.file(url, full_path)
            geotiff_file <- tempfile(fileext='.tif')
            httr::GET(url,httr::write_disk(path=geotiff_file))
            if(n==1) {
              r <- raster(geotiff_file)%>%
              crop(., admin) %>%
              raster::calc(function(x) ifelse(x==-9999 | x > threshold, 0, 1))
              if(ag_wt==TRUE) {
                r <- raster::resample(r, ag, method="ngb")
                r <- r*ag ##only consider those areas considered to be agricultural.
                names(r) <- paste0("vhi_",year,"_",period)
              }
              if(pop_wt==TRUE) {
                r <- raster::resample(r, pop, method="ngb")
                r <- r*pop
              }
              names(r) <- paste0("vhi_",year,"_",period)
            }
            if(n>1) {
              r2 <- raster(geotiff_file) %>%
                crop(., admin) %>%
                raster::calc(function(x) ifelse(x==-9999 | x > threshold, 0, 1))
              if(ag_wt==TRUE) {
                r2 <- raster::resample(r2, ag, method="ngb")
                r2 <- r2*ag ##only consider those areas considered to be agricultural.
              }
              if(pop_wt==TRUE) {
                r2 <- raster::resample(r2, pop, method="ngb")
                r2 <- r2*pop
              }
              names(r2) <- paste0("vhi_",year,"_",period)
              r <- raster::stack(r, r2)
            }
          },
          error = function(e){
            message(paste0("Got an for ", url))
            print(e)
          }
        )
      }
    }
  }
  if(pop_wt==FALSE) {
    admin <- admin %>%
      exactextractr::exact_extract(r, ., fun="mean") %>%
      cbind(admin,.) %>%
      sf::st_drop_geometry() %>%
      as.data.frame()
  }
  if(pop_wt==TRUE) {
      admin <- admin %>%
        exactextractr::exact_extract(r, ., fun=c("sum")) %>%
        cbind(admin,.) %>%
        sf::st_drop_geometry() %>%
        as.data.frame()
    }
  return(admin)
}

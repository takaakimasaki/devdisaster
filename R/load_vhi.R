#' Load the Vegetation Health Index (https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_TechniqueBackground.php).The VH are indices, which range from 0 to 100 characterizing changes in vegetation conditions from extremely poor (0) to excellent (100). Fair conditions are coded by green color (50), which changes to brown and red when conditions deteriorate and to blue when they improve. The VH values below 40 are used for identifying vegetation stress which is an indirect drought indicator. The VH is very useful for early drought detection, assessing drought area coverage, duration, and intensity, and for monitoring drought impacts on vegetation and agricultural crops.
#' @param sf object of class sf containing administrative boundaries.
#' @param start_year object of class integer; specify the start year for which the Vegetation Health Index is loaded.
#' @param end_year object of class integer; specify the end year for which the Vegetation Health Index is loaded.
#' @return `load_vhi()` returns a spatRaster object of VHI data from start_year to end_year.

#' @export
#' @examples
#'\dontrun{
#' load_vhi(sf,start_year=2001,end_year=2001)
#'}
#' @import sf dplyr
#' @importFrom terra rast crop resample
#' @importFrom stringr str_replace
#' @importFrom exactextractr exact_extract
#' @importFrom httr GET

# download vhi files --------------------------------------
load_vhi <-function(sf, start_year, end_year){
  # start_year: num. starting year for the range of years to get data for
  # end_year: num. ending year for the range of years to get data for
  # output_folder: character. folder for output
  # set timeout for downloading
  #start_year = 2022
  #end_year = 2022
  #sf = devdisaster::sf
  options(timeout=1000)
  if(st_crs(sf)[[1]]!="WGS 84") sf <- sf %>% sf::st_transform(4326)

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
              r <- terra::rast(geotiff_file) %>%
                terra::crop(., sf)
              names(r) <- paste0("vhi_",year,"_",period)
            }
            if(n>1) {
              r2 <- terra::rast(geotiff_file) %>%
                terra::crop(., sf)
              names(r2) <- paste0("vhi_",year,"_",period)
              r <- c(r, r2)
            }
          },
          error = function(e){
            message(paste0("Got an error for ", url))
            print(e)
          }
        )
        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
      }
    }
  }
  return(r)
}

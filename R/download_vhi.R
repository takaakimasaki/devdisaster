#' Download the Vegetation Health Index (https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_TechniqueBackground.php).The VH are indices, which range from 0 to 100 characterizing changes in vegetation conditions from extremely poor (0) to excellent (100). Fair conditions are coded by green color (50), which changes to brown and red when conditions deteriorate and to blue when they improve. The VH values below 40 are used for identifying vegetation stress which is an indirect drought indicator. The VH is very useful for early drought detection, assessing drought area coverage, duration, and intensity, and for monitoring drought impacts on vegetation and agricultural crops.
#' @param start_year object of class integer; specify the start year for which the Vegetation Health Index is loaded.
#' @param end_year object of class integer; specify the end year for which the Vegetation Health Index is loaded.
#' @param output_folder folder in which GEOTIFF files are downloaded
#' @return `download_vhi()` saves VHI data in GEOTIFF in a folder that you specify.

#' @export
#' @examples
#'\dontrun{
#' download_vhi(start_year=1981, end_year=1982,output_folder="")
#'}
#' @import sf dplyr
#' @importFrom terra rast crop resample
#' @importFrom stringr str_replace
#' @importFrom exactextractr exact_extract
#' @importFrom httr GET

# download vhi files --------------------------------------
download_vhi <-function(start_year, end_year, output_folder){
  # start_year: num. starting year for the range of years to get data for
  # end_year: num. ending year for the range of years to get data for
  # output_folder: character. folder for output
  # start_year = 2015
  # end_year = 2015
  #output_folder <- paste0(dir_input,"/VHI")
  # set timeout for downloading
  # options(timeout=5000)

  # make a list of years
  year_list <- seq(start_year, end_year)

  # make a base url
  base_url <- "https://www.star.nesdis.noaa.gov/data/pub0018/VHPdata4users/VHP_4km_GeoTiff/"

  for (year in year_list){

    file_name <- paste0("VHP.G04.C07_", year,".tar.gz")
    url <- paste0(base_url, file_name)
    full_path <- paste0(output_folder,"/",file_name)
      # download files
      if (!file.exists(full_path)){
        # use try catch to deal with errors
        tryCatch(
          expr = {
            download.file(url, full_path)
          },
          error = function(e){
            message(paste0("Got an for ", url))
            print(e)
          }
        )
      }
    }
  }

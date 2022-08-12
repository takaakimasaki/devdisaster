#' Download the Vegetation Health Index (https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_browse.php).
#' @param start_year specify the start year for which the Vegetation Health Index is downloaded.
#' @param end_year specify the end year for which the Vegetation Health Index is downloaded.
#' @param output_folder specifiy the folder where the downloaded data will be saved.
#' @export
#' @examples
#'\dontrun{
#' download_vhi_files(2000,2021,output_folder)
#'}
#' @import dplyr tidyverse

# download vhi files --------------------------------------
download_vhi_files<-function(start_year, end_year, output_folder){
  # start_year: num. starting year for the range of years to get data for
  # end_year: num. ending year for the range of years to get data for
  # output_folder: character. folder for output
  # set timeout for downloading
  options(timeout=1000)

  # make a list of years
  year_list <- seq(start_year, end_year)

  # make a base url
  base_url <- "https://www.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/"

  for (year in year_list){
    # loop through 1-52 (weeks in the year)
    for (period in seq(1,52)){
      # set Satellite ID
      satellite_id <- case_when(
        (year>=1981) & (year<1985) ~ "NC",
        (year>=1985) & (year<1989) ~ "NF",
        (year>=1989) & (year<1995) ~ "NH",
        (year>=1995) & (year<2001) ~ "NJ",
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
}





# split geometries into n pieces of roughly equal area size ---------------
split_n<-function(n,polygon,output_folder){
  # n: number of pieces that you want to split the polygon into
  # polygon: sf object of polygon
  # output_folder: string of the output folder
  pacman::p_load(dplyr)
  
  # calculate the number of rows to get each time
  n_rows<-nrow(polygon)
  quotient<-n_rows%/%n
  mod<-n_rows%%n
  first_vec_len<-n-mod
  first_vec<-rep(quotient,first_vec_len)
  second_vec<-rep(quotient+1,mod)
  complete_vec<-c(first_vec,second_vec)
  
  # set id for polygon
  polygon<-polygon %>% 
    mutate(id = row_number())
  
  for (i in seq_along(complete_vec)){
    # if n is an even number
    if (complete_vec[i]%%2==0){
      top_n<-complete_vec[i]/2
      bottom_n<-complete_vec[i]/2
    }
    else{
      top_n<-complete_vec[i]%/%2+1
      bottom_n<-complete_vec[i]%/%2
    }
    polygon_top_selected<-polygon %>%
      slice_max(n = top_n, order_by = area)
    if (bottom_n!=0){
      polygon_bottom_selected<-polygon %>%
        slice_min(n = bottom_n, order_by = area)
    }
    else{
      polygon_bottom_selected <- NULL
    }
    polygon_selected<-rbind(polygon_top_selected,polygon_bottom_selected)
    polygon<-polygon %>%
      filter(!id %in% polygon_selected$id)
    # save selected polygon to zipped shp file
    naomi.utils::write_sf_shp_zip(polygon_selected,
                     paste0(output_folder,"split_",i,".zip"),
                     overwrite = TRUE)
  }
}



# authenticate for the appears API ----------------------------------------
authenticate_api <- function(username,password){
  # input
  # username: string. user name on the appears
  # password: string. password on the appears
  
  # output
  # token_response: json. token necessary to authenticate api
  pacman::p_load(httr,jsonlite)
  
  secret <- base64_enc(paste(username, password, sep = ":"))
  response <- POST("https://appeears.earthdatacloud.nasa.gov/api/login", 
                   add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                               "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                   body = "grant_type=client_credentials")
  token_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
  token <- paste("Bearer", fromJSON(token_response)$token)
  return(token)
}


# download files ----------------------------------------------------------
download_files<-function(txt_file, token, output_folder){
  # input
  # txt_file: string of file path to the text file containing the url of the files
  # token: json. token returned from authenticate_api function
  # output_folder: string of the output folder
  #                [output_folder]-SVI-Data_EVI
  #                                  |-Data_Pixel_Reliability
  
  # output
  # None
  pacman::p_load(tidyverse,magrittr,httr)
  
  # set timeout for downloading
  options(timeout=1000)
  
  # check the # of the split from the text file
  # e.g.,: admin1_1
  num_split<-txt_file %>% 
    str_split(.,"/") %>% 
    extract2(1) %>% 
    extract(length(.)) %>% 
    str_split(.,"-") %>% 
    extract2(1) %>% 
    extract(1)

  # read the text file
  url_vec<-scan(txt_file,character())%>%
    walk(~{
      # set a destination file name and download the file
      # template file name is
      # [path to the folder]/[Data_EVI or Data_Pixel_Reliability]/DOY/DOY_[day of the year]/DOY_YYYY_[original name]_admin1_[# of the split].tif
      url<-.x
      # print(url)
      # check the data type
      data_type<-url %>%
        str_split(.,"16_days_") %>%
        extract2(1) %>%
        extract(length(.))
      # set the data type variable and get DOY and YYYY
      if (startsWith(data_type, "pixel_reliability")){
        data_type_folder<-"Pixel_Reliability"
        doy<-url %>%
          str_split(.,paste0("pixel_reliability_doy")) %>%
          extract2(1) %>%
          extract(length(.)) %>%
          substr(.,5,7) 
        yyyy<-url %>%
          str_split(.,paste0("pixel_reliability_doy")) %>%
          extract2(1) %>%
          extract(length(.)) %>%
          substr(.,1,4)
      }
      else if(startsWith(data_type, "EVI")){
        data_type_folder<-"EVI"
        doy<-url %>%
          str_split(.,paste0("EVI_doy")) %>%
          extract2(1) %>%
          extract(length(.)) %>%
          substr(.,5,7) 
        yyyy<-url %>%
          str_split(.,paste0("EVI_doy")) %>%
          extract2(1) %>%
          extract(length(.)) %>%
          substr(.,1,4)
      }
      else{
        data_type_folder<-""
      }
      # download the file if data_type_folder variable is an instance
      if (!data_type_folder==""){
        # set the destination file path
        folder_path<-paste0(output_folder,"Data_",data_type_folder,"/DOY_",doy,"/")
        # extract task_id and file_id
        task_id <- url %>% 
          str_split(.,"/") %>%
          extract2(1) %>%
          extract(6) 
        file_id <-  url %>% 
          str_split(.,"/") %>%
          extract2(1) %>%
          extract(7) 
        # set file name
        file_name<-url %>%
          str_split(.,"/") %>%
          extract2(1) %>%
          extract(length(.)) %>%
          str_split(.,".tif") %>%
          extract2(1) %>%
          extract(1) %>%
          paste0(.,num_split,".tif")
        full_path<-paste0(folder_path,file_name)
        
        # download the file if it doesnt exist yet
        if (!file.exists(full_path)){
          print(full_path)
          response <- GET(paste0("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, '/', file_id),
                          write_disk(full_path, overwrite = FALSE), progress(), add_headers(Authorization = token))
          # download.file(url,full_path)
        }
      }
      else{
        # do nothing
      }
    })
}

rename_files<-function(folder_path){
  # folder_path: path to the parent folder whose files in the subfolders to be renamed
  
  pacman::p_load(DescTools)
  
  # get the list of files
  file_list<-list.files(path = folder_path, full.names = TRUE, recursive = TRUE) %>% 
    walk(~{
      # get the directory
      dir_path<-SplitPath(.x) %>% 
        extract("dirname")
      file_name<-SplitPath(.x) %>% 
        extract("fullfilename") %>% 
        as.character()
      doy<-file_name %>% 
        str_split(.,"doy") %>% 
        extract2(1) %>%
        extract(length(.)) %>%
        substr(.,5,7)
      yyyy<-file_name %>% 
        str_split(.,"doy") %>% 
        extract2(1) %>%
        extract(length(.)) %>%
        substr(.,1,4)
      new_file_name<-paste0(dir_path,doy,"_",yyyy,"_",file_name)
      
      # rename to the new file name if the new name doesn't still exist
      if (!startsWith(file_name,paste0(doy,"_",yyyy,"_"))){
        file.rename(.x,new_file_name)
      }
      else{
        #do nothing
      }
    })
}


# merge all the layers with the same DOY and YYYY -------------------------
merge_layers<-function(folder_path){
  # folder_path: path to the folder to conducting merging of layers
  # get the list of files
  file_list<-list.files(paste0(folder_path,"/"),pattern=".tif")
  # get the list of unique first 8 digits of the file (DOY_YYYY)
  unique_8_digits<-file_list %>%
    substr(.,1,8) %>%
    unique() %>%
    walk(~{
      print(.x)
      # loop through the list of unique strings to get respective raster and merge them
      file_8_digits_list<-file_list %>%
        magrittr::extract(startsWith(.,.x)) %>%
        paste0(folder_path,"/",.)

      # get the base file name
      base_name<-file_8_digits_list %>%
        magrittr::extract(1) %>%
        str_split(.,"_doy") %>%
        magrittr::extract2(1) %>%
        magrittr::extract(1)
      
      # save the merged raster to by naming it "[original file name up to '_doy']_merged.tif"
      if (!file.exists(paste0(base_name,"_merged.tif"))){
        file_8_digits_list %>%
          lapply(., raster) %>%
          Reduce(merge,.) %>%
          writeRaster(.,filename=paste0(base_name,"_merged.tif"), format="GTiff", overwrite=FALSE)
      }
    }) 
}

compute_svi<-function(# border_shp_file,
                      evi_folder_path,
                      pr_folder_path,
                      path_jpg,
                      path_tif){
  # border_shp_file: file path to border shapefile
  # evi_folder_path: folder path to svi
  # pr_folder_path: folder path to pixel reliability 
  # path_jpg: folder path to jpg
  # path_tif: folder oath to tif
  
  # installing relevant packages
  pacman::p_load(raster,rgdal,sp)
  
  # load borders 
  # border <- shapefile(border_shp_file) # ToDo: insert link to the shapefile with the country borders
  # download country borders as shapefiles: http://www.gadm.org/download
  
  # ToDo: enter link to the folder where you have stored the MODIS EVI data
  dlist <- dir(evi_folder_path,pattern="DOY")
  
  # ToDo: enter link to the folder where you have stored the MODIS Pixel Reliability data
  dlist_c <- dir(pr_folder_path,pattern="DOY")
  
  # ToDo: enter the link to the folder where you want to store the resulting .jpg-images.
  # ToDo: enter the link to the folder where you want to store the resulting .tif-files.
  
  pb <- txtProgressBar (min=0, max=length(dlist), style=1) # this creates a progress bar in the Console, 
  # which ends at the end of the loop. The proegress bar looks like this: =========
  setTxtProgressBar (pb, 0)
  for (i in 1:length(dlist)) {                   # start of the outer for-loop
    # run the following code if the output files don't exist yet
    file_list<-list.files(path_tif,pattern = paste0("^",substr(dlist[i],5,7)))
    if (length(file_list)==0){
      fold <- paste(evi_folder_path,dlist[i],sep="/")         # the respective DOY folder of the Data_EVI folder
      fls <- dir(fold,pattern="merged.tif")              # all files that are available in the respective EVI DOY folder
      flsp <-paste(fold,fls,sep="/")               # all files that are available in the respective EVI DOY folder with complete path name
      evistack <- stack(flsp)                      # creates a layer stack of all files within the EVI DOY folder
      # eviresize<- crop(evistack,border)            # resizes the EVI layer stack to the rectangular extent of the border shapefile
      # evimask<-mask(eviresize,border)              # masks the EVI layer stack using the border shapefile
      evi<-evistack*0.0001                          # rescaling of MODIS EVI data
      evi[evi==-0.3]<-NA                           # EVI fill value(-0,3) in NA
      evi[evi<(0)]<-NA                             # as we are only interested in vegetation valid EVI range is 0 to 1 and all EVI values smaller than 0 set to NA
      
      fold_c <- paste(pr_folder_path,dlist_c[i],sep="/")   # the respective DOY folder of the Data_Pixel_Reliability folder
      fls_c <- dir(fold_c,pattern="merged.tif")          # all files that are available in the respective Pixel Reliability DOY folder
      flsp_c <-paste(fold_c,fls_c,sep="/")         # all files that are available in the respective Pixel Reliability DOY folder with complete path name
      
      cloudstack <- stack(flsp_c)                  # creates a layer stack of all files within the Pixel Relaibility DOY folder
      # cloudresize<- crop(cloudstack,border)        # resizes the Pixel Reliability layer stack to the rectangular extent of the border shapefile
      # cloudmask<-mask(cloudresize,border)          # masks the Pixel Reliability layer stack using the border shapefile
      cloudstack[cloudstack==(3)]<-NA                # Pixel Reliability rank 3 pixels (cloudy) set to NA
      cloudstack[cloudstack==(2)]<-NA                # Pixel Reliability rank 2 pixels (snow&ice) set to NA
      cloudstack[cloudstack==(0)]<-1                 # Pixel Reliability rank 0 pixels (good quality) set to 1
      cloudstack[cloudstack>(3)]<-NA                 # as valid Pixel Reliability range is -1 to 3, all Pixel Reliability values >3 set to NA
      # (as -1 rank pixels show value 255)
      
      evi_c=evi*cloudstack                          # multiplying the EVI layer stack by the Pixel Reliability layer stack
      # to get one single layer stack with applied cloud mask
      
      # extracting mean and standard deviation for each pixel
      evimean <- stackApply (evi_c, rep (1, nlayers (evi)), mean, na.rm=T) #calculating the mean for the layer stack for each individual pixel
      evisd <- stackApply (evi_c, rep (1, nlayers (evi)), sd, na.rm=T) #calculating the standard deviation for the layer stack for each individual pixel
      
      
      # if na.rm is TRUE NA values are ignored, otherwise an NA value in any of the arguments will cause a value of NA to be returned,
      # https://stat.ethz.ch/R-manual/R-devel/library/base/html/Extremes.html
      
      SVI_all <- ((evi_c-evimean)/evisd) #calculating SVI
      
      for (k in 1:nlayers(SVI_all)) {     # start of the inner for-loop
        year <- substr(fls[k],5,8)        # extracting the fifth to eigths letter of the filename, which is the year (cf. data preparation above)
        doy <- substr(fls[k],1,3)         # extracting the first to third letter of the filename, which is the DOY (cf. data preparation above)
        
        writeRaster(SVI_all[[k]], filename=paste(path_tif,"/",doy,"_",year,".tif",sep=""), format="GTiff", overwrite=TRUE)
        # writes the geotiff and automizes the file naming according to the pattern DOY_YYYY
        
      } # end of the inner for-loop
    }
    else{
      # do nothing
      print("files already exist")
    }
    setTxtProgressBar (pb, i)
  }   # end of the outer for-loop
}

compute_svi_by_batch<-function(
  evi_folder_path,
  pr_folder_path,
  path_jpg,
  path_tif){
  # border_shp_file: file path to border shapefile
  # evi_folder_path: folder path to svi
  # pr_folder_path: folder path to pixel reliability 
  # path_jpg: folder path to jpg
  # path_tif: folder oath to tif
  
  # installing relevant packages
  pacman::p_load(raster,rgdal,sp)
  
  # ToDo: enter link to the folder where you have stored the MODIS EVI data
  dlist <- dir(evi_folder_path,pattern="DOY")
  
  # ToDo: enter link to the folder where you have stored the MODIS Pixel Reliability data
  dlist_c <- dir(pr_folder_path,pattern="DOY")
  
  # ToDo: enter the link to the folder where you want to store the resulting .jpg-images.
  # ToDo: enter the link to the folder where you want to store the resulting .tif-files.
  
  pb <- txtProgressBar (min=0, max=length(dlist), style=1) # this creates a progress bar in the Console, 
  # which ends at the end of the loop. The proegress bar looks like this: =========
  setTxtProgressBar (pb, 0)
  for (i in 1:length(dlist)) {                   # start of the outer for-loop
    # run the following code if the output files don't exist yet
    file_list<-list.files(path_tif,pattern = paste0("^",substr(dlist[i],5,7)))
    if (length(file_list)==0){
      # the respective DOY folder of the Data_EVI folder
      fold <- paste(evi_folder_path,dlist[i],sep="/")
      # get unique batch list of aidX + admin1X combinations
      list.files(fold,pattern=".*mozadmin.*.tif") %>% 
        str_extract(., "aid[:digit:]*mozadmin[:digit:]*") %>% 
        unique() %>% 
        # loop through the list of combination
        walk(~{
          # get file pattern (e.g., aid0001mazadmin12.tif etc)
          file_pattern <- .x
          # all files that match the pattern
          fls <- dir(fold,pattern=paste0(file_pattern, ".tif")) 
          # complete path name
          flsp <-paste(fold,fls,sep="/")
          # creates a layer stack of all files that match the pattern
          evistack <- stack(flsp)
          # rescaling of MODIS EVI data
          evi<-evistack*0.0001
          # as we are only interested in vegetation valid EVI range is 0 to 1 and all EVI values smaller than 0 set to NA
          evi[evi<(0)]<-NA
          
          # the respective DOY folder of the Data_Pixel_Reliability folder
          fold_c <- paste(pr_folder_path,dlist_c[i],sep="/")
          # all files that match the pattern
          fls_c <- dir(fold_c,pattern=paste0(file_pattern, ".tif"))
          # complete path name
          flsp_c <-paste(fold_c,fls_c,sep="/")
          # creates a layer stack of all files within the Pixel Relaibility DOY folder
          cloudstack <- stack(flsp_c)
          # Pixel Reliability rank 3 pixels (cloudy) set to NA
          cloudstack[cloudstack==(3)]<-NA
          # Pixel Reliability rank 2 pixels (snow&ice) set to NA
          cloudstack[cloudstack==(2)]<-NA
          # Pixel Reliability rank 0 pixels (good quality) set to 1
          cloudstack[cloudstack==(0)]<-1
          # as valid Pixel Reliability range is -1 to 3, all Pixel Reliability values >3 set to NA
          cloudstack[cloudstack>(3)]<-NA
          # (as -1 rank pixels show value 255)
          # multiplying the EVI layer stack by the Pixel Reliability layer stack to get one single layer stack with applied cloud mask
          evi_c=evi*cloudstack
          
          # extracting mean and standard deviation for each pixel
          #calculating the mean for the layer stack for each individual pixel
          evimean <- stackApply (evi_c, rep (1, nlayers (evi)), mean, na.rm=T)
          #calculating the standard deviation for the layer stack for each individual pixel
          evisd <- stackApply (evi_c, rep (1, nlayers (evi)), sd, na.rm=T)
          
          
          # if na.rm is TRUE NA values are ignored, otherwise an NA value in any of the arguments will cause a value of NA to be returned,
          # https://stat.ethz.ch/R-manual/R-devel/library/base/html/Extremes.html
          
          #calculating SVI
          SVI_all <- ((evi_c-evimean)/evisd)
          # loop through layers of SVI_all
          for (k in 1:nlayers(SVI_all)) {
            # extracting the fifth to eigths letter of the filename, which is the year (cf. data preparation above)
            year <- substr(fls[k],5,8)
            # extracting the first to third letter of the filename, which is the DOY (cf. data preparation above)
            doy <- substr(fls[k],1,3) 
            # writes the geotiff and automizes the file naming according to the pattern DOY_YYYY
            writeRaster(SVI_all[[k]], filename=paste(path_tif,"/",doy,"_",year,file_pattern,".tif",sep=""), format="GTiff", overwrite=TRUE)
          }
          # clear memory
          gc()
        })
    }
    else{
      # do nothing
      print("files already exist")
    }
    setTxtProgressBar (pb, i)
  }   # end of the outer for-loop
  # merge output tif files
  doy_list <- list.files(path_tif,pattern = ".tif") %>% 
    substr(.,1,8) %>% 
    unique() %>% 
    walk(~{
      doy_year <- .x
      if(!(file.exists(paste0(path_tif,"/",doy_year,"merged.tif")))){
        list.files(path_tif,pattern = doy_year,full.names = TRUE) %>% 
          lapply(., raster) %>%
          Reduce(merge,.) %>%
          writeRaster(.,filename=paste0(path_tif,"/",doy_year,".tif"), format="GTiff", overwrite=FALSE)
      }
    })
  # remove batch files
  junk <- dir(path=path_tif,  pattern="mozadmin", full.names = TRUE)
  file.remove(junk)
  print("batch files removed")
}

# calculate the yearly average for each year --------------
yearly_average<-function(folder_path){
  # folder_path: path to a folder that has SVI raster layers of different DOY and YYYY
  
  # get a list of unique year
  file_list<-list.files(paste0(folder_path,"/"),pattern=".tif")
  # get the list of unique years (i.e.,YYYY)
  unique_year<-file_list %>%
    substr(.,5,8) %>%
    unique() %>% 
    walk(~{
      # get the list of raster files with the same years
      print(.x)
      file_year_list<-file_list %>%
        magrittr::extract(endsWith(.,paste0(.x,".tif"))) %>%
        paste0(folder_path,"/",.)
      
      # create a folder for annual average files if it doesn't exist yet
      new_folder_path<-folder_path %>% 
        str_split(.,"/") %>% 
        extract2(1) %>% {
          split_folder_path<-.
          num_dir<-length(split_folder_path)-1
          output<-split_folder_path %>% 
            extract(1:num_dir)
          paste(output,collapse = '/')
        } %>%
        paste0(.,"/SVI_yearly_average")
      if (!dir.exists(new_folder_path)){
        dir.create(new_folder_path)
      }
      # calculate mean if the file doesn't exist yet
      # and save the merged raster to by naming it "YYYY.tif"
      if (!file.exists(paste0(new_folder_path,"/",.x,".tif"))){
        file_year_list %>%
          lapply(., raster) %>%    # read the as a list of raster object (stack)
          stack() %>% 
          stackApply(.,rep(1, nlayers(.)), mean, na.rm=T) %>%   # calculate the mean with stackApply
          writeRaster(.,filename=paste0(new_folder_path,"/",.x,".tif"), format="GTiff", overwrite=FALSE)
      }
      else{
        # do nothing
        print("files already exist")
      }
    })
}


# download vhi files --------------------------------------
download_vhi_files<-function(start_year, end_year, output_folder){
  # start_year: num. starting year for the range of years to get data for
  # end_year: num. ending year for the range of years to get data for
  # output_folder: character. folder for output
  start_year = 2015
  end_year = 2015
  output_folder <- paste0(dir_input,"/VHI")
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
        (year>=2001) & (year<2005) ~ "NL",
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




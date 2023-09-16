#' Descarga los datos
#'
#' @param eea_data_links_variables_df_manual El manual con los capitulos rellenados previamente
#'
#' @return Devuelve un raw data frame con todas las variables, aun no se elige las variables
#' @export
#'
#' @examples
#' download_rawdata(eea_data_links_variables_df_manual)
download_rawdata=function(eea_data_links_variables_df_manual){#0. main directory and fixing your memory to download all data

memory.limit (20000)

library(tidyverse)
library(lubridate)
library(rvest)
library(haven)
library(readr)

#1. charge all_links

temp_raw_links<-readxl::read_xlsx(eea_data_links_variables_df_manual)

#2. download enaho data and setting download directory

eea_raw_data<-list()

setwd('archivos/')

if(length(list.files())>0){

  unlink(list.files(),recursive = TRUE)

}

for (l in 1:nrow(temp_raw_links)) {

  #l=sample(1:nrow(temp_raw_links),1)
  #l=318

  message('downloading: ',temp_raw_links$sector[l],
          ' - ',temp_raw_links$year[l]
          )

  #2.1 download in .../archivos/

  file_name <- str_c('../archivos/',temp_raw_links$sector[l],'.zip')


  download.file(url = temp_raw_links$enlaces_datos[l],destfile = file_name)

  # Sys.sleep(runif(1,3,5))

  #2.2 .sav inspect in every zip

  files_data <- untar(file_name, list=TRUE)

  list_p<-files_data[which(files_data%>%
          str_detect('.csv')
          )]

  #2.3 paths located in every zip

  paths_located<-str_locate_all(list_p,'/')

  list_paths<-tibble(primero=str_sub(list_p[1],1,paths_located[[1]][1]-1),
                     segundo=str_sub(list_p[1],1,paths_located[[1]][2]-1)
  )

  #2.4 unzip

  unzip(file_name)

  # Sys.sleep(runif(1,3,5))

  #3. cases:

  #3.1 if unzip fell .sav file into current path

  data<-list()

  only_firms=list_p[!str_detect(list_p,'_2.csv')]

  if(l!=318){
  for (i in 1:length(only_firms)) {

    if(i==1){
      #i=1
      if(l!=290){
        data[[i]]<-read.csv2(only_firms[i],sep = ';')%>%
          mutate(
            sector=temp_raw_links$sector[l],
            year=temp_raw_links$year[l]#,
            #CodCapitulo=str_extract(CodCapitulo,'[0-9][0-9]|[0-9]')
          )
      }else{
        data[[i]]<-tibble(sector=temp_raw_links$sector[l],
                          year=temp_raw_links$year[l])
      }
      
    }else{
      
      if(l!=205){
        
        data[[i]]<-read.csv2(only_firms[i],sep = ';')%>%
          mutate(
            sector=temp_raw_links$sector[l],
            year=temp_raw_links$year[l]
          )
      }else{
        
        #i=22
        
        if(!(i%in% c(18,19))){
          
          data[[i]]<-read.csv2(only_firms[i],sep = ';')%>%
            mutate(
              sector=temp_raw_links$sector[l],
              year=temp_raw_links$year[l]
            )
        }else{
          data[[i]]<-tibble(sector=temp_raw_links$sector[l],
                            year=temp_raw_links$year[l])
        }
        
      }
      
    }

  }
  }else{

   d2_manufactura= '../2013_manufactura_f2/393-Modulo355/a2012_s11_fD2'

   files_data <- list.files(d2_manufactura)

   list_p<-files_data[which(files_data%>%
                              str_detect('.csv')
   )]

   only_firms_manufactura=list_p[!str_detect(list_p,'_2.csv')]

    for (i in 1:length(only_firms)) {

      data[[i]]<-read.csv2(str_c(d2_manufactura,'/',only_firms_manufactura[i]),sep = ';')%>%
        mutate(
          sector=temp_raw_links$sector[l],
          year=temp_raw_links$year[l]
        )

    }

    message('Manufactura D2 2013-hay info extra en inicio')
    message('Info que se requiere de todos los sectores-inicio')

  }


  eea_raw_data[[l]]=data

  unlink(list.files(),recursive = TRUE)

}


setwd('../')

message('saving data')
return(eea_raw_data)
#write_rds(eea_raw_data,'eea_raw_data.rds')
}

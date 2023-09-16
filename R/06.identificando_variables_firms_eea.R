#' Opcional, solo cuando sale el 2021 en adelante, para completar manualmente el excel de capitulos de las variables
#'
#' @param temp_all_data los enlaces unificados
#'
#' @return nada, solo te provee un chrome que se actualiza cada cierto intervalo para que rellenes el manual
#' @export
#'
#' @examples
#' id_variables(temp_all_data)
id_variables=function(temp_all_data){#0. main directory and fixing your memory to download all data

memory.limit (20000)

# 0. CONFIGURANDO EL SERVIDOR PARA SELENIUM
if (!"RSelenium" %in% installed.packages()[,"Package"]){install.packages("RSelenium")}
if (!"tidyverse" %in% installed.packages()[,"Package"]){install.packages("tidyverse")}
if (!"lubridate" %in% installed.packages()[,"Package"]){install.packages("lubridate")}
if (!"rvest" %in% installed.packages()[,"Package"]){install.packages("rvest")}
library(RSelenium)
library(tidyverse)
library(lubridate)
library(rvest)

fdriver <- RSelenium::rsDriver(browser = c('chrome'),
                               port = 4444L,
                               chromever = 'latest', version="latest",
                               verbose = FALSE)
driver<-fdriver[['client']]
driver$maxWindowSize()

#1. charge all_links

temp_raw_links<-temp_all_data%>%
  mutate(cap_activos='',
         pre_activos='',
         cla_activos='',
         cap_patrimo='',
         pre_patrimo='',
         cla_patrimo='',
         cap_ventas='',
         pre_ventas='',
         cla_ventas='',
         cap_insumo='',
         pre_insumo='',
         cla_insumo='',
         cap_taunet='',
         pre_taunet='',
         cla_taunet='',
         cap_acinta='',
         pre_acinta='',
         cla_acinta='',
         cap_public='',
         pre_public='',
         cla_public='',
         cap_result='',
         pre_result='',
         cla_result='',
         cap_ubruta='',
         pre_ubruta='',
         cla_ubruta='',
         cap_uopera='',
         pre_uopera='',
         cla_uopera='',
         cap_acfijo='',
         pre_acfijo='',
         cla_acfijo='',
         cap_empleo='',
         pre_empleo='',
         cla_empleo='',
         cap_salari='',
         pre_salari='',
         cla_salari='')%>%
  arrange(year,sector)

writexl::write_xlsx(temp_raw_links,'eea_data_links_variables_df.xlsx')

#2. download enaho data and setting download directory

setwd('archivos/')

if(length(list.files())>0){

  unlink(list.files(),recursive = TRUE)

}


for (l in 464:nrow(temp_raw_links)) {

  #l=sample(1:nrow(temp_raw_links),1)
  #l=298
  message('Buscando variables en: ',temp_raw_links$sector[l],' - ',temp_raw_links$year[l])

  message('downloading: ',temp_raw_links$sector[l],
          ' - ',temp_raw_links$year[l]
  )

  if(temp_raw_links$sector[l]!='TODOS'){

  #2.1 download in .../archivos/

  file_name <- here::here(str_c(temp_raw_links$sector[l],'.zip'))



  download.file(url = URLencode(temp_raw_links$enlaces_docs[l]),destfile = file_name)

  # Sys.sleep(runif(1,3,5))

  #2.2 .sav inspect in every zip

  files_data <- untar(file_name, list=TRUE)

  list_p<-files_data[which(files_data%>%
                             str_detect('.pdf|.PDF')
  )]

  #2.3 paths located in every zip

  paths_located<-str_locate_all(list_p,'/')

  list_paths<-tibble(primero=str_sub(list_p[1],1,paths_located[[1]][1]-1),
                     segundo=str_sub(list_p[1],1,paths_located[[1]][2]-1)
  )

  #2.4 unzip

  unzip(file_name)

  aux=list.files()

  pdf_name<-aux[which(aux%>%str_detect('.pdf|.PDF')
  )]

  # Sys.sleep(runif(1,3,5))

  directorio=getwd()

  main_url<-str_c('file:///',directorio,'/',pdf_name)

  driver$navigate(main_url)

  Sys.sleep(15)

  unlink(list.files(),recursive = TRUE)
  }

}


setwd('../')


driver$close()
}

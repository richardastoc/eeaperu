#' Selecciona solo firmas
#'
#' @param establecimientos_xlsx Archivo de establecimientos para eliminar
#' @param start_date periodo de inicio
#' @param end_date periodo final
#' @param data_links link de datos
#' @param doc_links link de documentacion
#'
#' @return dataframe de documentacion solo para establecimientos
#' @export
#'
#' @examples
#' select_only_firm(establecimientos_xlsx,start_date,end_date,data_links,doc_links)
select_only_firm=function(establecimientos_xlsx,
                          start_date,
                          end_date,
                          data_links,
                          doc_links){#0. main directory and fixing your memory to download all data

memory.limit (20000)

library(tidyverse)
library(lubridate)
library(rvest)
library(haven)
library(readr)

#1. charge all_links

establecimientos_name=readxl::read_xlsx(establecimientos_xlsx)%>%
  mutate(year=as.character(year))



temp_doc_links<-doc_links%>%
  left_join(establecimientos_name,by=c('year','nombre'))%>%
  mutate(lala=case_when(
    is.na(caract_docs)~1,
    TRUE~0
  ))%>%
  filter(lala==1)%>%
  select(-caract_docs,-caract_datos,-lala)

temp_dat_links<-data_links

for (i in 2001:2020) {
  #i=2001
  aux=dim(temp_dat_links%>%filter(year==i))[1]-1==dim(temp_doc_links%>%filter(year==i))[1]
  if(aux==TRUE){
    message(i,' - ','cumple')
  }else{
    message(i,' - ','problemas')

    #2010-los M de datos est?n en una sola en docs
    #2011-falta restaurantes R1 en docs
    #2012-falta Acuicultura A en docs
    #2013-falta Transportes y Comunicaciones M en docs
  }
}

message('saving data')

only_firms_doc_link=temp_doc_links

return(only_firms_doc_link)

}

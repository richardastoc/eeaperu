#' Selecciona los datos de todos, capitulo 1
#'
#' @param eea_data_links_variables_df_manual excel de capitulos
#' @param eea_raw_data datos brutos
#' @param tildes vocales en tildes
#'
#' @return dataframe con generalidades de cada firma
#' @export
#'
#' @examples
#' select_cap1(eea_data_links_variables_df_manual,eea_raw_data)
select_cap1=function(eea_data_links_variables_df_manual,
                          eea_raw_data,
                     tildes){#0. main directory and fixing your memory to download all data

memory.limit (20000)

library(tidyverse)
library(lubridate)
library(rvest)
library(haven)
library(readr)

#1. charge all_links

temp_raw_links<-readxl::read_xlsx(eea_data_links_variables_df_manual,
                                  sheet = 'data_df')
temp_raw_caps<-readxl::read_xlsx(eea_data_links_variables_df_manual,
                                 sheet = 'caracteres_df')

raw_data=eea_raw_data


datos_todos=list()

enlaces_todos=raw_data[which(temp_raw_links$sector=='TODOS')]

for(i in 1:length(enlaces_todos)){

  #i=1
  message('TODOS LOS SECTORES',' - ',
          unique(as.data.frame(enlaces_todos[[i]][1])$year))

  names(enlaces_todos[[i]][[1]])=chartr(tildes, "AEIOU", toupper(names(enlaces_todos[[i]][[1]])))

  datos_todos[[i]]=enlaces_todos[[i]][[1]]%>%
    select(IRUC,CIIU,CCDD,CCPP,CCDI,YEAR)%>%
    rename(iruc=IRUC,
           year=YEAR)%>%
    mutate(iruc=as.character(iruc))

}

todos_conjunto=bind_rows(datos_todos)

# 14.reuniendo datos----

return(todos_conjunto)
}

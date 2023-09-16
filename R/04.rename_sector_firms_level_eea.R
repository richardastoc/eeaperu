#' Renombra, formatea o uniformiza los nombres
#'
#' @param only_firms_doc_link dataframe de enlaces de doc
#' @param eea_data_links dataframe de enlace de datos
#' @param correspondencia_datos excel de correspondencia datos
#' @param correspondencia_docs excel de correspondencia docs
#' @param tildes caracteres especiales, vocales mayusculas tildadas
#'
#' @return datafrme renombrado uniformemente
#' @export
#'
#' @examples
#' processing_sector_name(only_firms_doc_link,eea_data_links,correspondencia_datos,correspondencia_docs,tildes)
processing_sector_name=function(only_firms_doc_link,
                                eea_data_links,
                                correspondencia_datos,
                                correspondencia_docs,
                                tildes){#0. main directory and fixing your memory to download all data

memory.limit (20000)

library(tidyverse)
library(lubridate)
library(rvest)
library(haven)
library(readr)

# 1.DOCS ----


temp_doc_links_05<-only_firms_doc_link%>%
  filter(year<=2005)%>%
  mutate(nombre=chartr(tildes, "AEIOU", toupper(nombre)))%>%
  mutate(extra=str_remove_all(nombre,'EEA [0-9][0-9] ')%>%
           str_remove_all(' [0-9][0-9][0-9][0-9]'),
         solo_nombre=str_extract(extra,'[A-Z][A-Z]+ [A-Z]+ [A-Z][A-Z]+|[A-Z][A-Z]+ [A-Z]+|[A-Z][A-Z]+'),
         solo_formato=str_extract(extra,'^[A-Z][0-9]|[A-Z]'),
         sector=str_c(solo_nombre,' ',solo_formato)

      )
temp_doc_links_67<-only_firms_doc_link%>%
  filter(year %in% c(2006,2007))%>%
  mutate(nombre=chartr(tildes, "AEIOU", toupper(nombre)))%>%
  mutate(extra=str_sub(nombre,7,100)%>%
           str_remove_all(' [0-9][0-9][0-9][0-9]'),
         solo_nombre=str_extract(extra,'[A-Z][A-Z]+ [A-Z]+ [A-Z][A-Z]+|[A-Z][A-Z]+ [A-Z]+|[A-Z][A-Z]+'),
         solo_formato=str_extract(extra,'^[A-Z][0-9]|[A-Z]'),
         sector=str_c(solo_nombre,' ',solo_formato)%>%
           str_remove('EEA ')

  )

temp_doc_links_08<-only_firms_doc_link%>%
  filter(year==2008)%>%
  mutate(nombre=chartr(tildes, "AEIOU", toupper(nombre)))%>%
  mutate(extra=substr(nombre,9,100)%>%
           str_remove_all(' [0-9][0-9][0-9][0-9]'),
         solo_nombre=str_extract(extra,'[A-Z][A-Z]+ [A-Z]+ [A-Z][A-Z]+|[A-Z][A-Z]+ [A-Z]+|[A-Z][A-Z]+'),
         solo_formato=str_extract(extra,'^[A-Z][0-9]|[A-Z]'),
         sector=str_c(solo_nombre,' ',solo_formato)%>%
           str_remove('EEA ')

  )

temp_doc_links_09<-only_firms_doc_link%>%
  filter(year %in% c(2009))%>%
  mutate(nombre=chartr(tildes, "AEIOU", toupper(nombre)))%>%
  mutate(extra=str_sub(nombre,7,100)%>%
           str_remove_all(' [0-9][0-9][0-9][0-9]'),
         solo_nombre=str_extract(extra,'[A-Z][A-Z]+ [A-Z]+ [A-Z][A-Z]+|[A-Z][A-Z]+ [A-Z]+|[A-Z][A-Z]+'),
         solo_formato=str_extract(extra,'^[A-Z][0-9]|[A-Z]'),
         sector=str_c(solo_nombre,' ',solo_formato)%>%
           str_remove('EEA ')

  )%>%
  mutate(
    verdad=str_detect(extra,'_'),
    sector=case_when(
      verdad==FALSE~sector,
      verdad==TRUE~'COMERCIO M'
    )
  )

temp_doc_links_10<-only_firms_doc_link%>%
  filter(year %in% c(2010,2011))%>%
  mutate(nombre=chartr(tildes, "AEIOU", toupper(nombre)))%>%
  mutate(extra=str_sub(nombre,5,100)%>%
           str_remove_all(' [0-9][0-9][0-9][0-9]'),
         solo_nombre=str_extract(extra,'[A-Z][A-Z]+ [A-Z]+ [A-Z][A-Z]+|[A-Z][A-Z]+ [A-Z]+|[A-Z][A-Z]+'),
         solo_formato=str_extract(extra,'^[A-Z][0-9]|[A-Z]'),
         sector=str_c(solo_nombre,' ',solo_formato)%>%
           str_remove('EEA '),
         sector=case_when(
           sector=='ACUICULTURA A'~'ACUICULTURA',
           TRUE~sector
         )

  )

temp_doc_links_12_14<-only_firms_doc_link%>%
  filter(year>=2012&year<=2014)%>%
  mutate(nombre=chartr(tildes, "AEIOU", toupper(nombre)))%>%
  mutate(extra=str_sub(nombre,7,100)%>%
           str_remove_all(' [0-9][0-9][0-9][0-9]'),
         solo_nombre=str_extract(extra,'[A-Z][A-Z]+ [A-Z]+ [A-Z][A-Z]+|[A-Z][A-Z]+ [A-Z]+|[A-Z][A-Z]+'),
         solo_formato=str_extract(extra,'^[A-Z][0-9]|[A-Z]'),
         sector=str_c(solo_nombre,' ',solo_formato)%>%
           str_remove('EEA '),
         sector=case_when(
           sector=='QUE Q'~'PEQUENAS Y MEDIANA M',
           TRUE~sector
         )

  )

temp_doc_links_20<-only_firms_doc_link%>%
  filter(year>=2015)%>%
  mutate(nombre=chartr(tildes, "AEIOU", toupper(nombre)))%>%
  mutate(sector=str_remove_all(nombre,' [0-9][0-9][0-9][0-9]')

  )

#binding

data_all=bind_rows(temp_doc_links_05,
                   temp_doc_links_67,
                   temp_doc_links_08,
                   temp_doc_links_09,
                   temp_doc_links_10,
                   temp_doc_links_12_14,
                   temp_doc_links_20)%>%
  select(year,enlaces_datos,nombre,sector)


# 2.Data ----

temp_data_links<-eea_data_links%>%
  mutate(nombre=chartr(tildes, "AEIOU", toupper(nombre)),
         sector=str_remove(nombre,'[0-9][0-9][0-9][0-9]')%>%
           str_remove('EEA 08 '),
         sector=case_when(
           sector=='AGENCIAS DE VIAJES  B '~'AGENCIAS DE VIAJES  B',
           sector=='AGENCIA DE VIAJES B '~'AGENCIA DE VIAJES B',
           sector=='PESCA  A  '~'PESCA  A',
           sector=='PESCA  P     '~'PESCA  P',
           sector=='HIDROCARBUROS U '~'HIDROCARBUROS U',
           sector=='HIDROCARBUROS  F1 '~'HIDROCARBUROS F1',
           sector=='PESCA P  '~'PESCA P',
           TRUE~sector
         ))

nombre_dato=as.data.frame(unique(temp_data_links$sector))%>%
  rename(nombres='unique(temp_data_links$sector)')%>%
  mutate(sector_comun='')

writexl::write_xlsx(nombre_dato,'correspondencia_sector_dat.xlsx')

nombre_docs=as.data.frame(unique(data_all$sector))%>%
  rename(nombres='unique(data_all$sector)')%>%
  mutate(sector_comun='')

writexl::write_xlsx(nombre_docs,'correspondencia_sector_docs.xlsx')

# 3. matches ----

corr_data=readxl::read_xlsx(correspondencia_datos)%>%
  rename(sector=nombres)
corr_doc=readxl::read_xlsx(correspondencia_docs)%>%
  rename(sector=nombres)

all_data_corr=temp_data_links%>%
  left_join(corr_data,by='sector')%>%
  unique()%>%
  select(year,enlaces_capitulos,enlaces_datos,sector,sector_comun)%>%
  mutate(sector=sector_comun)%>%
  select(-sector_comun)

all_docs_corr=data_all%>%
  left_join(corr_doc,by='sector')%>%
  unique()%>%
  select(year,enlaces_datos,sector_comun)%>%
  mutate(sector=sector_comun)%>%
  select(-sector_comun)

return(list(doc_links=all_docs_corr,dat_links=all_data_corr))

}

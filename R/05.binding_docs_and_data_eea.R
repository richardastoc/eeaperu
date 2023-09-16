#' Une ambos dataframes para cada firma.
#'
#' @param all_docs_corr dataframe para documentos
#' @param all_data_corr dataframe para datos
#'
#' @return devuelve un dataframe
#' @export
#'
#' @examples
#' all_link_df(all_docs_corr,all_data_corr)
all_link_df=function(all_docs_corr,
                     all_data_corr){
#0. main directory and fixing your memory to download all data

memory.limit (20000)

library(tidyverse)
library(lubridate)
library(rvest)
library(haven)
library(readr)
library(stringr)


#1. charge all_links

temp_data<-all_data_corr%>%
  arrange(year,sector)%>%
  filter(year<=2009 | year>=2014)%>%
  mutate(
    sector=case_when(
      str_detect(sector,'  ')~str_replace(sector,'  ',' '),
      TRUE~sector
    )
  )


temp_docs<-all_docs_corr%>%
  rename(enlaces_docs=enlaces_datos)%>%
  arrange(year,sector)%>%
  filter(year<=2009 | year>=2014)

temp_group_data_first=temp_data%>%
  left_join(temp_docs,
            by=c('year','sector'))

# 2. 2010 ----

temp_data_10<-all_data_corr%>%
  arrange(year,sector)%>%
  filter(year==2010)%>%
  mutate(
    sector=case_when(
      str_detect(sector,'  ')~str_replace(sector,'  ',' '),
      TRUE~sector
    )
  )%>%
  filter(sector!='RESTAURANTES R1')


temp_docs_10<-all_docs_corr%>%
  rename(enlaces_docs=enlaces_datos)%>%
  arrange(year,sector)%>%
  filter(year==2010)%>%
  mutate(sector=case_when(
    sector=='ACUICULTURA'~'ACUICULTURA A',
    TRUE~sector
  ))

AGROINDUSTRIA_M=temp_docs_10[13,]%>%
  mutate(sector='AGROINDUSTRIA M')

COMERCIO_M=temp_docs_10[13,]%>%
  mutate(sector='COMERCIO M')

CONSTRUCCION_M=temp_docs_10[13,]%>%
  mutate(sector='CONSTRUCCION M')

MANUFACTURA_M=temp_docs_10[13,]%>%
  mutate(sector='MANUFACTURA M')

SERVICIOS_M=temp_docs_10[13,]%>%
  mutate(sector='SERVICIOS M')

TRANSPORTES_M=temp_docs_10[13,]%>%
  mutate(sector='TRANSPORTES M')

temp_docs_10_full=rbind(temp_docs_10,
                        AGROINDUSTRIA_M,
                        COMERCIO_M,
                        CONSTRUCCION_M,
                        MANUFACTURA_M,
                        SERVICIOS_M,
                        TRANSPORTES_M)%>%
  filter(sector!='PEQUENAS M')%>%arrange(year,sector)


temp_group_data_second=temp_data_10%>%
  left_join(temp_docs_10_full,
            by=c('year','sector'))

# 3. 2011 ----

temp_data_11<-all_data_corr%>%
  arrange(year,sector)%>%
  filter(year==2011)%>%
  mutate(
    sector=case_when(
      str_detect(sector,'  ')~str_replace(sector,'  ',' '),
      TRUE~sector
    )
  )


temp_docs_11<-all_docs_corr%>%
  rename(enlaces_docs=enlaces_datos)%>%
  arrange(year,sector)%>%
  filter(year==2011)%>%
  mutate(sector=case_when(
    sector=='ACUICULTURA'~'ACUICULTURA A',
    TRUE~sector
  ))

AGROINDUSTRIA_M1=temp_docs_11[13,]%>%
  mutate(sector='AGROINDUSTRIA M')

COMERCIO_M1=temp_docs_11[13,]%>%
  mutate(sector='COMERCIO M')

CONSTRUCCION_M1=temp_docs_11[13,]%>%
  mutate(sector='CONSTRUCCION M')

MANUFACTURA_M1=temp_docs_11[13,]%>%
  mutate(sector='MANUFACTURA M')

SERVICIOS_M1=temp_docs_11[13,]%>%
  mutate(sector='SERVICIOS M')

TRANSPORTES_M1=temp_docs_11[13,]%>%
  mutate(sector='TRANSPORTES M')

temp_docs_11_full=rbind(temp_docs_11,
                        AGROINDUSTRIA_M1,
                        COMERCIO_M1,
                        CONSTRUCCION_M1,
                        MANUFACTURA_M1,
                        SERVICIOS_M1,
                        TRANSPORTES_M1)%>%
  filter(sector!='PEQUENAS M')%>%arrange(year,sector)


temp_group_data_third=temp_data_11%>%
  left_join(temp_docs_11_full,
            by=c('year','sector'))


# 4. 2012 ----

temp_data_12<-all_data_corr%>%
  arrange(year,sector)%>%
  filter(year==2012)%>%
  mutate(
    sector=case_when(
      str_detect(sector,'  ')~str_replace(sector,'  ',' '),
      TRUE~sector
    )
  )%>%
  filter(sector!='ACUICULTURA A')


temp_docs_12<-all_docs_corr%>%
  rename(enlaces_docs=enlaces_datos)%>%
  arrange(year,sector)%>%
  filter(year==2012)%>%
  mutate(sector=case_when(
    sector=='ACUICULTURA'~'ACUICULTURA A',
    TRUE~sector
  ))

AGROINDUSTRIA_M2=temp_docs_12[9,]%>%
  mutate(sector='AGROINDUSTRIA M')

COMERCIO_M2=temp_docs_12[9,]%>%
  mutate(sector='COMERCIO M')

CONSTRUCCION_M2=temp_docs_12[9,]%>%
  mutate(sector='CONSTRUCCION M')

MANUFACTURA_M2=temp_docs_12[9,]%>%
  mutate(sector='MANUFACTURA M')

SERVICIOS_M2=temp_docs_12[9,]%>%
  mutate(sector='SERVICIOS M')

TRANSPORTES_M2=temp_docs_12[9,]%>%
  mutate(sector='TRANSPORTES M')

temp_docs_12_full=rbind(temp_docs_12,
                        AGROINDUSTRIA_M2,
                        COMERCIO_M2,
                        CONSTRUCCION_M2,
                        MANUFACTURA_M2,
                        SERVICIOS_M2,
                        TRANSPORTES_M2)%>%
  filter(sector!='PEQUENAS M')%>%arrange(year,sector)


temp_group_data_fourth=temp_data_12%>%
  left_join(temp_docs_12_full,
            by=c('year','sector'))

# 5. 2013 ----

temp_data_13<-all_data_corr%>%
  arrange(year,sector)%>%
  filter(year==2013)%>%
  mutate(
    sector=case_when(
      str_detect(sector,'  ')~str_replace(sector,'  ',' '),
      TRUE~sector
    )
  )%>%
  filter(sector!='TRANSPORTES M')

temp_docs_13<-all_docs_corr%>%
  rename(enlaces_docs=enlaces_datos)%>%
  arrange(year,sector)%>%
  filter(year==2013)%>%
  mutate(sector=case_when(
    sector=='ACUICULTURA'~'ACUICULTURA A',
    TRUE~sector
  ))

temp_group_data_fifth=temp_data_13%>%
  left_join(temp_docs_13,
            by=c('year','sector'))

temp_all_data=rbind(temp_group_data_first,
                    temp_group_data_second,
                    temp_group_data_third,
                    temp_group_data_fourth,
                    temp_group_data_fifth)


#write_rds(temp_all_data,'eea_all_links_df.rds')

return(temp_all_data)

}

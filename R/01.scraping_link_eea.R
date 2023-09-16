#' Scraper de enlaces de datos
#'
#' @param start_date Periodo de inicio
#'
#' @return Df de enlaces
#' @export
#'
#' @examples
#' scraping_eea_datalink_function(start_date)
scraping_eea_datalink_function=function(start_date){
# 0. CONFIGURANDO EL SERVIDOR PARA SELENIUM
if (!"RSelenium" %in% installed.packages()[,"Package"]){install.packages("RSelenium")}
if (!"tidyverse" %in% installed.packages()[,"Package"]){install.packages("tidyverse")}
if (!"lubridate" %in% installed.packages()[,"Package"]){install.packages("lubridate")}
if (!"rvest" %in% installed.packages()[,"Package"]){install.packages("rvest")}
library(RSelenium)
library(tidyverse)
library(lubridate)
library(rvest)
#0. interest year
start_date<-start_date
#1. open browser
fdriver <- RSelenium::rsDriver(browser = c('chrome'),
                               port = 4444L,
                               chromever = 'latest', version="latest",
                               verbose = FALSE)
driver<-fdriver[['client']]
driver$maxWindowSize()
#2. microdata link
main_url<-'https://proyectos.inei.gob.pe/microdatos/'
driver$navigate(main_url)
Sys.sleep(runif(1,2,5))
open_query<-driver$findElement(using='xpath','//*[@id="jsmenu"]/li[1]/a')
open_query$clickElement()
Sys.sleep(runif(1,2,5))
# 2.1 access to EEA poll
open_poll<-driver$findElement(using = 'xpath','//*[@id="select2-cmbEncuesta0ID-container"]')
open_poll$clickElement()
Sys.sleep(runif(1,2,5))
n=0
while(n<=23){
  open_poll$sendKeysToActiveElement(list(key='down_arrow'))
  n=n+1
}
open_poll$sendKeysToActiveElement(list(key='enter'))
Sys.sleep(runif(1,2,5))
# 2.3 access to years
page_to_year<-driver$getPageSource()[[1]]
temp_year<-read_html(page_to_year)%>%
  html_node('#divAnio')%>%
  html_nodes('option')%>%
  html_text()
interest_year<-which(str_detect(temp_year,start_date))
# 2.4 access to trim for every year
links_temp<-list()
for (y in interest_year:length(temp_year)) {
  #y=2
  open_year<-driver$findElement(using = 'xpath',
                                str_c('//*[@id="divAnio"]/select/option[',y,']'))
  open_year$clickElement()
  Sys.sleep(runif(1,2,5))
  message('scrapping links : ',str_c(temp_year[y]))
  #t=3
  open_anual<-driver$findElement('xpath',
                                 str_c('//*[@id="divPeriodo"]/select/option[2]'))
  open_anual$clickElement()
  Sys.sleep(runif(1,5,10))
  #2.5 scrapper to links
  page_to_scraper<-driver$getPageSource()[[1]]

  links_down<-read_html(page_to_scraper)%>%
    html_node('#divDetalle')%>%
    html_nodes(xpath='//*[@id="divDetalle"]/table/tbody/tr/td/table/tbody/tr/td[11]')%>%
    html_nodes('a')%>%
    html_attr('href')
  links_data_temp=tibble(enlaces_datos=str_c('https://proyectos.inei.gob.pe',links_down),
                         year=temp_year[y])
  caps_name_link=list()
  max=length(links_down)+1
  for (j in 2:max) {
    #j=3
    uno_link='https://proyectos.inei.gob.pe/microdatos/Detalle_Encuesta.asp?CU=19558&CodEncuesta='
    dos_link=read_html(page_to_scraper)%>%
      html_node('#divDetalle')%>%
      html_nodes(xpath=str_c('//*[@id="divDetalle"]/table/tbody/tr/td/table/tbody/tr[',j,']/td[4]'))%>%
      html_text()
    tres_link='&CodModulo='
    cuatro_link=read_html(page_to_scraper)%>%
      html_node('#divDetalle')%>%
      html_nodes(xpath=str_c('//*[@id="divDetalle"]/table/tbody/tr/td/table/tbody/tr[',j,']/td[6]'))%>%
      html_text()
    cinco_link='&NombreEncuesta=ENCUESTA+ECON?MICA+ANUAL+-+EEA&NombreModulo='
    nombre_link=read_html(page_to_scraper)%>%
      html_node('#divDetalle')%>%
      html_nodes(xpath=str_c('//*[@id="divDetalle"]/table/tbody/tr/td/table/tbody/tr[',j,']/td[7]'))%>%
      html_text()%>%
      str_remove_all('\n')%>%
      str_remove_all('\t')%>%
      str_replace_all(' ','+')
    nombre_sector=read_html(page_to_scraper)%>%
      html_node('#divDetalle')%>%
      html_nodes(xpath=str_c('//*[@id="divDetalle"]/table/tbody/tr/td/table/tbody/tr[',j,']/td[7]'))%>%
      html_text()%>%
      str_remove_all('\n')%>%
      str_remove_all('\t')
    if(str_sub(nombre_link,str_length(nombre_link))=='+'){
      nombre_link=str_sub(nombre_link,1,str_length(nombre_link)-1)
    }else{
      nombre_link=nombre_link
    }
    caps_name_link[[j-1]]=tibble(enlaces_capitulos=str_c(uno_link,
                                                         dos_link,
                                                         tres_link,
                                                         cuatro_link,
                                                         cinco_link,
                                                         nombre_link),
                                 year=temp_year[y],
                                 nombre=nombre_sector)
  }
  links_caps_temp=bind_rows(caps_name_link)
  links_temp[[y-1]]=bind_cols(links_caps_temp,links_data_temp)%>%
    mutate(year=year...2)%>%
    select(year,enlaces_capitulos,enlaces_datos,nombre)%>%
    mutate(sector='')
}

driver$close()

all_link<-bind_rows(links_temp)

return(all_link)

}

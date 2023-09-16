#' Scraper de enlaces de datos
#'
#' @param start_date Periodo de inicio
#'
#' @return Df de enlaces
#' @export
#'
#' @examples
#' scraping_eea_doclink_function(start_date)
scraping_eea_doclink_function=function(start_date){
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
open_query<-driver$findElement(using='xpath','//*[@id="jsmenu"]/li[2]/a')
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
    html_nodes(xpath='//*[@id="divDetalle"]/table/tbody/tr/td/table/tbody/tr/td[9]')%>%
    html_nodes('a')%>%
    html_attr('href')

  caps_name_link=read_html(page_to_scraper)%>%
    html_node('#divDetalle')%>%
    html_nodes(xpath='//*[@id="divDetalle"]/table/tbody/tr/td/table/tbody/tr/td[7]')%>%
    html_text()%>%
    str_remove_all('\n')%>%
    str_remove_all('\t')%>%.[-1]


  links_data_temp=tibble(enlaces_datos=str_c('https://proyectos.inei.gob.pe',links_down),
                         year=temp_year[y],
                         nombre=caps_name_link,
                         sector='')

  links_temp[[y-1]]=links_data_temp
}

driver$close()

all_link<-bind_rows(links_temp)

return(all_link)

}

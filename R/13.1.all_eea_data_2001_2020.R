#' Datos peru
#'
#' @param datos_bloue1 datos bloque 1, o periodo 1
#' @param datos_bloue2 datos bloque 2
#' @param datos_bloue3 datos bloque 3
#' @param datos_bloue4 datos bloque 4
#' @param datos_dap1 datos generales
#'
#' @return dataframe para peru
#' @export
#'
#' @examples
#' datos_peru(datos_bloue1,datos_bloue2,datos_bloue3,datos_bloue4,datos_dap1)
datos_peru=function(datos_bloue1,
                    datos_bloue2,
                    datos_bloue3,
                    datos_bloue4,
                    datos_dap1){#0. main directory and fixing your memory to download all data

memory.limit (20000)

library(tidyverse)
library(lubridate)
library(rvest)
library(haven)
library(readr)
#1.1 cargando datos


ventas_2011=datos_bloue1[[1]]
insumos_2011=datos_bloue1[[2]]
resultados_2011=datos_bloue1[[3]]
activo_fijo_2011=datos_bloue1[[4]]
activo_contable_2011=datos_bloue1[[5]]
impuesto_2011=datos_bloue1[[6]]
intangibles_2011=datos_bloue1[[7]]
publicidad_2011=datos_bloue1[[8]]

ventas_2012=datos_bloue2[[1]]
insumos_2012=datos_bloue2[[2]]
resultados_2012=datos_bloue2[[3]]
activo_fijo_2012=datos_bloue2[[4]]
activo_contable_2012=datos_bloue2[[5]]
impuesto_2012=datos_bloue2[[6]]
intangibles_2012=datos_bloue2[[7]]
publicidad_2012=datos_bloue2[[8]]

ventas_2019=datos_bloue3[[1]]
insumos_2019=datos_bloue3[[2]]
resultados_2019=datos_bloue3[[3]]
activo_fijo_2019=datos_bloue3[[4]]
activo_contable_2019=datos_bloue3[[5]]
impuesto_2019=datos_bloue3[[6]]
intangibles_2019=datos_bloue3[[7]]
publicidad_2019=datos_bloue3[[8]]


ventas_2020=datos_bloue4[[1]]
insumos_2020=datos_bloue4[[2]]
resultados_2020=datos_bloue4[[3]]
activo_fijo_2020=datos_bloue4[[4]]
activo_contable_2020=datos_bloue4[[5]]
impuesto_2020=datos_bloue4[[6]]
intangibles_2020=datos_bloue4[[7]]
publicidad_2020=datos_bloue4[[8]]

todos_conjunto_2020=datos_dap1

datos_ventas1=rbind(ventas_2011,ventas_2012,ventas_2019,ventas_2020)%>%
  left_join(todos_conjunto_2020%>%unique(),
            by=c('year','iruc'))%>%
  unique()%>%
  na.omit()

#write_rds(datos_ventas1,'datos_ventas_peru.rds')

datos_insumos1=rbind(insumos_2011,insumos_2012,insumos_2019,insumos_2020)%>%
  left_join(todos_conjunto_2020,
            by=c('year','iruc'))%>%
  unique()%>%
  na.omit()

#write_rds(datos_insumos1,'datos_insumos_peru.rds')

datos_impuesto1=rbind(impuesto_2011,impuesto_2012,impuesto_2019,impuesto_2020)%>%
  left_join(todos_conjunto_2020,
            by=c('year','iruc'))%>%
  unique()%>%
  na.omit()

#write_rds(datos_impuesto1,'datos_impuesto_peru.rds')

datos_acfijo1=rbind(activo_fijo_2011,activo_fijo_2012,activo_fijo_2019,activo_fijo_2020)%>%
  left_join(todos_conjunto_2020,
            by=c('year','iruc'))%>%
  unique()%>%
  na.omit()

#write_rds(datos_acfijo1,'datos_acfijo_peru.rds')

datos_resultados1=rbind(resultados_2011,resultados_2012,resultados_2019,resultados_2020)%>%
  left_join(todos_conjunto_2020,
            by=c('year','iruc'))%>%
  unique()%>%
  na.omit()

#write_rds(datos_resultados1,'datos_resultados_peru.rds')

datos_activos1=rbind(activo_contable_2011,activo_contable_2012,activo_contable_2019,activo_contable_2020)%>%
  left_join(todos_conjunto_2020,
            by=c('year','iruc'))%>%
  unique()%>%
  na.omit()

#write_rds(datos_activos1,'datos_activos_peru.rds')

datos_intangibles1=rbind(intangibles_2011,intangibles_2012,intangibles_2019,intangibles_2020)%>%
  left_join(todos_conjunto_2020,
            by=c('year','iruc'))%>%
  unique()%>%
  na.omit()

#write_rds(datos_intangibles1,'datos_intangibles_peru.rds')

datos_publicidad1=rbind(publicidad_2011,publicidad_2012,publicidad_2019,publicidad_2020)%>%
  left_join(todos_conjunto_2020,
            by=c('year','iruc'))%>%
  unique()%>%
  na.omit()

#write_rds(datos_publicidad1,'datos_publicidad_peru.rds')

datos_peru_01_20=datos_ventas1%>%
  left_join(datos_insumos1,
            by=c('iruc','year','sector','CIIU','CCDD','CCPP','CCDI'))%>%
  left_join(datos_impuesto1,
            by=c('iruc','year','sector','CIIU','CCDD','CCPP','CCDI'))%>%
  left_join(datos_acfijo1,
            by=c('iruc','year','sector','CIIU','CCDD','CCPP','CCDI'))%>%
  left_join(datos_resultados1,
            by=c('iruc','year','sector','CIIU','CCDD','CCPP','CCDI'))%>%
  left_join(datos_activos1,
            by=c('iruc','year','sector','CIIU','CCDD','CCPP','CCDI'))%>%
  left_join(datos_intangibles1,
            by=c('iruc','year','sector','CIIU','CCDD','CCPP','CCDI'))%>%
  left_join(datos_publicidad1,
            by=c('iruc','year','sector','CIIU','CCDD','CCPP','CCDI'))%>%
  unique()

# se pierde alrededor de 2mil datos en las demas variables, pero,
# como maximo representa el 3% del total de la muestra

#write_rds(datos_peru_01_20,'datos_peru_01_20.rds')

return(datos_peru_01_20)

}

#' Selecciona datos de 2000 a 2011, por semejanza
#'
#' @param eea_data_links_variables_df_manual excel de capitulos
#' @param eea_raw_data datos brutos
#'
#' @return lista de dataframe, una para cada variable
#' @export
#'
#' @examples
#' select_2000_2011(eea_data_links_variables_df_manual,eea_raw_data)
select_2000_2011=function(eea_data_links_variables_df_manual,
                          eea_raw_data){#0. main directory and fixing your memory to download all data

memory.limit (20000)

library(tidyverse)
library(lubridate)
library(rvest)
library(haven)
library(readr)

#1. charge all_links

temp_raw_links<-readxl::read_xlsx(eea_data_links_variables_df_manual,
                                  sheet = 'data_df')

raw_data=eea_raw_data

# (i in 1:484): nivel sectorial para cada ano, sin contar "TODOS".

datos_ventas=list()
datos_insumos=list()
datos_resultados=list()
datos_acfijos=list()
datos_activos=list()
datos_impuestos=list()
datos_intangibles=list()
datos_publicidad=list()

for(i in 1:285){#length(raw_data)){

  #i=285

  message(unique(as.data.frame(raw_data[[i]][1])$sector),' - ',
          unique(as.data.frame(raw_data[[i]][1])$year))

  if(length(unique(as.data.frame(raw_data[[i]][1])$sector))!=0){
  if(unique(as.data.frame(raw_data[[i]][1])$sector)!='TODOS'){

  #(j in 1:cantidad_de_capitulos_para_cada_sector):nivel capitulos

  for (j in 2:length(raw_data[[i]])) {

    #j=3

    #message(unique(as.data.frame(raw_data[[i]][j])$sector),
     #       ' - ',
      #      unique(as.data.frame(raw_data[[i]][j])$year),
       #     ' -  Capitulo: ',
        #    unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))
    if(dim(as.data.frame(raw_data[[i]][j]))[1]!=0){

    if(!is.null(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){

      cap_activos=temp_raw_links[i,6]
      cap_patrimo=temp_raw_links[i,9]
      cap_ventas=temp_raw_links[i,12]
      cap_insumo=temp_raw_links[i,15]
      cap_taunet=temp_raw_links[i,18]
      cap_acinta=temp_raw_links[i,21]
      cap_public=temp_raw_links[i,24]
      cap_result=temp_raw_links[i,27]
      cap_ubruta=temp_raw_links[i,30]
      cap_uopera=temp_raw_links[i,33]
      cap_acfijo=temp_raw_links[i,36]
      cap_empleo=temp_raw_links[i,39]
      cap_salari=temp_raw_links[i,42]

      pre_activos=temp_raw_links[i,7]
      pre_patrimo=temp_raw_links[i,10]
      pre_ventas=temp_raw_links[i,13]
      pre_insumo=temp_raw_links[i,16]
      pre_taunet=temp_raw_links[i,19]
      pre_acinta=temp_raw_links[i,22]
      pre_public=temp_raw_links[i,25]
      pre_result=temp_raw_links[i,28]
      pre_ubruta=temp_raw_links[i,31]
      pre_uopera=temp_raw_links[i,34]
      pre_acfijo=temp_raw_links[i,37]
      pre_empleo=temp_raw_links[i,40]
      pre_salari=temp_raw_links[i,43]

      cla_activos=temp_raw_links[i,8]
      cla_patrimo=temp_raw_links[i,11]
      cla_ventass=temp_raw_links[i,14]
      cla_insumo=temp_raw_links[i,17]
      cla_taunet=temp_raw_links[i,20]
      cla_acinta=temp_raw_links[i,23]
      cla_public=temp_raw_links[i,26]
      cla_result=temp_raw_links[i,29]
      cla_ubruta=temp_raw_links[i,32]
      cla_uopera=temp_raw_links[i,35]
      cla_acfijo=temp_raw_links[i,38]
      cla_empleo=temp_raw_links[i,41]
      cla_salari=temp_raw_links[i,44]

      #1. activos: ----

      if(!is.na(cap_activos[[1]])){

        if(!is.na(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){

          if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_activos[[1]]){

            message('activos contables')

            preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

            pre_activos1=str_c('RUC|iruc|Clave|CLAVE|',pre_activos[[1]])

            sub_set=preguntas[str_detect(names(preguntas),pre_activos1)]

            data_set_activos=sub_set[which(sub_set$Clave==cla_activos[[1]]),]%>%
              select(-Clave)

            names(data_set_activos)=c('iruc','activos')


            datos_activos[[i]]=data_set_activos%>%
              mutate(year=temp_raw_links[i,1][[1]],
                     sector=temp_raw_links[i,5][[1]],
                     activos=as.integer(activos))

          }

        }
      }
      #
      # #2. patrimonio: ----
      #
      # if(!is.na(cap_patrimo[[1]])){
      #
      # if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_patrimo[[1]]){
      #
      #   message('patrimonio-contable')
      #
      # }
      # }

      #3. ventas: ----



      if(!is.na(cap_ventas[[1]])){

      if(!is.na(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){

      if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_ventas[[1]]){

        message('ventas')

        #preguntas de ventas esta en la columna 13 del excel para todo sector

        if(isTRUE(as.character(temp_raw_links[i,13][[1]]) %in% c('1','2','3','4','5','6','7','8','9'))){

          # 3.1. Agroindustria F1 y Manufactura D1 tienen preguntas 1 y 2

          if(temp_raw_links[i,5][[1]]=='AGROINDUSTRIA F1'){


            preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

            pre_ventas1='iruc|RUC|Clave|CLAVE|1|2'

            sub_set=preguntas[str_detect(names(preguntas),pre_ventas1)]

            data_set_venta=sub_set[which(sub_set$Clave==cla_ventass[[1]]),]%>%
              select(-Clave)%>%
              group_by(iruc)%>%
              mutate(ventas=P01+P02,
                     ventas=as.integer(ventas))


          }

          if(temp_raw_links[i,5][[1]]=='MANUFACTURA D1'){


            preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

            pre_ventas1='iruc|RUC|Clave|CLAVE|1|2'

            sub_set=preguntas[str_detect(names(preguntas),pre_ventas1)]

            data_set_venta=sub_set[which(sub_set$Clave==cla_ventass[[1]]),]%>%
              select(-Clave)%>%
              group_by(iruc)%>%
              mutate(ventas=P01+P02,
                     ventas=as.integer(ventas))


          }



          preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

          pre_ventas1=str_c('iruc|Clave|CLAVE|RUC|',pre_ventas[[1]])

          sub_set=preguntas[str_detect(names(preguntas),pre_ventas1)]

          data_set_venta=sub_set[which(sub_set$Clave==cla_ventass[[1]]),]%>%
            select(-Clave)

          names(data_set_venta)=c('iruc','ventas')

          data_set_venta=data_set_venta%>%
            mutate(ventas=as.integer(ventas))



        }

        if(isTRUE(str_detect(temp_raw_links[i,13][[1]],'y')) && temp_raw_links[i,1][[1]]==2001){

          # 3.2. Viajes B 2001, puse 1y2 en el excel, por ello este condicional.

          preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

          pre_ventas='RUC|iruc|Clave|CLAVE|2|4|6'

          sub_set=preguntas[str_detect(names(preguntas),pre_ventas)]

          data_set_venta=sub_set[which(sub_set$Clave==cla_ventass[[1]]),]%>%
            select(-Clave)%>%
            group_by(iruc)%>%
            mutate(ventas=P02+P04+P06)%>%
            select(-P02,-P04,-P06)


        }

        datos_ventas[[i]]=data_set_venta%>%
          mutate(year=temp_raw_links[i,1][[1]],
                 sector=temp_raw_links[i,5][[1]],
                 ventas=as.integer(ventas))


      }
      }

      }

      #4. insumos: ----

      if(!is.na(cap_insumo[[1]])){

        if(!is.na(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){

      if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_insumo[[1]]){

        message('insumos')

        preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

        pre_insumos1=str_c('RUC|iruc|Clave|CLAVE|',pre_insumo[[1]])

        sub_set=preguntas[str_detect(names(preguntas),pre_insumos1)]

        data_set_insumo=sub_set[which(sub_set$Clave==cla_insumo[[1]]),]%>%
          select(-Clave)

        names(data_set_insumo)=c('iruc','insumos')


        datos_insumos[[i]]=data_set_insumo%>%
          mutate(year=temp_raw_links[i,1][[1]],
                 sector=temp_raw_links[i,5][[1]],
                 insumos=as.integer(insumos))

      }

       }
      }

      #5. impuestos: ----
      #
      if(!is.na(cap_taunet[[1]])){

        if(!is.na(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){

          if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_taunet[[1]]){

            message('impuesto')

            preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

            pre_taunet1=str_c('RUC|iruc|Clave|CLAVE|',pre_taunet[[1]])

            sub_set=preguntas[str_detect(names(preguntas),pre_taunet1)]

            data_set_taunet=sub_set[which(sub_set$Clave==cla_taunet[[1]]),]%>%
              select(-Clave)

            names(data_set_taunet)=c('iruc','impuesto')


            datos_impuestos[[i]]=data_set_taunet%>%
              mutate(year=temp_raw_links[i,1][[1]],
                     sector=temp_raw_links[i,5][[1]],
                     impuesto=as.integer(impuesto))

          }

        }
      }
      #
      # #6. activos intangibles: ----
      #
      if(!is.na(cap_acinta[[1]])){

        if(!is.na(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){

          if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_acinta[[1]]){

            message('intangibles')

            preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

            pre_acinta1=str_c('RUC|iruc|Clave|CLAVE|',pre_acinta[[1]])

            sub_set=preguntas[str_detect(names(preguntas),pre_acinta1)]

            data_set_acinta=sub_set[which(sub_set$Clave==cla_acinta[[1]]),]%>%
              select(-Clave)

            names(data_set_acinta)=c('iruc','intangibles')


            datos_intangibles[[i]]=data_set_acinta%>%
              mutate(year=temp_raw_links[i,1][[1]],
                     sector=temp_raw_links[i,5][[1]],
                     intangibles=as.integer(intangibles))

          }

        }
      }
      #
      # #7. publicidad: ----
      #
      if(!is.na(cap_public[[1]])){

        if(!is.na(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){

          if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_public[[1]]){

            message('publicidad')

            preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

            pre_public1=str_c('RUC|iruc|Clave|CLAVE|',pre_public[[1]])

            sub_set=preguntas[str_detect(names(preguntas),pre_public1)]

            data_set_public=sub_set[which(sub_set$Clave==cla_public[[1]]),]%>%
              select(-Clave)

            names(data_set_public)=c('iruc','publicidad')


            datos_publicidad[[i]]=data_set_public%>%
              mutate(year=temp_raw_links[i,1][[1]],
                     sector=temp_raw_links[i,5][[1]],
                     publicidad=as.integer(publicidad))

          }

        }
      }
      #
      # #8. resultados: ----
      #
      if(!is.na(cap_result[[1]])){

        if(!is.na(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){

          if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_result[[1]]){

            message('resultados')

            preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

            pre_result1=str_c('RUC|iruc|Clave|CLAVE|',pre_result[[1]])

            sub_set=preguntas[str_detect(names(preguntas),pre_result1)]

            data_set_result=sub_set[which(sub_set$Clave==cla_result[[1]]),]%>%
              select(-Clave)

            names(data_set_result)=c('iruc','resultados')


            datos_resultados[[i]]=data_set_result%>%
              mutate(year=temp_raw_links[i,1][[1]],
                     sector=temp_raw_links[i,5][[1]],
                     resultados=as.integer(resultados))

          }

        }
      }
      #
      # #9. utilidad bruta: ----
      #
      # if(!is.na(cap_ubruta[[1]])){
      #
      # if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_ubruta[[1]]){
      #
      #   message('utilidad bruta')
      #
      # }
      # }
      #
      # #10. utilidad operativa: ----
      #
      # if(!is.na(cap_uopera[[1]])){
      #
      # if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_uopera[[1]]){
      #
      #   message('utilidad operativa')
      #
      # }
      # }
      #
      # #11. activo fijo: ----
      #
      if(!is.na(cap_acfijo[[1]])){

        if(!is.na(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){

          if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_acfijo[[1]]){

            message('activos fijos')

            preguntas=as.data.frame(raw_data[[i]][j])[str_detect(names(as.data.frame(raw_data[[i]][j])),'P|Clave|CLAVE|iruc|RUC')]

            pre_acfijo1=str_c('RUC|iruc|Clave|CLAVE|',pre_acfijo[[1]])

            sub_set=preguntas[str_detect(names(preguntas),pre_acfijo1)]

            data_set_acfijo=sub_set[which(sub_set$Clave==cla_acfijo[[1]]),]%>%
              select(-Clave)

            names(data_set_acfijo)=c('iruc','acfijo')


            datos_acfijos[[i]]=data_set_acfijo%>%
              mutate(year=temp_raw_links[i,1][[1]],
                     sector=temp_raw_links[i,5][[1]],
                     acfijo=as.integer(acfijo))

          }

        }
      }

      #12. empleo: ----

      # if(!is.na(cap_empleo[[1]])){
      #
      # if(!is.na(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo))){
      #
      # if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_empleo[[1]]){
      #
      #   message('empleo')
      #
      # }
      # }
      # }

      #13. bolsa de remuneracion: ----

      # if(!is.na(cap_salari[[1]])){
      #
      # if(unique(as.data.frame(raw_data[[i]][j])$CodCapitulo)==cap_salari[[1]]){
      #
      #   message('bolsa de remuneracion')
      #
      # }
      # }

    }
    }



  }


  }

  }
}

# 14.reuniendo datos----


datos_ventas1=bind_rows(datos_ventas)%>%
  mutate(iruc=as.character(iruc))%>%
  filter(ventas>0)

datos_insumos1=bind_rows(datos_insumos)%>%
  mutate(iruc=as.character(iruc))%>%
  filter(insumos>0)

datos_resultados1=bind_rows(datos_resultados)%>%
  mutate(iruc=as.character(iruc))

datos_acfijo1=bind_rows(datos_acfijos)%>%
  mutate(iruc=as.character(iruc))

datos_activos1=bind_rows(datos_activos)%>%
  mutate(iruc=as.character(iruc))

datos_impuesto1=bind_rows(datos_impuestos)%>%
  mutate(iruc=as.character(iruc))

datos_intangibles1=bind_rows(datos_intangibles)%>%
  mutate(iruc=as.character(iruc))

datos_publicidad1=bind_rows(datos_publicidad)%>%
  mutate(iruc=as.character(iruc))

#write_rds(datos_ventas1,'ventas_2011.rds')
#write_rds(datos_insumos1,'insumos_2011.rds')
#write_rds(datos_resultados1,'resultados_2011.rds')
#write_rds(datos_acfijo1,'activo_fijo_2011.rds')
#write_rds(datos_activos1,'activo_contable_2011.rds')
#write_rds(datos_impuesto1,'impuesto_2011.rds')
#write_rds(datos_intangibles1,'intangibles_2011.rds')
#write_rds(datos_publicidad1,'publicidad_2011.rds')

return(list(ventas=datos_ventas1,
            insumos=datos_insumos1,
            resultados=datos_resultados1,
            acfijo=datos_acfijo1,
            activo=datos_activos1,
            impuesto=datos_impuesto1,
            intang=datos_intangibles1,
            publicidad=datos_publicidad1))

}

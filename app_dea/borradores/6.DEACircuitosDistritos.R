# Filtrar PDET: Planes de desarrollad con Enfoque Territorial
# 170 mpios más 

#### PENAL, CIVIL Y FAMILIA
rm(list=ls())
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(rDEA)
library(knitr)
library(tibble)
library(stringr)
library(ggplot2)
library(ggrepel)
library(plotly)
#ruta<-'C:/Users/User/Documents/trabajo/csj'
ruta <- "C:/Users/Home/Documents/Laboral2020/Min Justicia/DEA/Version2"
setwd(ruta)
setwd('data')
bd_nueva<-readRDS('BD_procesos_Rama_Judicial_2017_2018.rds')
#### Filtros

bd_nueva <- bd_nueva[bd_nueva$DISTRITO !=  "Altas Cortes",] 

# 1 -> Penal
# 2 -> Familia
# 3 -> Civil

filtrado <- function(num_especialidad,tutelas){
  
  if (num_especialidad==1){
    especialidad<-'Penal'
  } else if (num_especialidad==2){
    especialidad <- 'Familia'
  } else if (num_especialidad==3){
    especialidad <- 'Civil'
  }
  
  if (tutelas==F){
  bd_filtrada <- bd_nueva %>%
    filter(AÑO==2018)%>%
    filter(ESPECIALIDAD==especialidad) %>%
    filter(str_detect(SECCIÓN, "Tutelas")==FALSE) %>%
    filter(str_detect(`Proceso - Resumen`, "Tutelas")==FALSE)  
  } else {
    bd_filtrada <- bd_nueva %>%
      filter(AÑO==2018)%>%
      filter(ESPECIALIDAD==especialidad)
  }
  return(bd_filtrada)
}

bd_penal_st<-filtrado(1,tutelas=F)
bd_penal_ct<-filtrado(1,tutelas = T)
bd_familia_st<-filtrado(2,tutelas=F)
bd_familia_ct<-filtrado(2,tutelas = T)
bd_civil_st<-filtrado(3,tutelas=F)
bd_civil_ct<-filtrado(3,tutelas = T)

# Función para crear el input y output
# 1 -> circuito
# 2 -> distrito

creacionInputsOutputs <- function(datos,circ_dist){
  if (circ_dist==1){
    datos <- datos %>%
      group_by(DISTRITO,CIRCUITO)%>%
      summarise(jueces=n_distinct(`Cédula del Funcionario despacho`),
                `TOTAL EGRESOS` = sum(`TOTAL EGRESOS`),
                `TOTAL INGRESOS` = sum(`TOTAL INGRESOS`),
                `INVENTARIO INICIAL CON TRÁMITE` = sum(`INVENTARIO INICIAL CON TRÁMITE`),
                `INVENTARIO FINAL CON TRÁMITE` = sum(`INVENTARIO FINAL CON TRÁMITE`)
      ) %>% ungroup() %>%
      mutate(Ing_Egre = `TOTAL EGRESOS` - `TOTAL INGRESOS`,
             inventInic_Final =   `INVENTARIO INICIAL CON TRÁMITE` - `INVENTARIO FINAL CON TRÁMITE`,
             Ing_Egre_posit = Ing_Egre+abs(min(Ing_Egre))+1,
             inventInic_Final_posit = inventInic_Final+abs(min(inventInic_Final))+1)
    return(datos)      
  } else {
    datos <- datos %>%
      group_by(DISTRITO)%>%
      summarise(jueces=n_distinct(`Cédula del Funcionario despacho`),
                `TOTAL EGRESOS` = sum(`TOTAL EGRESOS`),
                `TOTAL INGRESOS` = sum(`TOTAL INGRESOS`),
                `INVENTARIO INICIAL CON TRÁMITE` = sum(`INVENTARIO INICIAL CON TRÁMITE`),
                `INVENTARIO FINAL CON TRÁMITE` = sum(`INVENTARIO FINAL CON TRÁMITE`)
      ) %>% ungroup() %>%
      mutate(Ing_Egre = `TOTAL EGRESOS` - `TOTAL INGRESOS`,
             inventInic_Final =   `INVENTARIO INICIAL CON TRÁMITE` - `INVENTARIO FINAL CON TRÁMITE`,
             Ing_Egre_posit = Ing_Egre+abs(min(Ing_Egre))+1,
             inventInic_Final_posit = inventInic_Final+abs(min(inventInic_Final))+1)
    return(datos)    
  }
  
}

#### AGRUPACIÓN POR DISTRITOS

bd_penal_st_dist <- creacionInputsOutputs(bd_penal_st,circ_dist = 2) 
bd_penal_ct_dist <- creacionInputsOutputs(bd_penal_ct,circ_dist = 2) 
bd_familia_st_dist <- creacionInputsOutputs(bd_familia_st,circ_dist = 2) 
bd_familia_ct_dist <- creacionInputsOutputs(bd_familia_ct,circ_dist = 2) 
bd_civil_st_dist <- creacionInputsOutputs(bd_civil_st,circ_dist = 2) 
bd_civil_ct_dist <- creacionInputsOutputs(bd_civil_ct,circ_dist = 2)


#### AGRUPACIÓN POR CIRCUITOS

bd_penal_st_circ <- creacionInputsOutputs(bd_penal_st,circ_dist = 1) 
bd_penal_ct_circ <- creacionInputsOutputs(bd_penal_ct,circ_dist = 1) 
bd_familia_st_circ <- creacionInputsOutputs(bd_familia_st,circ_dist = 1) 
bd_familia_ct_circ <- creacionInputsOutputs(bd_familia_ct,circ_dist = 1) 
bd_civil_st_circ <- creacionInputsOutputs(bd_civil_st,circ_dist = 1) 
bd_civil_ct_circ <- creacionInputsOutputs(bd_civil_ct,circ_dist = 1)
modelo_DEA(bd_civil_ct_dist, tipo = "DISTRITO")

modelo_DEA <- function(datos, tipo ,variables_input = "jueces", 
                       variables_output = c('Ing_Egre_posit', 'inventInic_Final_posit')){
  tipo <-  rlang::sym(tipo)
  inp <- datos[variables_input]
  out <- datos[variables_output]
  modelo <- dea(XREF = inp,YREF = out, X=inp[,], Y=out[,],model='output',RTS = 'constant')
  datos$Eficiencia <- modelo$thetaOpt
  datos$Aumento_Recomendado<-((1-datos$Eficiencia) * 100)
  datos$cociente1 <- round(datos$inventInic_Final_posit/datos$jueces,1)  
  datos$cociente2 <- round(datos$Ing_Egre_posit/datos$jueces,1)
  
  grafico <- ggplot(data=datos, aes(x = cociente1, y = cociente2, 
                           color = Eficiencia, label = !!tipo)) +
             geom_point(aes(color=Eficiencia)) +
             theme(legend.position = "none", rect=element_rect(fill = "transparent"),
                   plot.title = element_text(hjust = 0.5),
                   panel.grid=element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black")) +
            ggtitle(paste0("Eficiencias ", tolower(rlang::quo_text(tipo)))) +
            labs(x="cociente1 = (Inventario inicial - final)/jueces",
                 y = "cociente2 = (Ingreso - Egresos)/jueces",
                 color = "EFICIENCIA") +
            geom_text_repel(aes(label = paste0(!!tipo, "(",
                            round(100 * Eficiencia, 1), "%)"),
                            color = Eficiencia),force=8,
                    arrow = arrow(length = unit(0.5, 'picas')), size=2.5) +
            scale_color_gradient(low = "springgreen4", high = "red")
  
  resultado <- list(datos, grafico)
  return(resultado)
  }





grafico_frontera <- function(datos,result,circuitos=F){
  if (circuitos==F){
  #tablaf1 <- data.frame(datos,eficiencia=result$Eficiencia)
  
  result$cociente1 <- round(result$inventInic_Final_posit/result$Jueces,1)  
  result$cociente2 <- round(result$Ing_Egre_posit/result$Jueces,1)
  fronteraJC <- ggplot(data=result,
                       aes(x=cociente1,
                           y=cociente2, 
                           color = Eficiencia,
                           label = circuito)) +
    geom_point(aes(color=Eficiencia)) +
    theme(legend.position = "none",
          rect=element_rect(fill = "transparent"),
          plot.title = element_text(hjust = 0.5),
          # para fondo blanco
          panel.grid=element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
          ggtitle("Eficiencias distritos") +
    labs(x="cociente1 = (Inventario inicial - final)/jueces",
         y="cociente2 = (Ingreso - Egresos)/jueces",
         color = "EFICIENCIA") +
    geom_text_repel(aes(label=circuito,
                        color=Eficiencia),force=8,
                        arrow = arrow(length = unit(0.5, 'picas')), 
                        size=2.5) +
     scale_color_gradient(low = "springgreen4", high = "red")
 
  
  
   return(fronteraJC)}
  else {
    tablalabelf1 <- data.frame(datos,eficiencia=result$Eficiencia)
    tablalabelf1$cociente1 <- round(tablalabelf1$inventInic_Final_posit/tablalabelf1$jueces,1)  
    tablalabelf1$cociente2 <- round(tablalabelf1$Ing_Egre_posit/tablalabelf1$jueces,1)
    
    fronteraJC <- ggplot(data=tablalabelf1,
                         aes(x=cociente1,
                             y=cociente2,color = eficiencia,
                             label=DISTRITO)
    )+
      geom_point(aes(color=eficiencia)
      )+
      theme(legend.position = "none",
            rect=element_rect(fill = "transparent"),
            plot.title = element_text(hjust = 0.5),
            # para fondo blanco
            panel.grid=element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")
      )+
      
      ggtitle("Eficiencias distritos"
      )+
      labs(x="cociente1 = (Inventario inicial - final)/jueces",
           y="cociente2 = (Ingreso - Egresos)/jueces",
           color = "EFICIENCIA"
      )+
      geom_text_repel(aes(label=CIRCUITO,
                          color=eficiencia),force=8,
                      arrow = arrow(length = unit(0.5, 'picas')),size=4) +
      scale_color_gradient(low = "springgreen4", high = "red")
    return(ggplotly(fronteraJC))  
  }
}

#st: sin tutelas
# ct: con tutelas
result_penal_st_dist <- modelo_DEA(bd_penal_st_dist,circuitos = F,
                                   imprimir_lambdas = F)
grafico_penal_st_dist <- grafico_frontera(bd_penal_st_dist,
                                          result_penal_st_dist,
                                          circuitos = F)
#plotly::ggplotly(grafico_penal_st_dist)


result_penal_ct_dist <- modelo_DEA(bd_penal_ct_dist,circuitos = F,
                                   imprimir_lambdas = F)
grafico_penal_ct_dist <- grafico_frontera(bd_penal_ct_dist,
                                          result_penal_ct_dist,
                                          circuitos = F)
result_familia_st_dist <- modelo_DEA(bd_familia_st_dist,circuitos = F,
                                     imprimir_lambdas = F)
grafico_familia_st_dist <- grafico_frontera(bd_familia_st_dist,
                                          result_familia_st_dist,
                                          circuitos = F)

result_familia_ct_dist <- modelo_DEA(bd_familia_ct_dist,circuitos = F,
                                     imprimir_lambdas = F)
grafico_familia_ct_dist <- grafico_frontera(bd_familia_ct_dist,
                                          result_familia_ct_dist,
                                          circuitos = F)
result_civil_st_dist <- modelo_DEA(bd_civil_st_dist,circuitos = F,
                                   imprimir_lambdas = F)
grafico_civil_st_dist <- grafico_frontera(bd_civil_st_dist,
                                          result_civil_st_dist,
                                          circuitos = F)
result_civil_ct_dist <- modelo_DEA(bd_civil_ct_dist,circuitos = F,
                                   imprimir_lambdas = F)
grafico_civil_ct_dist <- grafico_frontera(bd_civil_ct_dist,
                                          result_civil_ct_dist,
                                          circuitos = F)


result_penal_st_circ <- modelo_DEA(bd_penal_st_circ,circuitos = T,
                                   imprimir_lambdas = F)
grafico_penal_st_circ <- grafico_frontera(bd_penal_st_circ,
                                          result_penal_st_circ,
                                          circuitos = T)


result_penal_ct_circ <- modelo_DEA(bd_penal_ct_circ,circuitos = T,
                                   imprimir_lambdas = F)
grafico_penal_ct_circ <- grafico_frontera(bd_penal_ct_circ,
                                          result_penal_ct_circ,
                                          circuitos = T)
result_familia_st_circ <- modelo_DEA(bd_familia_st_circ,circuitos = T,
                                     imprimir_lambdas = F)
grafico_familia_st_circ <- grafico_frontera(bd_familia_st_circ,
                                            result_familia_st_circ,
                                            circuitos = T)
result_familia_ct_circ <- modelo_DEA(bd_familia_ct_circ,circuitos = T,
                                     imprimir_lambdas = F)
grafico_familia_ct_circ <- grafico_frontera(bd_familia_ct_circ,
                                            result_familia_ct_circ,
                                            circuitos = T)
result_civil_st_circ <- modelo_DEA(bd_civil_st_circ,circuitos = T,
                                   imprimir_lambdas = F)
grafico_civil_st_circ <- grafico_frontera(bd_civil_st_circ,
                                          result_civil_st_circ,
                                          circuitos = T)
result_civil_ct_circ <- modelo_DEA(bd_civil_ct_circ,circuitos = T,
                                   imprimir_lambdas = F)
grafico_civil_ct_circ <- grafico_frontera(bd_civil_ct_circ,
                                          result_civil_ct_circ,
                                          circuitos = T)

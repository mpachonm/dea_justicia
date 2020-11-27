# Filtrar PDET: Planes de desarrollad con Enfoque Territorial
# 170 mpios más 

#### PENAL, CIVIL Y FAMILIA
#rm(list=ls())
library(DT)
library(readr)
library(dplyr)
library(stringr)
library(rDEA)
library(knitr)
library(tibble)
library(stringr)
library(ggplot2)
library(readxl)
library(ggrepel)
library(plotly)
library(rlang)
library(skimr)

ruta<-'C:/Users/User/Documents/trabajo/Version2'
#ruta <- "C:/Users/Home/Documents/Laboral2020/Min Justicia/DEA/Version2"
setwd(ruta)
setwd('data')
bd_nueva <- readRDS('BD_procesos_Rama_Judicial_2017_2018.rds')
bd_nueva$COD_DPTO <- substr(bd_nueva$CÓDIGO, 1, 2)
bd_nueva$COD_MUNICIPIO <- substr(bd_nueva$CÓDIGO, 1, 5)

# Pegr municipios PDET
dir()
df_pdet <- read_excel("Municipios_PDET.xlsx")
df_pdet <- df_pdet[c("CodigoDepartamento", "CodigoMunicipio")]  
names(df_pdet) <- c("COD_DPTO", "COD_MUNICIPIO")
df_pdet$pdet <- 1
df_pdet$COD_DPTO <- stringr::str_pad(string = df_pdet$COD_DPTO, width = 2,
                                     side = "left", pad = "0")
df_pdet$COD_MUNICIPIO <- stringr::str_pad(string = df_pdet$COD_MUNICIPIO, width = 3,
                                          side = "left", pad = "0")

bd_nueva <- left_join(bd_nueva, df_pdet, by = c("COD_MUNICIPIO", "COD_DPTO"))
bd_nueva$pdet <- ifelse(is.na(bd_nueva$pdet), 0, 1)

# length(unique(bd_nueva$COD_MUNICIPIO[bd_nueva$pdet == 1]))
# length(unique(df_pdet$COD_MUNICIPIO))

#### Filtros
bd_nueva <- bd_nueva[bd_nueva$DISTRITO !=  "Altas Cortes",] 

# 1 -> Penal
# 2 -> Familia
# 3 -> Civil
# 4 -> Administrativa
# 5 -> Laboral
# 6 -> Disciplinaria

filtrado <- function(num_especialidad,tutelas){
  
  if (num_especialidad==1){
    especialidad<-'Penal'
  } else if (num_especialidad==2){
    especialidad <- 'Familia'
  } else if (num_especialidad==3){
    especialidad <- 'Civil'
  } else if (num_especialidad==4){
    especialidad <- 'Administrativa'
  } else if (num_especialidad==5){
    especialidad <- 'Laboral'
  } else if (num_especialidad==6){
    especialidad <- 'Disciplinaria'
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
bd_admin_st<-filtrado(4,tutelas=F)
bd_admin_ct<-filtrado(4,tutelas = T)
bd_laboral_st<-filtrado(5,tutelas=F)
bd_laboral_ct<-filtrado(5,tutelas = T)
bd_discip_st<-filtrado(6,tutelas=F)
bd_discip_ct<-filtrado(6,tutelas = T)

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
                `INVENTARIO FINAL CON TRÁMITE` = sum(`INVENTARIO FINAL CON TRÁMITE`),
                pdet = max(pdet),
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
                `INVENTARIO FINAL CON TRÁMITE` = sum(`INVENTARIO FINAL CON TRÁMITE`),
                pdet = max(pdet)
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
bd_admin_st_dist <- creacionInputsOutputs(bd_admin_st,circ_dist = 2) 
bd_admin_ct_dist <- creacionInputsOutputs(bd_admin_ct,circ_dist = 2) 
bd_laboral_st_dist <- creacionInputsOutputs(bd_laboral_st,circ_dist = 2) 
bd_laboral_ct_dist <- creacionInputsOutputs(bd_laboral_ct,circ_dist = 2) 
bd_discip_st_dist <- creacionInputsOutputs(bd_discip_st,circ_dist = 2) 
bd_discip_ct_dist <- creacionInputsOutputs(bd_discip_ct,circ_dist = 2)

#### AGRUPACIÓN POR CIRCUITOS

bd_penal_st_circ <- creacionInputsOutputs(bd_penal_st,circ_dist = 1) 
bd_penal_ct_circ <- creacionInputsOutputs(bd_penal_ct,circ_dist = 1) 
bd_familia_st_circ <- creacionInputsOutputs(bd_familia_st,circ_dist = 1) 
bd_familia_ct_circ <- creacionInputsOutputs(bd_familia_ct,circ_dist = 1) 
bd_civil_st_circ <- creacionInputsOutputs(bd_civil_st,circ_dist = 1) 
bd_civil_ct_circ <- creacionInputsOutputs(bd_civil_ct,circ_dist = 1)
bd_admin_st_circ <- creacionInputsOutputs(bd_admin_st,circ_dist = 1) 
bd_admin_ct_circ <- creacionInputsOutputs(bd_admin_ct,circ_dist = 1) 
bd_laboral_st_circ <- creacionInputsOutputs(bd_laboral_st,circ_dist = 1) 
bd_laboral_ct_circ <- creacionInputsOutputs(bd_laboral_ct,circ_dist = 1) 
bd_discip_st_circ <- creacionInputsOutputs(bd_discip_st,circ_dist = 1) 
bd_discip_ct_circ <- creacionInputsOutputs(bd_discip_ct,circ_dist = 1)

##### PROMISCUOS

promis<-bd_nueva%>%
  filter(ESPECIALIDAD=='Promiscuo')

seccion_otros<-c("Movimiento de Impugnaciones",  
                 "Primera y única instancia", "Primera y única Instancia", 
                 "Segunda Instancia Sala única")
seccion_tutelas<-c("Movimiento de Tutelas")
seccion_penal<-c("Control de Garantías - Ley 1098", "Control de Garantías - Ley 906",
                 "Primera Instancia Conocimiento - Ley 906","Segunda Instancia Penal",
                 "Segunda Instancia Penal - Ley 600","Primera y única instancia Penal",
                 "Primera y única Instancia Penal","Segunda instancia - Ley 906 - Control de garantías y conocimiento", 
                 "Segunda Instancia - Ley 906 - Control de garantías y conocimiento")
seccion_civil<-c("Primera y única instancia Civil", "Primera y única Instancia Civil", 
                 "Primera y única instancia Civil - Oral", "Primera y única Instancia Civil - Oral", 
                 "Primera y única Instancia Civil - Oral", "Primera y única instancia Civil Familia", 
                 "Primera y única Instancia Civil Familia", "Primera y única instancia Civil Pequeñas Causas", 
                 "Primera y única Instancia Civil Pequeñas Causas", "Primera y única instancia Civil Pequeñas Causas - Oral", 
                 "Primera y única Instancia Civil Pequeñas Causas - Oral","Segunda Instancia Civil", "Segunda Instancia Civil - Oral", 
                 "Segunda Instancia Civil Familia", "Segunda Instancia Civil Familia - Oral", 
                 "Segunda Instancia Civil Familia Laboral", "Segunda Instancia Civil Familia Laboral - Oral")
seccion_familia<-c("Segunda Instancia  Familia", "Segunda Instancia  Familia - Oral", 
                   "Segunda Instancia Familia", "Segunda Instancia Familia - Oral", 
                   "Segunda Instancia Laboral", "Segunda Instancia Laboral - Oral")

filtrado_prom <- function(num_seccion,tutelas){
  
  if (num_seccion==1){
    seccion<-seccion_penal
  } else if (num_seccion==2){
    seccion <- seccion_familia
  } else if (num_seccion==3){
    seccion <- seccion_civil
  }
  
  if (tutelas==F){
    bd_filtrada <- promis %>%
      filter(AÑO==2018)%>%
      filter(SECCIÓN %in% seccion) %>%
      filter(str_detect(SECCIÓN, "Tutelas")==FALSE) %>%
      filter(str_detect(`Proceso - Resumen`, "Tutelas")==FALSE)  
  } else {
    bd_filtrada <- promis %>%
      filter(AÑO==2018)%>%
      filter(SECCIÓN %in% seccion)
  }
  return(bd_filtrada)
}
bd_promis_penal<-filtrado_prom(1,tutelas = T)
bd_promis_familia<-filtrado_prom(2,tutelas = T)
bd_promis_civil<-filtrado_prom(3,tutelas = T)
bd_promis_tutelas <- promis %>%
  filter(AÑO==2018)%>%
  filter(SECCIÓN %in% c("Movimiento de Tutelas"))
#### AGRUPACIÓN POR CIRCUITOS


bd_promis_penal_circ <- creacionInputsOutputs(bd_promis_penal,circ_dist = 1) 
bd_promis_familia_circ <- creacionInputsOutputs(bd_promis_familia,circ_dist = 1) 
bd_promis_civil_circ <- creacionInputsOutputs(bd_promis_civil,circ_dist = 1)
bd_promis_tutelas_circ<-creacionInputsOutputs(bd_promis_tutelas,circ_dist = 1)

#### AGRUPACIÓN POR DISTRITOS


bd_promis_penal_dist <- creacionInputsOutputs(bd_promis_penal,circ_dist = 2) 
bd_promis_familia_dist <- creacionInputsOutputs(bd_promis_familia,circ_dist = 2) 
bd_promis_civil_dist <- creacionInputsOutputs(bd_promis_civil,circ_dist = 2)
bd_promis_tutelas_dist<-creacionInputsOutputs(bd_promis_tutelas,circ_dist = 2)

setwd(ruta)
setwd("app_dea")
datos_especialidad <- save(bd_penal_st_dist, bd_penal_ct_dist, bd_familia_st_dist,
                           bd_familia_ct_dist,bd_civil_st_dist, bd_civil_ct_dist,
  bd_penal_st_circ, bd_penal_ct_circ, bd_familia_st_circ,
                           bd_familia_ct_circ, bd_civil_st_circ,  bd_civil_ct_circ,
  bd_promis_penal_circ,bd_promis_familia_circ,bd_promis_civil_circ,bd_promis_tutelas_circ,
  bd_promis_penal_dist,bd_promis_familia_dist,bd_promis_civil_dist,bd_promis_tutelas_dist,
  bd_admin_st_dist,bd_admin_ct_dist,bd_laboral_st_dist,bd_laboral_ct_dist,bd_discip_st_dist,
  bd_discip_ct_dist,bd_admin_st_circ,bd_admin_ct_circ,bd_laboral_st_circ,bd_laboral_ct_circ,
  bd_discip_st_circ,bd_discip_ct_circ,
                           file = "datos_especialidad.Rdata" 
                            )

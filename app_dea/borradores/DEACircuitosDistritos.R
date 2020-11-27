# Filtrar PDET: Planes de desarrollad con Enfoque Territorial
# 170 mpios más 

#### PENAL, CIVIL Y FAMILIA
rm(list=ls())
library(readxl)
library(readr)
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


#### AGRUPACIÓN POR CIRCUITOS

bd_penal_st_circ <- creacionInputsOutputs(bd_penal_st,circ_dist = 1) 
bd_penal_ct_circ <- creacionInputsOutputs(bd_penal_ct,circ_dist = 1) 
bd_familia_st_circ <- creacionInputsOutputs(bd_familia_st,circ_dist = 1) 
bd_familia_ct_circ <- creacionInputsOutputs(bd_familia_ct,circ_dist = 1) 
bd_civil_st_circ <- creacionInputsOutputs(bd_civil_st,circ_dist = 1) 
bd_civil_ct_circ <- creacionInputsOutputs(bd_civil_ct,circ_dist = 1)

# prueba <- modelo_DEA(bd_civil_ct_dist, jerarquia = "DISTRITO")
#prueba <- modelo_DEA(bd_civil_ct_dist, jerarquia = "DISTRITO", PDET = T)

modelo_DEA <- function(datos, jerarquia ,variables_input = "jueces", PDET = F,
                       variables_output = c('Ing_Egre_posit', 'inventInic_Final_posit')){
  jerarquia <-  rlang::sym(jerarquia)
  inp <- datos[variables_input]
  out <- datos[variables_output]
  modelo <- dea(XREF = inp,YREF = out, X=inp[,], Y=out[,],model='output',
                RTS = 'constant')
  datos$Eficiencia <- modelo$thetaOpt
  datos$Aumento_Recomendado<-((1-datos$Eficiencia) * 100)
  datos$cociente1 <- round(datos$inventInic_Final_posit/datos$jueces,1)  
  datos$cociente2 <- round(datos$Ing_Egre_posit/datos$jueces,1)
  
  grafico <- ggplot(data=datos, aes(x = cociente1, y = cociente2, 
                           color = Eficiencia, label = !!jerarquia)) +
             geom_point(aes(color=Eficiencia)) +
             theme(legend.position = "none", 
                   rect=element_rect(fill = "transparent"),
                   plot.title = element_text(hjust = 0.5),
                   panel.grid=element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black")) +
            ggtitle(paste0("Eficiencias ", tolower(rlang::quo_text(jerarquia)))) +
            labs(x="cociente1 = (Inventario inicial - final)/jueces",
                 y = "cociente2 = (Ingreso - Egresos)/jueces",
                 color = "EFICIENCIA") +
            geom_text_repel(aes(label = paste0(!!jerarquia, "(",
                            round(100 * Eficiencia, 1), "%)"),
                            color = Eficiencia),force=8,
                    arrow = arrow(length = unit(0.5, 'picas')), size=2.5) +
            scale_color_gradient(low = "springgreen4", high = "red")
  
 
  if(PDET == T){
    
    grafico <- ggplot(data=datos, aes(x = cociente1, y = cociente2, 
                                      color = pdet, label = !!jerarquia)) +
      geom_point(aes(color=pdet)) +
      theme(legend.position = "none", rect=element_rect(fill = "transparent"),
            plot.title = element_text(hjust = 0.5),
            panel.grid=element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      ggtitle(paste0("Eficiencias ", tolower(rlang::quo_text(jerarquia)))) +
      labs(x="cociente1 = (Inventario inicial - final)/jueces",
           y = "cociente2 = (Ingreso - Egresos)/jueces",
           color = "PDET") +
      geom_text_repel(aes(label = paste0(!!jerarquia, "(",
                                         round(100 * Eficiencia, 1), "%)", "jueces:", jueces),
                          color = pdet),force=8,
                      arrow = arrow(length = unit(0.5, 'picas')), size=2.5) +
      scale_color_gradient(low = "springgreen4", high = "red")
    
  }
  
   resultado <- list(datos, grafico)
   names(resultado) <- c("df_resutlado", "grafico")
  return(resultado)
  }


# Tablas a analizar
vctr_jerarquia <- c("Distrito", "circuito")
vctr_especialidad <- c("Civil", "Familia", "Penal")
vctr_conTutelas <- c("Sí", "No")
df_opciones <- expand.grid(vctr_jerarquia, vctr_especialidad, vctr_conTutelas, 
                           stringsAsFactors = F)
names(df_opciones) <- c("jerarquia", "especialidad", "tutelas")
df_opciones <- arrange(df_opciones, desc(jerarquia), especialidad, desc(tutelas))
df_opciones$dataframe <- c("bd_civil_ct_dist", "bd_civil_st_dist",
                            "bd_familia_ct_dist", "bd_familia_st_dist",
                            "bd_penal_ct_dist", "bd_penal_st_dist",
                            "bd_civil_ct_circ", "bd_civil_st_circ",
                            "bd_familia_ct_circ", "bd_familia_st_circ",
                            "bd_penal_ct_circ", "bd_penal_st_circ")   

modelo_DEA(datos = get("bd_civil_ct_dist"), jerarquia = "DISTRITO")  



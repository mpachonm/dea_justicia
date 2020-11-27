library(DT)
library(readr)
library(dplyr)
library(stringr)
library(rDEA)
library(tibble)
library(stringr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(rlang)
library(skimr)
setwd('C:/Users/User/Documents/trabajo/Version2/app_dea')
#setwd("C:/Users/Home/Documents/Laboral2020/Min Justicia/DEA/Version2/app_dea")
load("datos_especialidad.Rdata")
# prueba <- modelo_DEA(bd_civil_ct_dist, jerarquia = "DISTRITO")
#prueba <- modelo_DEA(bd_civil_ct_dist, jerarquia = "DISTRITO", PDET = T)
modelo_DEA <- function(datos, jerarquia, especialidad, tutelas, variables_input = "jueces", PDET = F,
                       variables_output = c('Ing_Egre_posit', 'inventInic_Final_posit')){
    jerarquia <-  rlang::sym(toupper(jerarquia))
    indica_tutelas <- ifelse(tutelas == "Sí", "con tutelas", ifelse(tutelas == "No",
                                                                    "sin tutelas", NA)) 
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
        datos$Eficiencia <-  round(100 * datos$Eficiencia, 1)
        
        datos$detalle <- paste0(datos[[rlang::quo_text(jerarquia)]], ":", datos$Eficiencia,
                                "/n  jueces:", datos$jueces)
        grafico <- ggplot(data=datos, aes(x = cociente1, y = cociente2, 
                                          color = pdet, label = detalle )) +
            geom_point() +
            theme(legend.position = "none", rect=element_rect(fill = "transparent"),
                  plot.title = element_text(hjust = 0.5),
                  panel.grid=element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black")) +
            ggtitle(paste0("Eficiencias ", tolower(rlang::quo_text(jerarquia)), 
                           " (", especialidad, " ", indica_tutelas ,")")) +
            labs(x="cociente1 = (Inventario inicial - final)/jueces",
                 y = "cociente2 = (Ingreso - Egresos)/jueces",
                 color = "PDET") +
            geom_text_repel(aes(label = paste0(!!jerarquia, "(",
                                               round(100 * Eficiencia, 1), "%)"),
                                color = pdet),force=8,
                            arrow = arrow(length = unit(0.5, 'picas')), size=2.5) +
            scale_color_gradient(low = "springgreen4", high = "red")
        
    }
    
    resultado <- list(datos, grafico)
    names(resultado) <- c("df_resutlado", "grafico")
    return(resultado)
}


# Tablas a analizar
vctr_jerarquia <- c("Distrito", "Circuito")
vctr_especialidad <- c("Civil", "Familia", "Penal",'Administrativa'
                       ,'Laboral','Disciplinaria','Promiscuo')
vctr_conTutelas <- c("Sí", "No")
vctr_especialidad_promiscuo<-c('Civil','Familia','Penal','Tutelas')
df_opciones <- expand.grid(vctr_jerarquia, vctr_especialidad,vctr_conTutelas, 
                           stringsAsFactors = F)
names(df_opciones) <- c("jerarquia", "especialidad","tutelas")
df_opciones <- arrange(df_opciones, desc(jerarquia), especialidad, desc(tutelas))
df_opciones<-df_opciones%>%
    filter(especialidad!='Promiscuo')
df_opciones$dataframe <- c('bd_admin_ct_dist','bd_admin_st_dist',
                           "bd_civil_ct_dist", "bd_civil_st_dist",
                           'bd_discip_ct_dist','bd_discip_st_dist',
                           "bd_familia_ct_dist", "bd_familia_st_dist",
                           'bd_laboral_ct_dist','bd_laboral_st_dist',
                           "bd_penal_ct_dist", "bd_penal_st_dist",
                           'bd_admin_ct_circ','bd_admin_st_circ',
                           "bd_civil_ct_circ", "bd_civil_st_circ",
                           'bd_discip_ct_circ','bd_discip_st_circ',
                           "bd_familia_ct_circ", "bd_familia_st_circ",
                           'bd_laboral_ct_circ','bd_laboral_st_circ',
                           "bd_penal_ct_circ", "bd_penal_st_circ")
                          
df_opciones_promiscuo <- expand.grid(vctr_jerarquia, vctr_especialidad_promiscuo,
                                     stringsAsFactors = F)
names(df_opciones_promiscuo) <- c("jerarquia", "especialidad")
df_opciones_promiscuo <- arrange(df_opciones_promiscuo, desc(jerarquia), especialidad)

df_opciones_promiscuo$dataframe <- c('bd_promis_civil_dist','bd_promis_familia_dist',
                           'bd_promis_penal_dist','bd_promis_tutelas_dist',
                           'bd_promis_penal_circ','bd_promis_penal_circ',
                           'bd_promis_tutelas_circ','bd_promis_tutelas_circ')   

library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel(title=div(img(src="logo.png"), "Benchmarking distritos y circuitos")),
    
    sidebarLayout(
        sidebarPanel(width = 3,
                        selectInput(inputId = "seleccion_jerarquia", 
                        label = "Seleccione la jerarquía para el análisis:",
                        choices =  vctr_jerarquia,
                        selected = vctr_jerarquia[1]),
            selectInput(inputId = "seleccion_especialidad", 
                        label = "Seleccione la especialidad:",
                        choices = vctr_especialidad,
                        selected = vctr_especialidad[1]),
            conditionalPanel(
                condition = "input.seleccion_especialidad == 'Promiscuo'",
                selectInput(inputId = "seleccion_especialidad_promiscuo", 
                            label = "Seleccione la especialidad:",
                            choices = vctr_especialidad_promiscuo,
                            selected = vctr_especialidad_promiscuo[1])
                ),
            conditionalPanel(
                    condition = "input.seleccion_especialidad != 'Promiscuo'",
                    radioButtons(inputId = "seleccion_tutelas",label = "Marque si desea ver el analisis incluyendo tutelas:",
                     choices = vctr_conTutelas,selected = vctr_conTutelas[1]
                    )
                    )
            ),
            uiOutput(outputId =  "numero_jueces")
                ),
        # Show a plot of the generated distribution
        mainPanel(
                   tabsetPanel(type = "tabs",
                         tabPanel("Visualización", plotlyOutput("grafico2")),
                         tabPanel(width = 12,"Resumen", verbatimTextOutput("resumen")),
                         tabPanel("Tabla", DT::dataTableOutput("tabla"))
             )
           
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    numero_jueces <- reactive({if(input$seleccion_especialidad !='Promiscuo'){
        string_df <- (df_opciones %>% dplyr::filter(jerarquia == input$seleccion_jerarquia & 
                                                 especialidad == input$seleccion_especialidad &
                                                 tutelas ==  input$seleccion_tutelas))$dataframe
        num_jueces <- get(string_df)$jueces
        num_jueces}
        else {string_df <- (df_opciones_promiscuo %>% dplyr::filter(jerarquia == input$seleccion_jerarquia & 
                                                              especialidad == input$seleccion_especialidad_promiscuo))$dataframe
        num_jueces <- get(string_df)$jueces
        num_jueces
            
        }
    })
    
    
    output$numero_jueces <- renderUI({
    sliderInput("variable_jueces", "Número mínimo de jueces:",
                 min = min(numero_jueces()),
                max = median(numero_jueces()),
                value = min(numero_jueces()) )
    })
   
        output$grafico2 <- renderPlotly({if(input$seleccion_especialidad !='Promiscuo'){
            
            string_df <- (df_opciones %>% dplyr::filter(jerarquia == input$seleccion_jerarquia & 
                                                            especialidad == input$seleccion_especialidad &
                                                            tutelas ==  input$seleccion_tutelas))$dataframe
            datos_analisis <- get(string_df)
            datos_analisis <- dplyr::filter(datos_analisis, jueces >= input$variable_jueces)
            modelo_DEA(datos = datos_analisis, jerarquia = input$seleccion_jerarquia,
                       especialidad = input$seleccion_especialidad,
                       tutelas = input$seleccion_tutelas, PDET = T)$grafico}
            else {string_df <- (df_opciones_promiscuo %>% dplyr::filter(jerarquia == input$seleccion_jerarquia & 
                                                                  especialidad == input$seleccion_especialidad_promiscuo))$dataframe
            datos_analisis <- get(string_df)
            datos_analisis <- dplyr::filter(datos_analisis, jueces >= input$variable_jueces)
            modelo_DEA(datos = datos_analisis, jerarquia = input$seleccion_jerarquia,
                       especialidad = input$seleccion_especialidad,
                       tutelas = 'Sí', PDET = T)$grafico
                
            }
        })
    
        output$tabla <- DT::renderDataTable({
            if(input$seleccion_especialidad !='Promiscuo'){
            string_df <- (df_opciones %>% dplyr::filter(jerarquia == input$seleccion_jerarquia & 
                                                            especialidad == input$seleccion_especialidad &
                                                            tutelas ==  input$seleccion_tutelas))$dataframe
            datos_analisis <- get(string_df)
            datos_analisis <- dplyr::filter(datos_analisis, jueces >= input$variable_jueces)
            salida <- modelo_DEA(datos = datos_analisis, jerarquia = input$seleccion_jerarquia,
                       especialidad = input$seleccion_especialidad,
                       tutelas = input$seleccion_tutelas, PDET = T)[[1]]
            salida$Ing_Egre_posit <- NULL
            salida$inventInic_Final_posit <- NULL
            salida$detalle <- NULL
            salida <- salida %>% relocate(Eficiencia, .before = jueces)
            names(salida) <- toupper(names(salida)) 
            salida}
            else {string_df <- (df_opciones_promiscuo %>% dplyr::filter(jerarquia == input$seleccion_jerarquia & 
                                                                  especialidad == input$seleccion_especialidad_promiscuo))$dataframe
            datos_analisis <- get(string_df)
            datos_analisis <- dplyr::filter(datos_analisis, jueces >= input$variable_jueces)
            salida <- modelo_DEA(datos = datos_analisis, jerarquia = input$seleccion_jerarquia,
                                 especialidad = input$seleccion_especialidad, 
                                 tutelas = 'Sí',PDET = T)[[1]]
            salida$Ing_Egre_posit <- NULL
            salida$inventInic_Final_posit <- NULL
            salida$detalle <- NULL
            salida <- salida %>% relocate(Eficiencia, .before = jueces)
            names(salida) <- toupper(names(salida)) 
            salida
                
            }
        }
        )
        
        output$resumen <- shiny::renderPrint({
            if(input$seleccion_especialidad !='Promiscuo'){
            string_df <- (df_opciones %>% dplyr::filter(jerarquia == input$seleccion_jerarquia & 
                                                            especialidad == input$seleccion_especialidad &
                                                            tutelas ==  input$seleccion_tutelas))$dataframe
            datos_analisis <- get(string_df)
            datos_analisis <- dplyr::filter(datos_analisis, jueces >= input$variable_jueces)
            salida <- modelo_DEA(datos = datos_analisis, jerarquia = input$seleccion_jerarquia,
                                 especialidad = input$seleccion_especialidad,
                                 tutelas = input$seleccion_tutelas, PDET = T)[[1]]
            salida$Ing_Egre_posit <- NULL
            salida$inventInic_Final_posit <- NULL
            salida$detalle <- NULL
            salida <- salida %>% relocate(Eficiencia, .before = jueces)
            names(salida) <- toupper(names(salida)) 
            skimr::skim_without_charts((as.data.frame(salida)))}
            else {string_df <- (df_opciones %>% dplyr::filter(jerarquia == input$seleccion_jerarquia & 
                                                                  especialidad == input$seleccion_especialidad_promiscuo))$dataframe
            datos_analisis <- get(string_df)
            datos_analisis <- dplyr::filter(datos_analisis, jueces >= input$variable_jueces)
            salida <- modelo_DEA(datos = datos_analisis, jerarquia = input$seleccion_jerarquia,
                                 especialidad = input$seleccion_especialidad_promiscuo,
                                 tutelas = 'Sí',PDET = T)[[1]]
            salida$Ing_Egre_posit <- NULL
            salida$inventInic_Final_posit <- NULL
            salida$detalle <- NULL
            salida <- salida %>% relocate(Eficiencia, .before = jueces)
            names(salida) <- toupper(names(salida)) 
            skimr::skim_without_charts((as.data.frame(salida)))
                
            }
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)

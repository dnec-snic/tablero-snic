library(shiny)
library(leaflet)
library(writexl)
library(haven)
library(sf)
library(rgdal)
library(sp)
library(htmlwidgets)
library(dplyr)
library(RColorBrewer)
library(leaflet.extras)
library(tidyr)
library(gt)
library(ggplot2)
library(gridExtra)
library(gghighlight)
library(shinyWidgets)
library(shinydashboard)
library(plotly)
library(base64enc)

valid_codes <- c("1","2","3","4","5","6","7","8","9","10","11","14_1","14_2","14_3","31")
valid_codes2 <- c("12","13","15","16","17","18","19","20","21","22_1","22_2","22_3","22_4","22_5","22_6","22_7","23","24","25","26","27","28_1","28_2","28_3","28_4","28_5","28_6","28_7","28_8","28_9","28_10","29_1","29_2","29_3","29_4","29_5","29_6","29_7","29_8","30","32")
codificacion_snic<- readxl::read_excel("NN-DATOS/codificacion_snic.xlsx")

datos_nacionales <-readxl::read_excel("NN-DATOS/DATOS_NACIONALES_2.xlsx")
datos_nacionales<- datos_nacionales %>%
  filter(anio >= 2017 & anio <= 2022)
datos_provinciales <-readxl::read_excel("NN-DATOS/DATOS_PROVINCIALES_2.xlsx")
datos_provinciales <- datos_provinciales %>%
  mutate(provincia_nombre = ifelse(provincia_nombre== "Tierra del Fuego, Antártida e Islas del Atlántico Sur", "Tierra del Fuego", provincia_nombre))
datos_departamentales <-readxl::read_excel("NN-DATOS/DATOS_DEPARTAMENTALES_2.xlsx")

geo_provincias <- read_sf("NN-DATOS/SHP Argentina Prov-Dpto/Provincias.shp")
geo_departamentos <- read_sf("NN-DATOS/SHP Argentina Prov-Dpto/Dpto segun SNIC.shp")

ruta_imagen <- "NN-DATOS/esquema-metodología.png"
imagen_base64 <- base64encode(ruta_imagen)

opciones <- sort(unique(geo_provincias$provincia))
opciones_filtradas <- opciones[opciones != "Otro"]


ui <- fluidPage(
  column(width = 12,
         fluidRow(
           style = "background-color:#47baec; color: #333535; padding: 2px; font-size: 22px; font-weight: bold; text-align: left; font-family: Trebuchet MS;",
           column(width = 12, style = "margin-top: 8px;",
                  "Sistema Nacional de Información Criminal",
                  tags$a(href = "https://www.argentina.gob.ar/seguridad/estadisticascriminales", target = "_blank",
                         tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/57/Minseguridadarg.png/250px-Minseguridadarg.png", width = "138px", height = "48px", style = "float:right; margin-top: -0.15cm;")
                  )
           )
         )
  ),
  tabsetPanel(
    # Primera pestaña
    tabPanel("Nivel nacional",
             fluidRow(
               column(
                 width = 12,
                 column(
                   width = 6,
                   column(
                     width = 6,
                     selectInput("año", "Seleccione el año", choices = c(2022, 2021, 2020, 2019, 2018, 2017)),
                     tags$head(tags$style(HTML(".selectize-input {height: 30px; font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))
                   ),
                   column(
                     width = 6,
                     selectInput("codigo", "Seleccione el delito", choices = codificacion_snic$Delito),
                     tags$head(tags$style(HTML(".selectize-input {height: 30px; font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))
                   )
                 ),
                 column(
                   width = 6,
                   tags$head(
                     tags$style(
                       HTML(".blue-box-container {
                          display: flex;
                          flex-wrap: nowrap; /* Mantener una sola línea en dispositivos pequeños */
                          justify-content: space-around;
                          margin-top: -0.25cm;
                          align-items: center;
                        }
                        .blue-box {
                          background-color:#47baec;
                          color: black;
                          padding: 0px;
                          border-radius: 10px;
                          width: 150px;
                          height: 70px; /* Mantener el mismo alto para todas las cajas */
                          text-align: center;
                          font-family: Trebuchet MS;
                          font-size: 16px; /* Tamaño de fuente inicial */
                          margin: 8px;
                          display: inline-flex;
                          justify-content: center;
                          align-items: center;
                        }
                        .blue-box .title {
                          font-size: 18px;
                        }
                        .blue-box .subtitle {
                          font-size: 12px;
                        }")
                     )
                   ),
                   tags$head(
                     tags$style(
                       HTML("@media (max-width: 768px) { /* Ajustar el tamaño de fuente en pantallas pequeñas */
                               .blue-box .title {
                                 font-size: 14px;
                               }
                               .blue-box .subtitle {
                                 font-size: 8px;
                               }
                             }")
                     )
                   ),
                   div(class = "blue-box-container",
                       tagList(
                         div(class = "blue-box",
                             div(
                               div(class = "title", "Total nacional"),
                               div(textOutput(outputId = "total_n"))
                             )
                         ),
                         div(class = "blue-box",
                             div(
                               div(class = "title", "Tasa nacional"),
                               div(textOutput(outputId = "tasa_n")),
                               div(class = "subtitle", "cada 100 mil hab.")
                             )
                         ),
                         div(class = "blue-box",
                             div(
                               div(class = "title", "Variación"),
                               div(textOutput(outputId = "variacion_n")),
                               div(class = "subtitle", "interanual")
                             )
                         )
                       )
                   )
                 )
               )
             ),
             fluidRow(
               style = "height: 10px;"
             ),
             fluidRow(width=12,
                      column(width = 6,leafletOutput(outputId ="mapa_n", height = "1000px")),
                      column(width = 6,
                             fluidRow(
                               column(width = 12, plotlyOutput(outputId ="serie_t_n", height = "500px")),
                               fluidRow(
                                 style = "height: 10px;"
                               ),
                               column(width = 12,
                                      column(width = 6,  # Add a comma here
                                             div(style = "display: flex; justify-content: center;",
                                                 plotlyOutput(outputId ="grafico_total_n", height = "500px")
                                             )),
                                      column(width = 6,
                                             div(style = "display: flex; justify-content: center;", 
                                                 plotlyOutput(outputId ="grafico_tasa_n", height = "500px")
                                             )
                                      )))))),
    tabPanel("Nivel provincial",
             fluidRow(
               column(
                 width = 12,
                 column(
                   width = 6,
                   column(
                     width = 4,
                     selectInput("año2", label = tags$small("Seleccione el año", style = "font-size: 14px;"), choices = c(2022, 2021, 2020, 2019, 2018, 2017)),
                     tags$head(tags$style(HTML(".selectize-input {height: 30px; font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))
                   ),
                   column(
                     width = 4,
                     selectInput("provincia", label = tags$small("Seleccione la jurisdicción", style = "font-size: 14px;"), choices = sort(unique(geo_provincias$provincia))),
                     tags$head(tags$style(HTML(".selectize-input {height: 30px;font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))
                   ),
                   column(
                     width = 4,
                     selectInput("codigo2", label = tags$small("Seleccione el delito", style = "font-size: 14px;"), choices = codificacion_snic$Delito),
                     tags$head(tags$style(HTML(".selectize-input {height: 30px; font-size: 12px;} .selectize-dropdown {z-index: 9999;}")))
                   )
                 ),
                 column(
                   width = 6,
                   tags$head(
                     tags$style(
                       HTML(".blue-box-container {
                          display: flex;
                          flex-wrap: nowrap; /* Mantener una sola línea en dispositivos pequeños */
                          justify-content: space-around;
                          margin-top: -0.25cm;
                          align-items: center;
                        }
                        .blue-box {
                          background-color:#47baec;
                          color: black;
                          padding: 0px;
                          border-radius: 10px;
                          width: 150px;
                          height: 70px; /* Mantener el mismo alto para todas las cajas */
                          text-align: center;
                          font-family: Trebuchet MS;
                          font-size: 16px; /* Tamaño de fuente inicial */
                          margin: 8px;
                          display: inline-flex;
                          justify-content: center;
                          align-items: center;
                        }
                        .blue-box .title {
                          font-size: 16px;
                        }
                        .blue-box .subtitle {
                          font-size: 12px;
                        }")
                     )
                   ),
                   tags$head(
                     tags$style(
                       HTML("@media (max-width: 768px) { /* Ajustar el tamaño de fuente en pantallas pequeñas */
                               .blue-box .title {
                                 font-size: 14px;
                               }
                               .blue-box .subtitle {
                                 font-size: 8px;
                               }
                             }")
                     )
                   ),
                   div(class = "blue-box-container",
                       tagList(
                         div(class = "blue-box",
                             div(
                               div(class = "title", "Total jurisdiccional"),
                               div(textOutput(outputId = "total_j"))
                             )
                         ),
                         div(class = "blue-box",
                             div(
                               div(class = "title", "Tasa jurisdiccional"),
                               div(textOutput(outputId = "tasa_j")),
                               div(class = "subtitle", "cada 100 mil hab.")
                             )
                         ),
                         div(class = "blue-box",
                             div(
                               div(class = "title", "Variación"),
                               div(textOutput(outputId = "variacion_j")),
                               div(class = "subtitle", "interanual")
                             )
                         )
                       )
                   )
                 )
               )
             ),
             fluidRow(
               style = "height: 10px;"
             ),
             fluidRow(width =12,
                      column(width = 6, leafletOutput(outputId = "mapa_j", height = "1000px")),
                      column(width = 6,
                             fluidRow(
                               column(width = 12,plotlyOutput(outputId ="serie_t_j", height = "500px")),
                               fluidRow(
                                 style = "height: 10px;"
                               ),
                               column(width = 12,
                                      column(width = 6,
                                             div(uiOutput("grafico_totales_j", height = "500px"))),
                                      column(width = 6, div(uiOutput(outputId = "grafico_tasas_j", height = "500px"))))))),
             fluidRow(
               style = "height: 10px;"
             ),
             fluidRow(
               column(12,
                      tags$div(
                        style = "background-color: transparent; color: #808080; padding: 0px; font-size: 12px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: -0.10cm;",
                        "Los departamentos que cuentan con víctimas o hechos y una población menor a 50.000 habitantes se exponen en color gris. Los departamentos sin ocurrencia de hechos se exponen en color blanco."
                      ),
                      tags$div(
                        style = "background-color: transparent; color: #808080; padding: 0px; font-size: 12px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: -0.10cm;",
                        "En el caso de la provincia de La Pampa, los límites de las cuatro regiones usadas en el SNIC no se corresponden con los departamentos definidos por el INDEC, sino con las regionales policiales, debido a la metodología de reporte de la información por parte de la provincia."
                      )
               )
             )
    ),
    tabPanel("Nivel departamental",
             column(width = 12,
                    fluidRow(
                      column(width = 2),
                      column(width = 2, selectInput("año3", "Seleccione el año", choices = c(2022, 2021, 2020, 2019, 2018, 2017)),
                             tags$head(tags$style(HTML(".selectize-input {height: 30px; font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))),
                      column(width = 2, selectInput("codigo3", "Seleccione el delito", choices = codificacion_snic$Delito),
                             tags$head(tags$style(HTML(".selectize-input {height: 30px; font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))),
                      column(width = 2, selectInput(inputId = "provincia2", label = "Seleccione la jurisdicción", choices = c("", sort(unique(na.omit(geo_departamentos$provincia))))),
                             tags$head(tags$style(HTML(".selectize-input {height: 30px; font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))),
                      column(width = 2, selectInput(inputId = "departamento", label = "Seleccione el dpto.", choices = NULL),
                             tags$head(tags$style(HTML(".selectize-input {height: 30px; font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))),
                      column(width = 1))),
             fluidRow(width=12,
                      column(width = 6, leafletOutput(outputId = "mapa_dpto", height = "520px")),
                      column(width = 6,
                             fluidRow(
                               style = "height: 10px;"
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 tags$head(
                                   tags$style(
                                     HTML(".blue-box-container {
                          display: flex;
                          flex-wrap: nowrap; /* Mantener una sola línea en dispositivos pequeños */
                          justify-content: space-around;
                          margin-top: -0.25cm;
                          align-items: center;
                        }
                        .blue-box {
                          background-color:#47baec;
                          color: black;
                          padding: 0px;
                          border-radius: 10px;
                          width: 150px;
                          height: 70px; /* Mantener el mismo alto para todas las cajas */
                          text-align: center;
                          font-family: Trebuchet MS;
                          font-size: 16px; /* Tamaño de fuente inicial */
                          margin: 8px;
                          display: inline-flex;
                          justify-content: center;
                          align-items: center;
                        }
                        .blue-box .title {
                          font-size: 16px;
                        }
                        .blue-box .subtitle {
                          font-size: 12px;
                        }")
                                   )
                                 ),
                                 tags$head(
                                   tags$style(
                                     HTML("@media (max-width: 768px) { /* Ajustar el tamaño de fuente en pantallas pequeñas */
                               .blue-box .title {
                                 font-size: 14px;
                               }
                               .blue-box .subtitle {
                                 font-size: 8px;
                               }
                             }")
                                   )
                                 ),
                                 div(class = "blue-box-container",
                                     tagList(
                                       div(class = "blue-box",
                                           div(
                                             div(class = "title", "Tasa nacional"),
                                             div(textOutput(outputId = "tasa_nacional_dpto")),
                                             div(class = "subtitle", "cada 100 mil hab.")
                                           )
                                       ),
                                       div(class = "blue-box",
                                           div(
                                             div(class = "title", "Tasa jurisdiccional"),
                                             div(textOutput(outputId = "tasa_jurisdiccional_dpto")),
                                             div(class = "subtitle", "cada 100 mil hab.")
                                           )
                                       ),
                                       div(class = "blue-box",
                                           div(
                                             div(class = "title", "Tasa departamental"),
                                             div(textOutput(outputId = "tasa_dpto_dpto")),
                                             div(class = "subtitle", "cada 100 mil hab.")
                                           )
                                       )
                                     )
                                 )
                               )),
                             fluidRow(
                               style = "height: 10px;"
                             ),
                             fluidRow(
                               column(width = 12,
                                      plotlyOutput(outputId = "grafico_total_tasa_dpto", height = "430px")
                               )
                             ))),
             fluidRow(
               column(12,
                      tags$div(
                        style = "background-color: transparent; color: #808080; padding: 0px; font-size: 12px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: -0.10cm;",
                        "Los departamentos que cuentan con víctimas o hechos y una población menor a 50.000 habitantes se exponen en color gris. Los departamentos sin ocurrencia de hechos se exponen en color blanco."
                      ),
                      tags$div(
                        style = "background-color: transparent; color: #808080; padding: 0px; font-size: 12px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: -0.10cm;",
                        "En el caso de la provincia de La Pampa, los límites de las cuatro regiones usadas en el SNIC no se corresponden con los departamentos definidos por el INDEC, sino con las regionales policiales, debido a la metodología de reporte de la información por parte de la provincia."
                      )
               )
             )),
    tabPanel("Metodología",
             tags$div(
               style = "background-color: #FFFFFF; color: #5B9BD5; padding: 6px; font-size: 24px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: 0.5cm;",
               "¿Qué es el Sistema Nacional de Información Criminal (SNIC)?"),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "El SNIC es un sistema de recolección y consolidación de datos para el análisis de información estadística criminal en la Argentina, que tiene como objeto brindar información sobre hechos presuntamente delictuosos registrados por las fuerzas policiales provinciales, fuerzas federales de seguridad y otras entidades oficiales de recepción de denuncias, en todo el ámbito del territorio de la República Argentina."),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0.5cm;",
               "Sus objetivos son:",
               tags$ul(
                 tags$li("Generar información y transformar los datos criminales en información estadística, que sea pertinente, confiable, válida, adecuada y oportuna."),
                 tags$li("Colaborar en la generación de dicha información en los diferentes niveles territoriales y jurisdiccionales: nacional, provincial o departamental."),
                 tags$li("Colaborar en la generación, implementación, gestión y evaluación de políticas públicas basadas en evidencias en materia de seguridad.")
               )),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "Este sistema, se compone de un total de cinco módulos:"),
             tags$head(
               tags$style("
    .image-container {
      display: flex;
      justify-content: center;
      align-items: center;
      margin-top: 0cm;
    }
    /* Ajustar el tamaño de la imagen al ancho de la ventana */
    .image-container img {
      max-width: 100%; /* La imagen ocupará como máximo el 100% del ancho de su contenedor */
      height: auto; /* La altura se ajustará proporcionalmente al ancho */
    }
  ")
             ),
             tags$body(
               div(class = "image-container",
                   tags$img(src = sprintf("data:image/png;base64,%s", imagen_base64),
                            height = "230px", width = "700px")
               )
             ),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "En particular, este tablero refleja la información del módulo de SNIC-Total de hechos delictuosos, el cual releva información agregada sobre 56 tipos de hechos delictuosos, tomando en consideración los tipos delictivos establecidos por Código Penal de la Nación y Leyes Especiales."),
             tags$div(
               style = "background-color: #FFFFFF; color: #5B9BD5; padding: 6px; font-size: 24px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: 0.5cm;",
               "Aclaraciones metodológicas"),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "Para el cálculo de las tasas se utilizaron las proyecciones de población basadas en los resultados del Censo Nacional de Población, Hogares y Viviendas 2010 realizadas por INDEC."),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "Las Islas Malvinas, Georgias del Sur, Sandwich del Sur y los espacios marítimos circundantes forman parte integrante del territorio nacional argentino. Debido a que dichos territorios se encuentran sometidos a la ocupación ilegal del Reino Unido de Gran Bretaña e Irlanda del Norte, la República Argentina se vio impedida de incorporar los datos correspondientes a esa área."),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "La información que se encuentra del departamento “sin determinar”, se debe a las Fuerzas Federales, ya que se reporta información desde sus unidades operativas cuyo alcance territorial no coincide con la división política de cada jurisdicción."),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "En el caso de la provincia de La Pampa, los límites de las cuatro regiones usadas en el SNIC no se corresponden con los departamentos definidos por el INDEC, sino con las regionales policiales, debido a la metodología de reporte de la información por parte de la provincia. A fines del cálculo de las tasas, se realizó una aproximación con los departamentos que se incluyen en cada una. A la región Centro se le asignaron los departamentos Capital, Catrilo, Loventué y Toay. A la región Norte se le asignaron los departamentos Chapaleufu, Conhelo, Maracó, Quemú Quemú, Rancul, Realicó y Trenel. A la región Oeste se le asignaron los departamentos Chalileo, Chical Co, Curacó, Limay Mahuida y Puelén. Por último, a la región Sur se le asignaron los departamentos Atreucó, Caleu Caleu, Guatraché, Hucal, Lihuel Calel y Utracan."),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "El reporte de cantidad de víctimas es obligatorio solo para algunos tipos delictuales del SNIC. Por ello, en el tablero se expone cantidad de víctimas de los siguientes tipos de delitos:",
               tags$ul(
                 tags$div(style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
                          "Homicidios dolosos, Homicidios dolosos en grado de tentativa, Muertes en accidentes viales, Homicidios culposos por otros hechos, Lesiones dolosas, Lesiones culposas en accidentes de tránsito, Lesiones culposas por otros hechos, Otros delitos contra las personas, Delitos contra el honor, Abuso sexual con acceso carnal (Violación), Otros delitos contra la integridad sexual, Trata de personas simple, Trata de personas agravado, Otros delitos contra la libertad, Suicidios (consumados)."))),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: -0.5cm;",
               "Para los delitos restantes, se expone la cantidad de hechos."),
             tags$div(
               style = "background-color: #FFFFFF; color: #5B9BD5; padding: 6px; font-size: 24px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: 0.5cm;",
               "Unidades de análisis"),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               HTML("<b>Las personas:</b>"), "son todos los individuos involucrados en el evento delictual. En particular, el módulo SNIC sólo registra información respectiva a las víctimas."
             ),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               HTML("<b>Los hechos delictuales:</b>"), "considerados tales por el derecho penal y que, por tanto, se relevan para construir la estadística criminal. Estos delitos son acciones tipificadas como ilícitas y antijurídicas por el Código Penal y Leyes Especiales de la Nación. En un mismo evento delictual pueden concurrir varios delitos/hechos delictuales, y que cada uno de estos delitos (hechos) debe ser registrado en el SNIC, corresponde 1 hecho = 1 delito."),
             tags$div(
               style = "background-color: #FFFFFF; color: #5B9BD5; padding: 6px; font-size: 24px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: 0.5cm;",
               "Cálculos"),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "Una tasa es una relación entre dos magnitudes. Se trata de un coeficiente que expresa la relación existente entre una cantidad y la frecuencia de un fenómeno. De esta forma, la tasa permite expresar la existencia de una situación que no puede ser medida o calculada de forma directa."),
             tags$head(
               tags$script(src = "https://polyfill.io/v3/polyfill.min.js?features=es6"),
               tags$script(src = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
             ),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "",
               tags$div(
                 tags$script(HTML("MathJax.Hub.Queue(['Typeset', MathJax.Hub]);")),
                 tags$div(
                   style = "text-align: center;",
                   "$$\\text{Tasa} = \\frac{\\text{Cantidad de delitos}}{\\text{Población total}} \\times 100.000$$"
                 )
               )
             ),
             tags$div(
               style = "background-color: #FFFFFF; color: #5B9BD5; padding: 6px; font-size: 24px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "Documentación"),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "Para acceder y visualizar el manual de usuario de las bases de datos, haz clic en el siguiente enlace: ",
               tags$a(
                 href = "https://minsegar-my.sharepoint.com/:b:/g/personal/dnec_minseg_gob_ar/ES8jLhantixGtZP6s_-llssBtWcRjGcTnJW2TfixYyy5ug?e=FQFWc7",
                 target = "_blank",
                 style = "background-color: #FFFFFF; color: #5B9BD5; font-family: Trebuchet MS;",
                 "Manual de usuario"
               )
             ),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "Para acceder y visualizar el glosario delictual, haz clic en el siguiente enlace: ",
               tags$a(
                 href = "https://minsegar-my.sharepoint.com/:b:/g/personal/dnec_minseg_gob_ar/EYcQOQvAU1pClC5Lu1Ua3jcB3SQjt4XRqaUwRM2nEgd4tg?e=9Iozsq",
                 target = "_blank",
                 style = "background-color: #FFFFFF; color: #5B9BD5; font-family: Trebuchet MS;",
                 "Glosario delictual"
               )
             ),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "Para acceder y visualizar las bases de datos, haz clic en el siguiente enlace: ",
               tags$a(
                 href = "https://www.argentina.gob.ar/seguridad/estadisticascriminales/bases-de-datos",
                 target = "_blank",
                 style = "background-color: #FFFFFF; color: #5B9BD5; font-family: Trebuchet MS;",
                 "Bases de datos"
               )
             ),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               "Para acceder y visualizar los informes, haz clic en el siguiente enlace: ",
               tags$a(
                 href = "https://www.argentina.gob.ar/seguridad/estadisticascriminales/informes",
                 target = "_blank",
                 style = "background-color: #FFFFFF; color: #5B9BD5; font-family: Trebuchet MS;",
                 "Informes"
               )
             ),
             tags$div(
               style = "background-color: #FFFFFF; color: #666262; padding: 6px; font-size: 20px; text-align: left; font-family: Trebuchet MS; margin-top: 0cm;",
               ""),
             tags$p(
               tags$a(
                 "Contacto | snic-sat@minseg.gob.ar",
                 href = "mailto:snic-sat@minseg.gob.ar",
                 style = "color: #666262; padding: 6px; font-size: 14px; font-family: Trebuchet MS;"
               ),
               style = "background-color: #FFFFFF; text-align: center; margin-top: 0cm;"
             ))),
  tags$div(
    style = "background-color:#47baec; color: #333535; padding: 6px; font-size: 12px; font-weight: bold; text-align: left; font-family: Trebuchet MS; margin-top: 0.3cm;",
    "Fuente: Sistema Nacional de Información Criminal - Sistema Alerta Temprana (SNIC - SAT), Ministerio de Seguridad de la Nación e INDEC. ")
)


server <- function(input, output, session) {
  output$mapa_n <- renderLeaflet({
    datos_provinciales <- datos_provinciales %>%
      mutate(provincia_nombre = ifelse(provincia_nombre == "Ciudad Autónoma de Buenos Aires", "CABA", provincia_nombre))
    datos_n_filtrado <- subset(datos_provinciales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] & anio %in% c(input$año))
    datos_mn <- left_join(geo_provincias, datos_n_filtrado, by = c("ID" = "provincia_id"))
    
    datos_mn$pob_tot <- format(datos_mn$pob_tot, big.mark = ".")
    datos_mn$tasa_victimas <- format(round(datos_mn$tasa_victimas, 1), decimal.mark = ",", big.mark = ".")
    datos_mn$tasa_hechos <- format(round(datos_mn$tasa_hechos, 1), decimal.mark = ",", big.mark = ".")
    datos_mn$victimas <- format(datos_mn$victimas, big.mark = ".")
    datos_mn$hechos <- format(datos_mn$hechos, big.mark = ".")
    
    datos_mn <- datos_mn%>%
      mutate(
        color = ifelse(is.na(color), "#d9d9d9", color),
        victimas = ifelse(grepl("\\bNA\\b", datos_mn$victimas), "Sin datos", victimas),
        hechos = ifelse(grepl("\\bNA\\b", datos_mn$hechos), "Sin datos", hechos),
        tasa_victimas = ifelse(grepl("\\bNA\\b", datos_mn$tasa_victimas), "Sin datos", tasa_victimas),
        tasa_hechos = ifelse(grepl("\\bNA\\b", datos_mn$tasa_hechos), "Sin datos", tasa_hechos),
        pob_tot = ifelse(grepl("\\bNA\\b", datos_mn$pob_tot), "Sin datos", pob_tot),
        provincia.x = ifelse(provincia.x == "Otro", "Antártida e Islas del Atlántico Sur", provincia.x)
      )
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      leaflet() %>%
        addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png") %>%
        addPolygons(data = datos_mn, color = ~color, weight = 0, fillOpacity = 1)%>%
        addPolygons(data = datos_mn,weight = 1, color = "black", fillColor = "transparent", fillOpacity = 0.5,
                    highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                    label = lapply(1:nrow(datos_mn), function(i) {
                      label <- paste0('<span style="font-size: 12px;">',
                                      "Jurisdicción: ", datos_mn$provincia.x[i], "<br>",
                                      "Víctimas: ", datos_mn$victimas[i], "<br>",
                                      "Tasa: ", datos_mn$tasa_victimas[i], "<br>",
                                      "Población: ", datos_mn$pob_tot[i])
                      return(htmltools::HTML(label))}))%>%
        fitBounds(lng1 = -73.5604, lat1 = -55.0512, lng2 = -53.6375, lat2 = -21.7819)
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {
      leaflet() %>%
        addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png") %>%
        addPolygons(data = datos_mn, color = ~color, weight = 0, fillOpacity = 1)%>%
        addPolygons(data = datos_mn,weight = 1, color = "black", fillColor = "transparent", fillOpacity = 0.5,
                    highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                    label = lapply(1:nrow(datos_mn), function(i) {
                      label <- paste0('<span style="font-size: 12px;">',
                                      "Jurisdicción: ", datos_mn$provincia.x[i], "<br>",
                                      "Hechos: ", datos_mn$hechos[i], "<br>",
                                      "Tasa: ", datos_mn$tasa_hechos[i], "<br>",
                                      "Población: ", datos_mn$pob_tot[i])
                      return(htmltools::HTML(label))}))%>%
        fitBounds(lng1 = -73.5604, lat1 = -55.0512, lng2 = -53.6375, lat2 = -21.7819)
    }
    else {print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  output$grafico_total_n <- renderPlotly({
    colores <- c("#47baec")
    datos_provinciales <- datos_provinciales %>%
      mutate(provincia_nombre = ifelse(provincia_nombre == "Ciudad Autónoma de Buenos Aires", "CABA", provincia_nombre))
    datos_grafico_total_n <- subset(datos_provinciales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] & anio %in% c(input$año))
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      #Filtrar y agrupar por tipo de codigo y año. Sumar los hechos por codigo y añ
      plot_ly(
        datos_grafico_total_n, 
        x = datos_grafico_total_n$victimas, 
        y = reorder(datos_grafico_total_n$provincia_nombre, datos_grafico_total_n$victimas),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Total de víctimas por jurisdicción", font = list(color = "grey")),
          xaxis = list(
            title = "Víctimas", 
            titlefont = list(color = "grey"), 
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "", tickfont = list(size = 12, color = "gray"), showgrid = FALSE),
          showlegend = FALSE,
          margin = list(l = 50, pad = 5) # Ajustar el margen izquierdo para dar espacio al texto dentro de las barras
        ) %>% 
        add_annotations(
          x = datos_grafico_total_n$victimas,
          y = reorder(datos_grafico_total_n$provincia_nombre, datos_grafico_total_n$victimas),
          text = format(datos_grafico_total_n$victimas, big.mark="."),
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xref = "x", # Posicionamiento automático fuera de las barras
          xanchor = "left", # Alineación del texto con respecto al punto de referencia
          xshift = 2 # Ajustar el espaciado entre el texto y las barras
        )%>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {  
      #Filtrar y agrupar por tipo de codigo y año. Sumar los hechos por codigo y año
      plot_ly(
        datos_grafico_total_n, 
        x = datos_grafico_total_n$hechos, 
        y = reorder(datos_grafico_total_n$provincia_nombre, datos_grafico_total_n$hechos),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Total de hechos por jurisdicción", font = list(color = "grey")),
          xaxis = list(
            title = "Hechos",
            titlefont = list(color = "grey"),
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "", tickfont = list(size = 12, color = "gray"), showgrid = FALSE),
          showlegend = FALSE,
          margin = list(l = 50, pad = 5) # Ajustar el margen izquierdo para dar espacio al texto dentro de las barras
        ) %>% 
        add_annotations(
          x = datos_grafico_total_n$hechos,
          y = reorder(datos_grafico_total_n$provincia_nombre, datos_grafico_total_n$hechos),
          text = format(datos_grafico_total_n$hechos, big.mark="."),
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xref = "x", # Posicionamiento automático fuera de las barras
          xanchor = "left", # Alineación del texto con respecto al punto de referencia
          xshift = 2 # Ajustar el espaciado entre el texto y las barras
        )%>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))  
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  
  output$grafico_tasa_n <- renderPlotly({
    colores <- c("##0A72B9")
    datos_provinciales <- datos_provinciales %>%
      mutate(provincia_nombre = ifelse(provincia_nombre == "Ciudad Autónoma de Buenos Aires", "CABA", provincia_nombre))
    datos_grafico_tasa_n<- subset(datos_provinciales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] & anio %in% c(input$año))
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      #Filtrar y agrupar por tipo de codigo y año. Sumar los hechos por codigo y añ
      datos_grafico_tasa_n$tasa_victimas <- round(datos_grafico_tasa_n$tasa_victimas, 1)
      plot_ly(
        datos_grafico_tasa_n, 
        x = datos_grafico_tasa_n$tasa_victimas, 
        y = reorder(datos_grafico_tasa_n$provincia_nombre, datos_grafico_tasa_n$tasa_victimas),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Tasa de víctimas por jurisdicción", font = list(color = "grey")),
          xaxis = list(
            title = "Tasa cada 100.000 hab.",
            titlefont = list(color = "grey"),
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "", tickfont = list(size = 12, color = "gray"), showgrid = FALSE),
          showlegend = FALSE,
          margin = list(l = 50, pad = 5) # Ajustar el margen izquierdo para dar espacio al texto dentro de las barras
        ) %>% 
        add_annotations(
          x = datos_grafico_tasa_n$tasa_victimas,
          y = reorder(datos_grafico_tasa_n$provincia_nombre, datos_grafico_tasa_n$tasa_victimas),
          text = format(datos_grafico_tasa_n$tasa_victimas, big.mark = ".", decimal.mark = ",") ,
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xref = "x", # Posicionamiento automático fuera de las barras
          xanchor = "left", # Alineación del texto con respecto al punto de referencia
          xshift = 2 # Ajustar el espaciado entre el texto y las barras
        )%>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {  
      datos_grafico_tasa_n$tasa_hechos <- round(datos_grafico_tasa_n$tasa_hechos, 1)
      plot_ly(
        datos_grafico_tasa_n, 
        x = datos_grafico_tasa_n$tasa_hechos, 
        y = reorder(datos_grafico_tasa_n$provincia_nombre, datos_grafico_tasa_n$tasa_hechos),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Tasa de hechos por jurisdicción", font = list(color = "grey")),
          xaxis = list(
            title = "Tasa cada 100.000 hab.",
            titlefont = list(color = "grey"),
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "",tickfont = list(size = 12, color = "gray"), showgrid = FALSE),
          showlegend = FALSE,
          margin = list(l = 50, pad = 5) # Ajustar el margen inferior para evitar que se muestren los números del eje x
        ) %>% 
        add_annotations(
          x = datos_grafico_tasa_n$tasa_hechos,
          y = reorder(datos_grafico_tasa_n$provincia_nombre, datos_grafico_tasa_n$tasa_hechos),
          text = format(datos_grafico_tasa_n$tasa_hechos, big.mark = ".", decimal.mark = ","),
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xref = "x", # Posicionamiento automático fuera de las barras
          xanchor = "left", # Alineación del texto con respecto al punto de referencia
          xshift = 2 # Ajustar el espaciado entre el texto y las barras
        )%>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  
  output$total_n <- renderText({
    valor_total_n <- subset(datos_nacionales, codigo_delito_snic_id %in% c(codificacion_snic$codigo[codificacion_snic$Delito == input$codigo]) & anio %in% c(input$año))
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      format(valor_total_n$cantidad_victimas, big.mark = ".", decimal.mark = ",")
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {
      format(valor_total_n$cantidad_hechos, big.mark = ".", decimal.mark = ",")
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  
  output$tasa_n <- renderText({
    valor_tasa_n <- subset(datos_nacionales, codigo_delito_snic_id %in% c(codificacion_snic$codigo[codificacion_snic$Delito == input$codigo]) & anio %in% c(input$año))
    valor_tasa_n$tasa_hechos <- round(valor_tasa_n$tasa_hechos, 1)  
    valor_tasa_n$tasa_victi<- round(valor_tasa_n$tasa_victi, 1)
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      format(valor_tasa_n$tasa_victi, big.mark = ".", decimal.mark = ",", nsmall = 1)
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {
      format(valor_tasa_n$tasa_hechos, big.mark = ".", decimal.mark = ",", nsmall = 1)
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }   
  })
  
  
  output$variacion_n <- renderText({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      n_variacion<- subset(datos_nacionales, codigo_delito_snic_id %in% c(codificacion_snic$codigo[codificacion_snic$Delito == input$codigo]) & anio %in% c(input$año))
      n_variacion$variacion_anual_vic <- paste0(format(as.numeric(n_variacion$variacion_anual_vic), decimal.mark = ",", big.mark = "."), "%")
      n_variacion$variacion_anual_vic <- ifelse(n_variacion$variacion_anual_vic == "Inf%" | n_variacion$variacion_anual_vic == "NaN%" | n_variacion$variacion_anual_vic == "NA%", "-", n_variacion$variacion_anual_vic)
      n_variacion$variacion_anual_vic
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) { 
      n_variacion<- subset(datos_nacionales, codigo_delito_snic_id %in% c(codificacion_snic$codigo[codificacion_snic$Delito == input$codigo]) & anio %in% c(input$año))
      n_variacion$variacion_anual_hech <- paste0(format(as.numeric(n_variacion$variacion_anual_hech), decimal.mark = ",", big.mark = "."), "%")
      n_variacion$variacion_anual_hech <- ifelse(n_variacion$variacion_anual_hech == "Inf%" | n_variacion$variacion_anual_hech == "NaN%" | n_variacion$variacion_anual_hech == "NA%", "-", n_variacion$variacion_anual_hech)
      n_variacion$variacion_anual_hech
      
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }    
  })
  
  output$serie_t_n <- renderPlotly({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      df_filtrado<- subset(datos_nacionales, codigo_delito_snic_id %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo])
      colores <- c("#47baec")
      
      plot_ly(df_filtrado, 
              type = "bar", 
              x = ~anio,
              y = ~cantidad_victimas, 
              marker = list(color = colores),
              textposition = "auto", 
              name = "Víctimas",
              orientation = "v") %>% 
        add_trace(y = ~tasa_victi, name = "Tasa cada\n100.000 hab.", type = "scatter", marker = list(color = "#0A72B9", size = 10),
                  mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9")) %>% 
        layout(title = list(text = "Víctimas y tasas del delito seleccionado<br>a nivel nacional. Años 2017-2022", x = 0.5, y= 0.95, font = list(size = 16)),
               yaxis = list(side = "right", title = "Víctimas", font = list(color = "grey"), titlefont = list(color = "grey"), 
                            range = c(0, max(df_filtrado$cantidad_victimas)*1.05), showgrid = FALSE),
               yaxis2 = list(side = "left", title = "Tasa de víctimas", overlaying = "y", side = "left", 
                             tickfont = list(color = "grey"), titlefont = list(color = "grey"), 
                             range = c(0, max(df_filtrado$tasa_victi)*1.1), showgrid = FALSE),
               xaxis = list(title = list(text = "Año", standoff = 10), font = list(color = "grey")),  # Adjust the standoff value here
               titlefont = list(color = "grey"),
               legend = list(x = 0.5, y = -0.15, orientation = "h", xanchor = "center",
                             itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
               font = list(color = "grey"),
               autosize = "responsive",  # Make the plot responsive
               margin = list(t = 60)      # Add 60 pixels of margin at the top
        ) %>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {
      
      df_filtrado<- subset(datos_nacionales, codigo_delito_snic_id %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo])
      colores <- c("#47baec")
      plot_ly(df_filtrado, 
              type = "bar", 
              x = ~anio,
              y = ~cantidad_hechos, 
              marker = list(color = colores),
              textposition = "auto", 
              name = "Hechos",
              orientation = "v",
              autosize = "responsive")%>% 
        add_trace(y = ~tasa_hechos, name = "Tasa cada\n100.000 hab.", type = "scatter", marker = list(color = "#0A72B9", size = 10),
                  mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9"),autosize = "responsive") %>% 
        layout(title = list(text = "Hechos y tasas del delito seleccionado<br>a nivel nacional. Años 2017-2022", x = 0.5, y= 0.95, font = list(size = 16)),
               yaxis = list(side = "right", title = "Hechos", font = list(color = "grey"), titlefont = list(color = "grey"), 
                            range = c(0, max(df_filtrado$cantidad_hechos)*1.05), showgrid = FALSE),
               yaxis2 = list(side = "left", title = "Tasa de hechos", overlaying = "y", side = "left", 
                             tickfont = list(color = "grey"), titlefont = list(color = "grey"), 
                             range = c(0, max(df_filtrado$tasa_hechos)*1.1), showgrid = FALSE),
               xaxis = list(title = list(text = "Año", standoff = 10), font = list(color = "grey")),  # Adjust the standoff value here
               titlefont = list(color = "grey"),
               legend = list(x = 0.5, y = -0.15, orientation = "h", xanchor = "center",
                             itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
               font = list(color = "grey"),
               autosize = "responsive",  # Make the plot responsive
               margin = list(t = 60)      # Add 60 pixels of margin at the top
        ) %>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
      
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  output$mapa_j <- renderLeaflet({
    datos_dpto_geo <- geo_departamentos# %>% filter(provincia %in% c(input$provincia))
    datos_jurs_geo <- geo_provincias%>% filter(provincia %in% c(input$provincia))
    datos_j_filtrado <- subset(datos_departamentales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] & anio %in% c(input$año2))
    datos_mj <- left_join(datos_dpto_geo, datos_j_filtrado, by = c("link" = "Cod_INDEC"))
    
    datos_mj$poblacion <- format(datos_mj$poblacion, big.mark = ".")
    datos_mj$tasa_victimas <- format(round(datos_mj$tasa_victimas, 1), decimal.mark = ",", big.mark = ".")
    datos_mj$tasa_hechos <- format(round(datos_mj$tasa_hechos, 1), decimal.mark = ",", big.mark = ".")
    datos_mj$victimas <- format(datos_mj$victimas, big.mark = ".")
    datos_mj$hechos <- format(datos_mj$hechos, big.mark = ".")
    
    
    datos_mj <- datos_mj%>%
      mutate(
        color_prov = ifelse(is.na(color_prov), "#d9d9d9", color_prov),
        victimas = ifelse(grepl("\\bNA\\b", datos_mj$victimas), "Sin datos", victimas),
        hechos = ifelse(grepl("\\bNA\\b", datos_mj$hechos), "Sin datos", hechos),
        tasa_victimas = ifelse(grepl("\\bNA\\b", datos_mj$tasa_victimas), "Sin datos", tasa_victimas),
        tasa_hechos = ifelse(grepl("\\bNA\\b", datos_mj$tasa_hechos), "Sin datos", tasa_hechos),
        poblacion = ifelse(grepl("\\bNA\\b", datos_mj$poblacion), "Sin datos", poblacion),
        departamen = ifelse(departamen == "Islas del Atlántico Sur", "Antártida e Islas del Atlántico Sur", departamen)
      )
    
    datos_mj <- subset(datos_mj, provincia.x %in% c(input$provincia))
    
    if (input$provincia == "Tierra del Fuego") {
      if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes) {
        leaflet() %>%
          addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
          addPolygons(data = datos_mj, 
                      color = ~color_prov,
                      weight = 0,
                      fillOpacity = 1)%>%
          addPolygons(data = datos_mj, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                      highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                      label = lapply(1:nrow(datos_mj), function(i) {
                        label <- paste0('<span style="font-size: 12px;">',
                                        "Departamento: ", datos_mj$departamen[i], "<br>",
                                        "Víctimas: ", datos_mj$victimas[i], "<br>",
                                        "Tasa: ", datos_mj$tasa_victimas[i], "<br>",
                                        "Población: ", datos_mj$poblacion[i])
                        return(htmltools::HTML(label))}))%>%
          setView(lng=-66.478801, lat=-54.057724, zoom=7)
      } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2]  %in% valid_codes2) {
        leaflet() %>%
          addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
          addPolygons(data = datos_mj, 
                      color = ~color_prov,
                      weight = 0,
                      fillOpacity = 1)%>%
          addPolygons(data = datos_mj, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                      highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                      label = lapply(1:nrow(datos_mj), function(i) {
                        label <- paste0('<span style="font-size: 12px;">',
                                        "Departamento: ", datos_mj$departamen[i], "<br>",
                                        "Hechos: ", datos_mj$hechos[i], "<br>",
                                        "Tasa: ", datos_mj$tasa_hechos[i], "<br>",
                                        "Población: ", datos_mj$poblacion[i])
                        return(htmltools::HTML(label))}))%>%
          setView(lng=-66.478801, lat=-54.057724, zoom=7)
      } else {print("El valor ingresado no corresponde a un delito.")
      }
    } else{
      if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes) {
        leaflet() %>%
          addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
          addPolygons(data = datos_mj, 
                      color = ~color_prov,
                      weight = 0,
                      fillOpacity = 1)%>%
          addPolygons(data = datos_mj, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                      highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                      label = lapply(1:nrow(datos_mj), function(i) {
                        label <- paste0('<span style="font-size: 12px;">',
                                        "Departamento: ", datos_mj$departamen[i], "<br>",
                                        "Víctimas: ", datos_mj$victimas[i], "<br>",
                                        "Tasa: ", datos_mj$tasa_victimas[i], "<br>",
                                        "Población: ", datos_mj$poblacion[i])
                        return(htmltools::HTML(label))}))
      } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes2) {
        leaflet() %>%
          addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
          addPolygons(data = datos_mj, 
                      color = ~color_prov,
                      weight = 0,
                      fillOpacity = 1)%>%
          addPolygons(data = datos_mj, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                      highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                      label = lapply(1:nrow(datos_mj), function(i) {
                        label <- paste0('<span style="font-size: 12px;">',
                                        "Departamento: ", datos_mj$departamen[i], "<br>",
                                        "Hechos: ", datos_mj$hechos[i], "<br>",
                                        "Tasa: ", datos_mj$tasa_hechos[i], "<br>",
                                        "Población: ", datos_mj$poblacion[i])
                        return(htmltools::HTML(label))}))
      } else {print("El valor ingresado no corresponde a un delito.")
      }
    }
  })
  
  
  grafico_total_j <- reactive({
    df <- subset(datos_departamentales, provincia_nombre %in% input$provincia & codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] & anio %in% input$año2)
    df
  })
  
  
  output$grafico_totales_j <- renderUI({
    if (nrow(grafico_total_j()) > 24) {
      div(
        style = paste0("height: 500px; overflow-y: scroll;"),
        plotlyOutput("grafico2", height = nrow(grafico_total_j()) * 25)
      )
    } else {
      plotlyOutput("grafico2", height = "500px")
    }
  })
  
  
  output$grafico2 <- renderPlotly({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes) {
      plot_ly(
        grafico_total_j(), 
        x = grafico_total_j()$victimas, 
        y = reorder(grafico_total_j()$departamento, grafico_total_j()$victimas),
        type = "bar", 
        orientation = "h", 
        marker = list(color = "#47baec", textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Total de víctimas por departamento", font = list(color = "grey")),
          xaxis = list(
            title = "Víctimas", 
            titlefont = list(color = "grey"), 
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "", tickfont = list(size = 12, color = "gray")),
          showlegend = FALSE,
          margin = list(l = 50, pad = 5)
        ) %>% 
        add_annotations(
          x = grafico_total_j()$victimas,
          y = reorder(grafico_total_j()$departamento, grafico_total_j()$victimas),
          text = format(grafico_total_j()$victimas, big.mark="."),
          font = list(size = 12, color = "grey"),
          showarrow = FALSE,
          xref = "x", # Posicionamiento automático fuera de las barras
          xanchor = "left", # Alineación del texto con respecto al punto de referencia
          xshift = 2 # Ajustar el espaciado entre el texto y las barras
        )%>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))  
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2]%in% valid_codes2) {
      plot_ly(
        grafico_total_j(), 
        x = grafico_total_j()$hechos, 
        y = reorder(grafico_total_j()$departamento, grafico_total_j()$hechos),
        type = "bar", 
        orientation = "h", 
        marker = list(color = "#47baec", textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Total de hechos por departamento", font = list(color = "grey")),
          xaxis = list(
            title = "Hechos", 
            titlefont = list(color = "grey"), 
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "", tickfont = list(size = 12, color = "gray")),
          showlegend = FALSE,
          margin = list(l = 50, pad = 5) # Ajustar el margen izquierdo para dar espacio al texto dentro de las barras
        ) %>% 
        add_annotations(
          x = grafico_total_j()$hechos,
          y = reorder(grafico_total_j()$departamento, grafico_total_j()$hechos),
          text = format(grafico_total_j()$hechos, big.mark="."),
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xref = "x", # Posicionamiento automático fuera de las barras
          xanchor = "left", # Alineación del texto con respecto al punto de referencia
          xshift = 2 # Ajustar el espaciado entre el texto y las barras
        )%>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))  
      
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
    
  })
  
  
  datos_grafico_tasa_j <- reactive({
    subset(datos_departamentales, provincia_nombre %in% c(input$provincia) & codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] & anio %in% c(input$año2))
  })
  
  
  output$grafico_tasas_j <- renderUI({
    if (nrow(datos_grafico_tasa_j()) > 24) {
      div(
        style = paste0("height: 500px; overflow-y: scroll;"),
        plotlyOutput("grafico22", height = nrow(datos_grafico_tasa_j()) * 25)
      )
    } else {
      plotlyOutput("grafico22", height = "500px")
    }
  })
  
  
  output$grafico22 <- renderPlotly({
    df_grafico_tasa_j <- datos_grafico_tasa_j()
    df_grafico_tasa_j <- na.omit(df_grafico_tasa_j)
    
    # Agregar nueva columna con valor lógico para población menor a 50 mil
    df_grafico_tasa_j$menor_50mil <- df_grafico_tasa_j$poblacion < 50000
    
    # Definir colores para los departamentos
    colores <- ifelse(df_grafico_tasa_j$menor_50mil, "grey", "#0A72B9")
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes) {
      df_grafico_tasa_j$tasa_victimas <- round(df_grafico_tasa_j$tasa_victimas, 1)
      plot_ly(
        df_grafico_tasa_j, 
        x = df_grafico_tasa_j$tasa_victimas, 
        y = reorder(df_grafico_tasa_j$departamento, df_grafico_tasa_j$tasa_victimas),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Tasa de víctimas por departamento", font = list(color = "grey")),
          xaxis = list(
            title = "Tasa cada 100.000 hab.", 
            titlefont = list(color = "grey"), 
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "", tickfont = list(size = 12, color = "gray")),
          showlegend = FALSE,
          margin = list(l = 50, pad = 5) # Ajustar el margen izquierdo para dar espacio al texto dentro de las barras
        ) %>%
        add_annotations(
          x = df_grafico_tasa_j$tasa_victimas,
          y = reorder(df_grafico_tasa_j$departamento, df_grafico_tasa_j$tasa_victimas),
          text = format(df_grafico_tasa_j$tasa_victimas, big.mark = ".", decimal.mark = ","),
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xref = "x", # Posicionamiento automático fuera de las barras
          xanchor = "left", # Alineación del texto con respecto al punto de referencia
          xshift = 2 # Ajustar el espaciado entre el texto y las barras
        )%>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))  
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2]%in% valid_codes2) {
      df_grafico_tasa_j$tasa_hechos <- round(df_grafico_tasa_j$tasa_hechos, 1)
      plot_ly(
        df_grafico_tasa_j, 
        x = df_grafico_tasa_j$tasa_hechos, 
        y = reorder(df_grafico_tasa_j$departamento, df_grafico_tasa_j$tasa_hechos),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Tasa de hechos por departamento", font = list(color = "grey")),
          xaxis = list(
            title = "Tasa cada 100.000 hab.", 
            titlefont = list(color = "grey"), 
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "", tickfont = list(size = 12, color = "gray")),
          showlegend = FALSE,
          margin = list(l = 50, pad = 5) # Ajustar el margen izquierdo para dar espacio al texto dentro de las barras
        ) %>%
        add_annotations(
          x = df_grafico_tasa_j$tasa_hechos,
          y = reorder(df_grafico_tasa_j$departamento, df_grafico_tasa_j$tasa_hechos),
          text = format(df_grafico_tasa_j$tasa_hechos, big.mark = ".", decimal.mark = ","),
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xref = "x", # Posicionamiento automático fuera de las barras
          xanchor = "left", # Alineación del texto con respecto al punto de referencia
          xshift = 2 # Ajustar el espaciado entre el texto y las barras
        )%>%
        config(displayModeBar = FALSE)%>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))  
      
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }   
  })
  
  
  output$total_j <- renderText({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes) {
      valor_total_j <- subset(datos_provinciales, codigo %in%codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2]& anio %in% c(input$año2) & provincia_nombre %in% c(input$provincia))
      format(valor_total_j$victimas, big.mark = ".", decimal.mark = ",")
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes2) {
      valor_total_j <- subset(datos_provinciales, codigo %in%codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2]& anio %in% c(input$año2) & provincia_nombre %in% c(input$provincia))
      format(valor_total_j$hechos, big.mark = ".", decimal.mark = ",")
      
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  
  output$tasa_j <- renderText({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes) {
      datos_provinciales$tasa_victimas<- round(datos_provinciales$tasa_victimas, 1)
      valor_tasa_j <- subset(datos_provinciales, codigo %in%codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2]& anio %in% c(input$año2) & provincia_nombre %in% c(input$provincia))
      format(valor_tasa_j$tasa_victimas, big.mark = ".", decimal.mark = ",", nsmall = 1)
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes2) {
      datos_provinciales$tasa_hechos<- round(datos_provinciales$tasa_hechos, 1)
      valor_tasa_j <- subset(datos_provinciales, codigo %in%codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2]& anio %in% c(input$año2) & provincia_nombre %in% c(input$provincia))
      format(valor_tasa_j$tasa_hechos, big.mark = ".", decimal.mark = ",", nsmall = 1)
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  
  output$variacion_j <- renderText({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes) {
      
      j_variacion <- subset(datos_provinciales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] & anio %in% c(input$año2) & provincia_nombre %in% c(input$provincia))
      j_variacion$variacion_anual_vic <- paste0(format(as.numeric(j_variacion$variacion_anual_vic), decimal.mark = ",", big.mark = "."), "%")
      j_variacion$variacion_anual_vic <- ifelse(j_variacion$variacion_anual_vic == "Inf%" | j_variacion$variacion_anual_vic == "NaN%" | j_variacion$variacion_anual_vic == "NA%", "-", j_variacion$variacion_anual_vic)
      j_variacion$variacion_anual_vic
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes2) {
      
      j_variacion <- subset(datos_provinciales, provincia_nombre %in% c(input$provincia) & codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] & anio %in% c(input$año2))
      j_variacion$variacion_anual_hech <- paste0(format(as.numeric(j_variacion$variacion_anual_hech), decimal.mark = ",", big.mark = "."), "%")
      j_variacion$variacion_anual_hech <- ifelse(j_variacion$variacion_anual_hech == "Inf%" | j_variacion$variacion_anual_hech == "NaN%" | j_variacion$variacion_anual_hech == "NA%", "-", j_variacion$variacion_anual_hech)
      j_variacion$variacion_anual_hech
      
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }   
  })
  
  
  output$serie_t_j <- renderPlotly({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] %in% valid_codes) {
      df_filtrado <- subset(datos_provinciales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] & provincia_nombre %in% input$provincia)
      colores <- c("#47baec")
      
      if (all(df_filtrado$tasa_victimas == 0 & df_filtrado$victimas == 0)) {
        plot_ly(df_filtrado, 
                type = "bar", 
                x = ~anio,
                y = ~victimas, 
                marker = list(color = colores),
                textposition = "auto", 
                name = "Víctimas",
                orientation = "v") %>% 
          add_trace(y = ~tasa_victimas, name = "Tasa cada\n100.000 hab.", type = "scatter", marker = list(color = "#0A72B9", size = 10),
                    mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9")) %>% 
          layout(title = list(text = "Víctimas y tasas del delito seleccionado a<br>nivel jurisdiccional. Años 2017-2022", x = 0.5, y= 0.95, font = list(size = 16)),
                 yaxis = list(side = "right", title = "Víctimas", font = list(color = "grey"), titlefont = list(color = "grey"), 
                              range = c(-0.5, max(df_filtrado$victimas) + 10), showgrid = FALSE),
                 yaxis2 = list(side = "left", title = "Tasa de víctimas", overlaying = "y", side = "left", 
                               tickfont = list(color = "grey"), titlefont = list(color = "grey"), 
                               range = c(-0.5, max(df_filtrado$tasa_victimas) + 10), showgrid = FALSE), 
                 xaxis = list(title = list(text = "Año", standoff = 10), font = list(color = "grey")),  # Adjust the standoff value here
                 titlefont = list(color = "grey"),
                 legend = list(x = 0.5, y = -0.15, orientation = "h", xanchor = "center",
                               itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                 font = list(color = "grey"),
                 autosize = "responsive", margin = list(t = 60)) %>%
          config(displayModeBar = FALSE)%>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
      } else {
        plot_ly(df_filtrado, 
                type = "bar", 
                x = ~anio,
                y = ~victimas, 
                marker = list(color = colores),
                textposition = "auto", 
                name = "Víctimas",
                orientation = "v") %>% 
          add_trace(y = ~tasa_victimas, name = "Tasa cada\n100.000 hab.", type = "scatter", marker = list(color = "#0A72B9", size = 10),
                    mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9")) %>% 
          layout(title = list(text = "Víctimas y tasas del delito seleccionado a<br>nivel jurisdiccional. Años 2017-2022", x = 0.5, y= 0.95, font = list(size = 16)),
                 yaxis = list(side = "right", title = "Víctimas", font = list(color = "grey"), titlefont = list(color = "grey"), 
                              range = c(0, max(df_filtrado$victimas) * 1.05), showgrid = FALSE),
                 yaxis2 = list(side = "left", title = "Tasa de víctimas", overlaying = "y", side = "left", 
                               tickfont = list(color = "grey"), titlefont = list(color = "grey"), 
                               range = c(0, max(df_filtrado$tasa_victimas) * 1.1), showgrid = FALSE),
                 xaxis = list(title = list(text = "Año", standoff = 10), font = list(color = "grey")),  # Adjust the standoff value here
                 titlefont = list(color = "grey"),
                 legend = list(x = 0.5, y = -0.15, orientation = "h", xanchor = "center",
                               itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                 font = list(color = "grey"),
                 autosize = "responsive", margin = list(t = 60)) %>%
          config(displayModeBar = FALSE)%>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
      }
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2]%in% valid_codes2) {
      df_filtrado <- subset(datos_provinciales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo2] & provincia_nombre %in% input$provincia)
      colores <- c("#47baec")
      
      if (all(df_filtrado$tasa_hechos == 0 & df_filtrado$hechos == 0)) {
        plot_ly(df_filtrado, 
                type = "bar", 
                x = ~anio,
                y = ~hechos, 
                marker = list(color = colores),
                textposition = "auto", 
                name = "Hechos",
                orientation = "v",
                autosize = "responsive")%>% 
          add_trace(y = ~tasa_hechos, name = "Tasa cada\n100.000 hab.", type = "scatter", marker = list(color = "#0A72B9", size = 10),
                    mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9"),autosize = "responsive") %>% 
          layout(title = list(text = "Hechos y tasas del delito seleccionado a<br>nivel jurisdiccional. Años 2017-2022", x = 0.5, y= 0.95, font = list(size = 16)),
                 yaxis = list(side = "right", title = "Víctimas", font = list(color = "grey"), titlefont = list(color = "grey"), 
                              range = c(-0.5, max(df_filtrado$hechos)+10), showgrid = FALSE),
                 yaxis2 = list(side = "left", title = "Tasa de hechos", overlaying = "y", side = "left", 
                               tickfont = list(color = "grey"), titlefont = list(color = "grey"), 
                               range = c(-0.5, max(df_filtrado$tasa_hechos)+10), showgrid = FALSE),
                 xaxis = list(title = list(text = "Año", standoff = 10), font = list(color = "grey")),  # Adjust the standoff value here
                 titlefont = list(color = "grey"),
                 legend = list(x = 0.5, y = -0.15, orientation = "h", xanchor = "center",
                               itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                 font = list(color = "grey"),
                 autosize = "responsive", margin = list(t = 60))%>%
          config(displayModeBar = FALSE)%>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
      } else {
        plot_ly(df_filtrado, 
                type = "bar", 
                x = ~anio,
                y = ~hechos, 
                marker = list(color = colores),
                textposition = "auto", 
                name = "Hechos",
                orientation = "v",
                autosize = "responsive")%>% 
          add_trace(y = ~tasa_hechos, name = "Tasa cada\n100.000 hab.", type = "scatter", marker = list(color = "#0A72B9", size = 10),
                    mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9"),autosize = "responsive") %>% 
          layout(title = list(text = "Hechos y tasas del delito seleccionado a<br>nivel jurisdiccional. Años 2017-2022", x = 0.5, y= 0.95, font = list(size = 16)),
                 yaxis = list(side = "right", title = "Hechos", font = list(color = "grey"), titlefont = list(color = "grey"), 
                              range = c(0, max(df_filtrado$hechos)*1.05), showgrid = FALSE),
                 yaxis2 = list(side = "left", title = "Tasa de hechos", overlaying = "y", side = "left", 
                               tickfont = list(color = "grey"), titlefont = list(color = "grey"), 
                               range = c(0, max(df_filtrado$tasa_hechos)*1.1), showgrid = FALSE),
                 xaxis = list(title = list(text = "Año", standoff = 10), font = list(color = "grey")),  # Adjust the standoff value here
                 titlefont = list(color = "grey"),
                 legend = list(x = 0.5, y = -0.15, orientation = "h", xanchor = "center",
                               itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                 font = list(color = "grey"),
                 autosize = "responsive", margin = list(t = 60))%>%
          config(displayModeBar = FALSE)%>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
      }
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  
  
  get_number_from_string <- function(str) {
    num <- gsub("[^0-9]", "", str)
    as.numeric(num)
  }
  
  observe({
    # Obtener un índice aleatorio del vector de opciones de provincias
    rand_prov <- sample(length(unique(na.omit(datos_departamentales$provincia_nombre))), 1)
    # Asignar el valor de la provincia seleccionada aleatoriamente
    updateSelectInput(session, "provincia2", selected = unique(na.omit(datos_departamentales$provincia_nombre))[rand_prov])
  })
  
  observeEvent(input$provincia2, {
    # Filtrar los departamentos disponibles para la provincia seleccionada
    departamentos_provincia <- unique(na.omit(datos_departamentales[datos_departamentales$provincia_nombre == input$provincia2, ]$departamento))
    # Ordenar alfabéticamente los departamentos
    departamentos_provincia_sorted <- sort(departamentos_provincia)
    
    # Extraer las opciones que comienzan con "Comuna"
    comunas <- departamentos_provincia_sorted[grep("^Comuna\\s\\d+", departamentos_provincia_sorted)]
    
    # Ordenar numéricamente las opciones que comienzan con "Comuna"
    comunas_numericamente <- comunas[order(sapply(comunas, get_number_from_string))]
    
    # Reemplazar las opciones que comienzan con "Comuna" en departamentos_provincia_sorted
    departamentos_provincia_sorted[grep("^Comuna\\s\\d+", departamentos_provincia_sorted)] <- comunas_numericamente
    
    # Actualizar las opciones del menú desplegable de departamentos
    updateSelectInput(session, "departamento", choices = departamentos_provincia_sorted)
    
    # Seleccionar un departamento aleatorio de la provincia seleccionada
    rand_dept <- sample(length(departamentos_provincia_sorted), 1)
    # Asignar el valor del departamento seleccionado aleatoriamente
    updateSelectInput(session, "departamento", selected = departamentos_provincia_sorted[rand_dept])
  })
  
  
  output$mapa_dpto <- renderLeaflet({
    
    datos_geo_dpto <- geo_departamentos
    datos_dpto <- subset(datos_departamentales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3]& anio %in% c(input$año3))
    datos_dpto2 <- left_join(datos_geo_dpto, datos_dpto, by = c("link" = "Cod_INDEC"))%>%
      mutate(departamen = departamento)
    
    datos_dpto2$poblacion <- format(datos_dpto2$poblacion, big.mark = ".")
    datos_dpto2$tasa_victimas <- format(round(datos_dpto2$tasa_victimas, 1), decimal.mark = ",", big.mark = ".")
    datos_dpto2$tasa_hechos <- format(round(datos_dpto2$tasa_hechos, 1), decimal.mark = ",", big.mark = ".")
    datos_dpto2$victimas <- format(datos_dpto2$victimas, big.mark = ".")
    datos_dpto2$hechos <- format(datos_dpto2$hechos, big.mark = ".")
    
    datos_dpto2 <- datos_dpto2%>%
      mutate(
        color_prov = ifelse(is.na(color_prov), "#d9d9d9", color_prov),
        victimas = ifelse(grepl("\\bNA\\b", datos_dpto2$victimas), "Sin datos", victimas),
        hechos = ifelse(grepl("\\bNA\\b", datos_dpto2$hechos), "Sin datos", hechos),
        tasa_victimas = ifelse(grepl("\\bNA\\b", datos_dpto2$tasa_victimas), "Sin datos", tasa_victimas),
        tasa_hechos = ifelse(grepl("\\bNA\\b", datos_dpto2$tasa_hechos), "Sin datos", tasa_hechos),
        provincia_nombre = ifelse(is.na(provincia_nombre), "Tierra del Fuego", provincia_nombre),
        poblacion = ifelse(grepl("\\bNA\\b", datos_dpto2$poblacion), "Sin datos", poblacion),
        departamen = ifelse(is.na(departamen), "Antártida e Islas del Atlántico Sur", departamen),
        departamen = ifelse(departamen == "Antártida Argentina", "Antártida e Islas del Atlántico Sur", departamen),
        departamen = ifelse(departamen == "Islas del Atlántico Sur", "Antártida e Islas del Atlántico Sur", departamen),
        departamen = ifelse(departamen == "9 de julio", "9 de Julio", departamen)
      )
    
    datos_dpto2 <- subset(datos_dpto2, provincia_nombre %in% c(input$provincia2))
    poligono_departamental <- datos_dpto2 %>% filter(departamento == input$departamento)
    departamento_bounds <- st_bbox(poligono_departamental)
    departamento_bounds[c("xmin", "ymin", "xmax", "ymax")] <- lapply(departamento_bounds[c("xmin", "ymin", "xmax", "ymax")], as.numeric)
    
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes){
      if (input$provincia2 == "Tierra del Fuego") {
        if (input$departamento == "Dpto. sin determinar") {
          mapa<- leaflet() %>%
            addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
            addPolygons(data = datos_dpto2, 
                        color = ~color_prov,
                        weight = 0,
                        fillOpacity = 1)%>%
            addPolygons(data = datos_dpto2, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = lapply(1:nrow(datos_dpto2), function(i) {
                          label <- paste0('<span style="font-size: 12px;">',
                                          "Departamento: ", datos_dpto2$departamen[i], "<br>",
                                          "Víctimas: ", datos_dpto2$victimas[i], "<br>",
                                          "Tasa: ", datos_dpto2$tasa_victimas[i], "<br>",
                                          "Población: ", datos_dpto2$poblacion[i])
                          return(htmltools::HTML(label))}))%>%
            setView(lng=-66.478801, lat=-54.057724, zoom=7)
        } else{
          mapa<- leaflet() %>%
            addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
            addPolygons(data = datos_dpto2, 
                        color = ~color_prov,
                        weight = 0,
                        fillOpacity = 1)%>%
            addPolygons(data = poligono_departamental, 
                        color = "red",
                        fillColor= "transparent",
                        weight = 3,
                        fillOpacity = 1)%>%
            addPolygons(data = datos_dpto2, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = lapply(1:nrow(datos_dpto2), function(i) {
                          label <- paste0('<span style="font-size: 12px;">',
                                          "Departamento: ", datos_dpto2$departamen[i], "<br>",
                                          "Víctimas: ", datos_dpto2$victimas[i], "<br>",
                                          "Tasa: ", datos_dpto2$tasa_victimas[i], "<br>",
                                          "Población: ", datos_dpto2$poblacion[i])
                          return(htmltools::HTML(label))}))%>%
            setView(lng=-66.478801, lat=-54.057724, zoom=7)
        }
      } else{
        if (input$departamento == "Dpto. sin determinar") {
          mapa<- leaflet() %>%
            addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
            addPolygons(data = datos_dpto2, 
                        color = ~color_prov,
                        weight = 0,
                        fillOpacity = 1)%>%
            addPolygons(data = datos_dpto2, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = lapply(1:nrow(datos_dpto2), function(i) {
                          label <- paste0('<span style="font-size: 12px;">',
                                          "Departamento: ", datos_dpto2$departamen[i], "<br>",
                                          "Víctimas: ", datos_dpto2$victimas[i], "<br>",
                                          "Tasa: ", datos_dpto2$tasa_victimas[i], "<br>",
                                          "Población: ", datos_dpto2$poblacion[i])
                          return(htmltools::HTML(label))}))
        } else{
          
          mapa<- leaflet() %>%
            addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
            addPolygons(data = datos_dpto2, 
                        color = ~color_prov,
                        weight = 0,
                        fillOpacity = 1)%>%
            addPolygons(data = poligono_departamental, 
                        color = "red",
                        fillColor= "transparent",
                        weight = 3,
                        fillOpacity = 1)%>%
            addPolygons(data = datos_dpto2, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = lapply(1:nrow(datos_dpto2), function(i) {
                          label <- paste0('<span style="font-size: 12px;">',
                                          "Departamento: ", datos_dpto2$departamen[i], "<br>",
                                          "Víctimas: ", datos_dpto2$victimas[i], "<br>",
                                          "Tasa: ", datos_dpto2$tasa_victimas[i], "<br>",
                                          "Población: ", datos_dpto2$poblacion[i])
                          return(htmltools::HTML(label))}))
        }
      }
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes2){
      if (input$provincia2 == "Tierra del Fuego") {
        if (input$departamento == "Dpto. sin determinar") {
          mapa<- leaflet() %>%
            addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
            addPolygons(data = datos_dpto2, 
                        color = ~color_prov,
                        weight = 0,
                        fillOpacity = 1)%>%
            addPolygons(data = datos_dpto2, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = lapply(1:nrow(datos_dpto2), function(i) {
                          label <- paste0('<span style="font-size: 12px;">',
                                          "Departamento: ", datos_dpto2$departamen[i], "<br>",
                                          "Hechos: ", datos_dpto2$hechos[i], "<br>",
                                          "Tasa: ", datos_dpto2$tasa_hechos[i], "<br>",
                                          "Población: ", datos_dpto2$poblacion[i])
                          return(htmltools::HTML(label))}))%>%
            setView(lng=-66.478801, lat=-54.057724, zoom=7)
        } else{
          
          mapa<- leaflet() %>%
            addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
            addPolygons(data = datos_dpto2, 
                        color = ~color_prov,
                        weight = 0,
                        fillOpacity = 1)%>%
            addPolygons(data = poligono_departamental, 
                        color = "red",
                        fillColor= "transparent",
                        weight = 3,
                        fillOpacity = 1)%>%
            addPolygons(data = datos_dpto2, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = lapply(1:nrow(datos_dpto2), function(i) {
                          label <- paste0('<span style="font-size: 12px;">',
                                          "Departamento: ", datos_dpto2$departamen[i], "<br>",
                                          "Hechos: ", datos_dpto2$hechos[i], "<br>",
                                          "Tasa: ", datos_dpto2$tasa_hechos[i], "<br>",
                                          "Población: ", datos_dpto2$poblacion[i])
                          return(htmltools::HTML(label))}))%>%
            setView(lng=-66.478801, lat=-54.057724, zoom=7)
        }
      } else{
        if (input$departamento == "Dpto. sin determinar") {
          mapa<- leaflet() %>%
            addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
            addPolygons(data = datos_dpto2, 
                        color = ~color_prov,
                        weight = 0,
                        fillOpacity = 1)%>%
            addPolygons(data = datos_dpto2, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = lapply(1:nrow(datos_dpto2), function(i) {
                          label <- paste0('<span style="font-size: 12px;">',
                                          "Departamento: ", datos_dpto2$departamen[i], "<br>",
                                          "Hechos: ", datos_dpto2$hechos[i], "<br>",
                                          "Tasa: ", datos_dpto2$tasa_hechos[i], "<br>",
                                          "Población: ", datos_dpto2$poblacion[i])
                          return(htmltools::HTML(label))}))
        } else{
          
          mapa<- leaflet() %>%
            addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")%>%
            addPolygons(data = datos_dpto2, 
                        color = ~color_prov,
                        weight = 0,
                        fillOpacity = 1)%>%
            addPolygons(data = poligono_departamental, 
                        color = "red",
                        fillColor= "transparent",
                        weight = 3,
                        fillOpacity = 1)%>%
            addPolygons(data = datos_dpto2, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
                        label = lapply(1:nrow(datos_dpto2), function(i) {
                          label <- paste0('<span style="font-size: 12px;">',
                                          "Departamento: ", datos_dpto2$departamen[i], "<br>",
                                          "Hechos: ", datos_dpto2$hechos[i], "<br>",
                                          "Tasa: ", datos_dpto2$tasa_hechos[i], "<br>",
                                          "Población: ", datos_dpto2$poblacion[i])
                          return(htmltools::HTML(label))}))
        }
      }
    } else{
      print("El valor ingresado no corresponde a un delito.")
    }
    
    mapa <- mapa %>%
      setMaxBounds(
        lng1 = departamento_bounds$xmin,
        lng2 = departamento_bounds$xmax,
        lat1 = departamento_bounds$ymin,
        lat2 = departamento_bounds$ymax
      )
    mapa
  })
  
  output$grafico_total_tasa_dpto <- renderPlotly({
    datos_grafico_dpto<- subset(datos_departamentales, provincia_nombre %in% c(input$provincia2) & codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] & departamento %in% c(input$departamento))
    colores <- c("#47baec")
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes) {
      if (input$departamento == "Dpto. sin determinar") {
        if (all(datos_grafico_dpto$victimas == 0)) {
          plot_ly(datos_grafico_dpto, 
                  type = "bar", 
                  x = ~anio,
                  y = ~victimas,
                  name= "Víctimas",
                  marker = list(color = colores),
                  textposition = "auto", 
                  orientation = "v")%>%
            layout(title = list(text = "Víctimas con departamento sin determinar.<br>Años 2017-2022", x = 0.5, font = list(size = 14)),
                   yaxis = list(side = "right", title = "Víctimas", font = list(color = "grey"), 
                                titlefont = list(color = "grey"), range = c(0, max(datos_grafico_dpto$victimas)+10), 
                                showgrid = FALSE),
                   xaxis = list(title = "Año", font = list(color = "grey")),
                   titlefont = list(color = "grey"),
                   legend = list( x = 0.5, y = -0.2, orientation = "h", xanchor = "center",
                                  itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                   font = list(color = "grey"),
                   autosize = TRUE)%>%
            config(displayModeBar = FALSE)%>%
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
          
        } else {
          plot_ly(datos_grafico_dpto, 
                  type = "bar", 
                  x = ~anio,
                  y = ~victimas, 
                  marker = list(color = colores),
                  textposition = "auto", 
                  name = "Víctimas",
                  orientation = "v") %>%
            layout(title = list(text = "Víctimas con departamento sin determinar.<br>Años 2017-2022", x = 0.5, font = list(size = 14)),
                   yaxis = list(side = "right", title = "Víctimas", font = list(color = "grey"), titlefont = list(color = "grey"), 
                                range = c(0, max(datos_grafico_dpto$victimas)*1.05), showgrid = FALSE),
                   xaxis = list(title = "Año", font = list(color = "grey")),
                   titlefont = list(color = "grey"),
                   legend = list( x = 0.5, y = -0.2, orientation = "h", xanchor = "center",
                                  itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                   font = list(color = "grey"),
                   autosize = TRUE) %>%
            config(displayModeBar = FALSE)%>%
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
          
        }
      } else if (all(datos_grafico_dpto$tasa_victimas == 0 & datos_grafico_dpto$victimas == 0)) {
        plot_ly(datos_grafico_dpto, 
                type = "bar", 
                x = ~anio,
                y = ~victimas, 
                marker = list(color = colores),
                textposition = "auto", 
                name = "Víctimas",
                orientation = "v")%>% 
          add_trace(y = ~tasa_victimas, name = "Tasa cada\n100.000 hab.", type = "scatter",marker = list(color = "#0A72B9", size = 10), mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9"))%>% 
          layout(title = list(text = "Víctimas y tasas del departamento<br>seleccionado. Años 2017-2022", x = 0.5, font = list(size = 14)),
                 yaxis = list(side = "right", title = "Víctimas", font = list(color = "grey"), titlefont = list(color = "grey"), 
                              range = c(-0.5, max(datos_grafico_dpto$victimas)+10), showgrid = FALSE),
                 yaxis2 = list(side = "left", title = "Tasa de víctimas", overlaying = "y", side = "left", 
                               tickfont = list(color = "grey"), titlefont = list(color = "grey"), 
                               range = c(-0.5, max(datos_grafico_dpto$tasa_victimas)+10), showgrid = FALSE),
                 #range = c(0, max(datos_grafico_dpto$tasa_victimas)*1.1), showgrid = FALSE),
                 xaxis = list(title = "Año", font = list(color = "grey")),
                 titlefont = list(color = "grey"),
                 legend = list( x = 0.5, y = -0.2, orientation = "h", xanchor = "center",
                                itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                 font = list(color = "grey"),
                 autosize = TRUE)%>%
          config(displayModeBar = FALSE)%>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
        
      } else {
        plot_ly(datos_grafico_dpto, 
                type = "bar", 
                x = ~anio,
                y = ~victimas, 
                marker = list(color = colores),
                textposition = "auto", 
                name = "Víctimas",
                orientation = "v")%>% 
          add_trace(y = ~tasa_victimas, name = "Tasa cada\n100.000 hab.", type = "scatter",marker = list(color = "#0A72B9", size = 10), mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9"))%>% 
          layout(title = list(text = "Víctimas y tasas del departamento<br>seleccionado. Años 2017-2022", x = 0.5, font = list(size = 14)),
                 yaxis = list(side = "right", title = "Víctimas", font = list(color = "grey"), titlefont = list(color = "grey"), 
                              range = c(0, max(datos_grafico_dpto$victimas)*1.05), showgrid = FALSE),
                 yaxis2 = list(side = "left", title = "Tasa de víctimas", overlaying = "y", side = "left", 
                               tickfont = list(color = "grey"), titlefont = list(color = "grey"), 
                               range = c(0, max(datos_grafico_dpto$tasa_victimas)*1.1), showgrid = FALSE),
                 #range = c(0, max(datos_grafico_dpto$tasa_victimas)*1.1), showgrid = FALSE),
                 xaxis = list(title = "Año", font = list(color = "grey")),
                 titlefont = list(color = "grey"),
                 legend = list( x = 0.5, y = -0.2, orientation = "h", xanchor = "center",
                                itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                 font = list(color = "grey"),
                 autosize = TRUE) %>%
          config(displayModeBar = FALSE)%>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
        
      }
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes2) {
      if (input$departamento == "Dpto. sin determinar") {
        if (all(datos_grafico_dpto$hechos == 0)){
          plot_ly(datos_grafico_dpto, 
                  type = "bar", 
                  x = ~anio,
                  y = ~hechos,
                  name= "Hechos",
                  marker = list(color = colores),
                  textposition = "auto", 
                  orientation = "v")%>%
            layout(title = list(text ="Hechos con departamento sin determinar.<br>Años 2017-2022", x = 0.5, font = list(size = 14)),
                   yaxis = list(side = "right", title = "Hechos", font = list(color = "grey"), 
                                titlefont = list(color = "grey"), range = c(0, max(datos_grafico_dpto$hechos)+10), 
                                showgrid = FALSE),
                   xaxis = list(title = "Año", font = list(color = "grey")),
                   titlefont = list(color = "grey"),
                   legend = list( x = 0.5, y = -0.2, orientation = "h", xanchor = "center",
                                  itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                   font = list(color = "grey"),
                   autosize = TRUE)%>%
            config(displayModeBar = FALSE)%>%
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
        } else {
          plot_ly(datos_grafico_dpto, 
                  type = "bar", 
                  x = ~anio,
                  y = ~hechos, 
                  marker = list(color = colores),
                  textposition = "auto", 
                  name = "Hechos",
                  orientation = "v") %>%
            layout(title = list(text ="Hechos con departamento sin determinar.<br>Años 2017-2022", x = 0.5, font = list(size = 14)),
                   yaxis = list(side = "right", title = "Hechos", font = list(color = "grey"), titlefont = list(color = "grey"), 
                                range = c(0, max(datos_grafico_dpto$hechos)*1.05), showgrid = FALSE),
                   xaxis = list(title = "Año", font = list(color = "grey")),
                   titlefont = list(color = "grey"),
                   legend = list( x = 0.5, y = -0.2, orientation = "h", xanchor = "center",
                                  itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                   font = list(color = "grey"),
                   autosize = TRUE) %>%
            config(displayModeBar = FALSE)%>%
            layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
        }
      } else if (all(datos_grafico_dpto$tasa_hechos == 0 & datos_grafico_dpto$hechos == 0)) {
        plot_ly(datos_grafico_dpto, 
                type = "bar", 
                x = ~anio,
                y = ~hechos, 
                marker = list(color = colores),
                textposition = "auto", 
                name = "Hechos",
                orientation = "v")%>% 
          add_trace(y = ~tasa_hechos, name = "Tasa cada\n100.000 hab.", type = "scatter",marker = list(color = "#0A72B9", size = 10), mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9"))%>% 
          layout(title = list(text = "Hechos y tasas del departamento<br>seleccionado. Años 2017-2022", x = 0.5, font = list(size = 14)), 
                 yaxis = list(side = "right", title = "Hechos", font = list(color = "grey"), titlefont = list(color = "grey"), 
                              range = c(-0.5, max(datos_grafico_dpto$hechos)+10), showgrid = FALSE),
                 yaxis2 = list(side = "left", title = "Tasa de hechos", font = list(color = "grey"), titlefont = list(color = "grey"), 
                               range = c(-0.5, max(datos_grafico_dpto$tasa_hechos)+10), showgrid = FALSE),
                 #range = c(0, max(datos_grafico_dpto$tasa_victimas)*1.1), showgrid = FALSE),
                 xaxis = list(title = "Año", font = list(color = "grey")),
                 titlefont = list(color = "grey"),
                 legend = list( x = 0.5, y = -0.2, orientation = "h", xanchor = "center",
                                itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                 font = list(color = "grey"),
                 autosize = TRUE)%>%
          config(displayModeBar = FALSE)%>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
      } else {
        plot_ly(datos_grafico_dpto, 
                type = "bar", 
                x = ~anio,
                y = ~hechos, 
                marker = list(color = colores),
                textposition = "auto", 
                name = "Hechos",
                orientation = "v")%>% 
          add_trace(y = ~tasa_hechos, name = "Tasa cada\n100.000 hab.", type = "scatter",marker = list(color = "#0A72B9", size = 10), mode = "lines+markers", yaxis = "y2", line = list(shape = 'spline', smoothing = 0.8, color = "#0A72B9"))%>% 
          layout(title = list(text = "Hechos y tasas del departamento<br>seleccionado. Años 2017-2022", x = 0.5, font = list(size = 14)), 
                 yaxis = list(side = "right", title = "Hechos", font = list(color = "grey"), titlefont = list(color = "grey"), 
                              range = c(0, max(datos_grafico_dpto$hechos)*1.05), showgrid = FALSE),
                 yaxis2 = list(side = "left", title = "Tasa de hechos", overlaying = "y", side = "left", 
                               tickfont = list(color = "grey"), titlefont = list(color = "grey"), 
                               range = c(0, max(datos_grafico_dpto$tasa_hechos)*1.1), showgrid = FALSE),
                 xaxis = list(title = "Año", font = list(color = "grey")),
                 titlefont = list(color = "grey"),
                 legend = list( x = 0.5, y = -0.2, orientation = "h", xanchor = "center",
                                itemsizing = "constant", bgcolor = "#f5f5f5", bordercolor = "#CCCCCC", borderwidth = 1),
                 font = list(color = "grey"),
                 autosize = TRUE)%>%
          config(displayModeBar = FALSE)%>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE))
      }
    }
  })
  
  
  output$tasa_nacional_dpto <- renderText({
    tasa_nac <- subset(datos_nacionales, codigo_delito_snic_id %in% c(codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3]) & anio %in% c(input$año3))
    tasa_nac$tasa_hechos <- round(tasa_nac$tasa_hechos, 1)  
    tasa_nac$tasa_victi<- round(tasa_nac$tasa_victi, 1)
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes) {
      format(tasa_nac$tasa_victi, big.mark = ".", decimal.mark = ",", nsmall = 1)
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes2) {
      format(tasa_nac$tasa_hechos, big.mark = ".", decimal.mark = ",", nsmall = 1)
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }   
  })
  
  
  output$tasa_jurisdiccional_dpto <- renderText({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes) {
      datos_provinciales$tasa_victimas<- round(datos_provinciales$tasa_victimas, 1)
      tasa_jur <- subset(datos_provinciales, codigo %in%codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3]& anio %in% c(input$año3) & provincia_nombre %in% c(input$provincia2))
      format(tasa_jur$tasa_victimas, big.mark = ".", decimal.mark = ",", nsmall = 1)
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes2) {
      datos_provinciales$tasa_hechos<- round(datos_provinciales$tasa_hechos, 1)
      tasa_jur <- subset(datos_provinciales, codigo %in%codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3]& anio %in% c(input$año3) & provincia_nombre %in% c(input$provincia2))
      format(tasa_jur$tasa_hechos, big.mark = ".", decimal.mark = ",", nsmall = 1)
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  
  output$tasa_dpto_dpto <- renderText({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes) {
      datos_departamentales$tasa_victimas<- round(datos_departamentales$tasa_victimas, 1)
      tasa_dpto<- subset(datos_departamentales, codigo %in%codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3]& anio %in% c(input$año3) & provincia_nombre %in% c(input$provincia2) & departamento %in% c(input$departamento))
      format(tasa_dpto$tasa_victimas, big.mark = ".", decimal.mark = ",", nsmall = 1)
      tasa_dpto$tasa_victimas <- ifelse(is.na(tasa_dpto$tasa_victimas), "-", format(tasa_dpto$tasa_victimas, big.mark = ".", decimal.mark = ",", nsmall = 1))
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] %in% valid_codes2) {
      datos_departamentales$tasa_hechos <- round(datos_departamentales$tasa_hechos, 1)
      tasa_dpto <- subset(datos_departamentales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo3] & anio %in% c(input$año3) & provincia_nombre %in% c(input$provincia2) & departamento %in% c(input$departamento))
      format(tasa_dpto$tasa_hechos, big.mark = ".", decimal.mark = ",", nsmall = 1)
      tasa_dpto$tasa_hechos <- ifelse(is.na(tasa_dpto$tasa_hechos), "-", format(tasa_dpto$tasa_hechos, big.mark = ".", decimal.mark = ",", nsmall = 1))
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
}

shinyApp(ui, server)

  
  

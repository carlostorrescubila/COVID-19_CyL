##### > libraries ##########################################################################

source("./Code/libraries.r", encoding = "UTF-8")

##### > Read data ##########################################################################

source("./Code/read_data.r", encoding = "UTF-8") 

##### > Interfáz ###########################################################################

##### >> Header ############################################################################

header <- dashboardHeader(
  title = tagList(
    tags$span(class = "logo-mini", ""),
    tags$span(class = "logo-lg", "Covid-19 en Castilla y León")
    ), 
  titleWidth = 275,
  dropdownMenuOutput(outputId = "messageMenu")
)
  
##### >> Sidebar ###########################################################################

sidebar <- dashboardSidebar(
  disable = TRUE,
  width = 275,
  collapsed = TRUE, 
  sidebarMenu(
    menuItem("Actualidad", tabName = "actualidad", icon = icon("calendar-day")), 
    menuItem("Datos", tabName = "data", icon = icon("database")),
    menuItem("Mapa", tabName = "map", icon = icon("map-marked-alt")),
    menuItem(
      "Análisis", 
      tabName = "analysis",
      icon = icon("chart-line"), 
      names(Bases_de_Datos) %>%
        map(
          function(x){
            menuSubItem(
              as.character(x),
              tabName = str_to_lower(x) %>%
                str_replace(" ", "_") %>%
                as.character %>%
                paste0("analysis_", .)
            )
            }
          )
      )
  )
) 

##### >> Body ##################################################################################

body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$style(".selectize-dropdown {position: static}")
  ),
  tabItems(
    
    ##### >>> Actualidad #######################################################################
    
    tabItem(
      tabName = "actualidad", 
      fluidPage(
        h1("Situación actual del COVID-19 en Castilla y León", class = "text-center"), 
        h2(
          paste("hasta el ", Situacion_epidemiologica$fecha %>% unique %>% last), 
          class = "text-center"
          ), 
        br(),
        fluidRow(
          valueBoxOutput("actualidad_casos_confirmados", width = 3),
          valueBoxOutput("actualidad_nuevos_positivos", width = 3),
          valueBoxOutput("actualidad_altas", width = 3),
          valueBoxOutput("actualidad_fallecimientos", width = 3)
        ),
        plotlyOutput("actualidad_plot")
      )
    ),

    ##### >>> Base de datos ####################################################################
    
    tabItem(
      tabName = "data",
      fluidPage(
        h1("Datos disponibles para visualizar", class = "text-center"),
        tags$p(
          "Los conjuntos de datos disponibles han sido obtenidos del ",
          tags$a(
            href="https://datosabiertos.jcyl.es/web/es/catalogo-datos.html",
            "Portal de Datos Abiertos de la Junta de Castilla y León", 
            style = "color:red"
            ),
          ". Estos datos brindan información acerca de la situación de enfermos por coronavirus
          detectados en atención primaria, la situación epidemiológica en Castilla y León por
          hospitales, la situación de profesionales sanitarios afectados por coronavirus y
          la situación epidemiológica en Castilla y León por provincias.",
          style = "text-align: justify"
          ),
        br(),
        fluidRow(
          column(6, 
                 selectInput(
                  inputId = "select_data_base",
                  label = "Seleccione base de datos:",
                  choices = names(Bases_de_Datos)
                  )
                 ), 
          column(6, 
                 p(
                   class = 'text-right', 
                   br(), 
                   downloadButton("Download_data", "Descargar")
                   )
                 )
        ),
        h3("Descripción:"), 
        htmlOutput("Descripcion_datos"),
        h3("Datos:"),
        column(12, dataTableOutput("Datos"))
      )
    ),
    
    ##### >>> Mapa #############################################################################
    
    tabItem(
      tabName = "map",
      fluidPage(
        
        tags$style(
          type = "text/css", 
          "#map_hospitales {height: calc(100vh - 80px) !important;}
          #map_provincias {height: calc(100vh - 80px) !important;}"
          ),

        conditionalPanel(
          condition = "input.tabs=='Hospitales'",
          leafletOutput("map_hospitales", width="100%", height = "100%")
        ),
        
        conditionalPanel(
          condition = "input.tabs=='Provincias'",
          leafletOutput("map_provincias", width="100%", height = "100%")
        ),
        
        absolutePanel(
          fixed = TRUE,
          draggable = TRUE, 
          top = 70, left = "auto", right = 30, bottom = "auto",
          width = 330, height = "auto",
          id = "tabPanel",
          class = "panel panel-default",
          style = "padding : 10px",
          tabsetPanel(
            id = "tabs", 
            tabPanel(
              "Hospitales",
              selectInput(
                inputId = "map_variable_hospital", 
                label = "Visualizar:",
                c(
                  "Hospitalizados por planta" = "hospitalizados_planta",
                 "Hospitalizados UCI" = "hospitalizados_uci",
                 "Altas" = "altas",
                 "Fallecimientos" = "fallecimientos"
                 )
                )
              ),
            tabPanel(
              "Provincias",
              selectInput(
                inputId = "map_variable_provincias", 
                label = "Visualizar:",
                c(
                  "Casos confirmados" = "casos_confirmados",
                  "Altas" = "altas",
                  "Fallecimientos" = "fallecimientos"
                 )
                )
              )
            )
          )
        )
      ),
    
    ##### >>> Análisis #########################################################################
    
    ##### >>>> Atención primaria ###############################################################
    
    tabItem(
      tabName = "analysis_atención_primaria", 
      h2(
        "Cantidad de enfermos detectados en atención primaria en Castilla y León", 
        class = "text-center"
      ),
      br(), br(),
      fluidRow(
        box(dateInput(
          inputId = "analysis_atención_primaria_date", 
          label = "Seleccione fecha", 
          value = Atencion_primaria$fecha %>% unique() %>% last(), 
          min = Atencion_primaria$fecha %>% unique() %>% first(), 
          max = Atencion_primaria$fecha %>% unique() %>% last(), 
          language = "es"
        )), 
        box(selectInput(
          inputId = "analysis_atención_primaria_legend", 
          label = "Seleccione variable para la leyenda", 
          choices = c(
            "Edad" = "grupo_edad", 
            "Sexo" = "sexo"
            )
        ))
      ), 
     plotlyOutput("analysis_atención_primaria_plot")
    ), 
    
    ##### >>>> Hospitales ######################################################################
    
    tabItem(
      tabName = "analysis_hospitales", 
      h2(
        "Situación epidemiológica en los distintos hospitales de Castilla y León", 
        class = "text-center"
      ),
      br(), br(),
      fluidPage(
        fluidRow(
          box(width = 4,
            selectInput(
              inputId = "analysis_hospitales_select_provincia",
              label = "Seleccione provincia(s): ",
              choices = unique(Situacion_epidemiologica$provincia) %>% str_sort(), 
              multiple = TRUE, 
              selected = unique(Situacion_epidemiologica$provincia) %>% str_sort()
              ), 
            selectInput(
              inputId = "analysis_hospitales_select_data",
              label = "Seleccione datos: ", 
              choices = c(
                # "Nuevos hospitalizados por planta" = "nuevos_hospitalizados_planta",
                "Hospitalizados por planta" = "hospitalizados_planta", 
                # "Nuevos hospitalizados UCI" = "nuevos_hospitalizados_uci",  
                "Hospitalizados UCI" = "hospitalizados_uci",            
                # "Nuevas altas" = "nuevas_altas", 
                "Altas" = "altas",
                # "Nuevos fallecimientos" = "nuevos_fallecimientos", 
                "Fallecimientos" = "fallecimientos"
                )
            )
          ),
          box(width = 8,
            selectInput(
              inputId = "analysis_hospitales_select_hospital",
              label = "Seleccione hospital(es):",
              choices = NULL, 
              multiple = TRUE
            )
          )
        ), 
        column(
          12,
          box(width = 12, plotlyOutput("analysis_hospitales_plot"))
        )
      )
    ),
    
    ##### >>>> Situación epidemiológica ########################################################
    
    tabItem(
      tabName = "analysis_situacion_epidemiologica",
      fluidPage(
        h2(
          "Situación epidemiológica en las distintas provincias de Castilla y León", 
          class = "text-center"
        ),
        br(), br(), 
        fluidRow(
          box(selectInput(
            inputId = "analysis_situacion_epidemiologica_select_data",
            label = "Seleccione datos",
            choices = c(
              "Casos confirmados" = "casos_confirmados",
              "Nuevos positivos" = "nuevos_positivos",
              "Altas" = "altas",
              "Fallecimientos" = "fallecimientos"
            )
          )),
          box(selectInput(
            inputId = "analysis_situacion_epidemiologica_provincias",
            label = "Seleccione provincia(s):",
            choices = unique(Situacion_epidemiologica$provincia) %>% str_sort(),
            multiple = TRUE
          ))
        ),
        column(
          12,
          box(width = 12, plotlyOutput("Line_plot"))
        )
      )
    )
  )
)

##### > UI ####################################################################################

ui <- div(
  dashboardPagePlus(
    title = "COVID-19 CyL",
    skin = "red",
    header,
    sidebar,
    body,
    useShinyalert()
  )
)


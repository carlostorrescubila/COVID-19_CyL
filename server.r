##### > libraries ####################################################################################

source("./Code/libraries.r", encoding = "UTF-8")

##### > Read data ####################################################################################

source("./Code/read_data.r", encoding = "UTF-8")

##### > Server #######################################################################################

shinyServer(function(input, output, session) {
  
  showModal(
    shinyalert(
      title = "Bienvenido",
      text = tagList(
        tags$p(
          "Esta aplicación ha sido creada para el IV Concurso de Datos Abiertos de Castilla y León 
          con el objetivo de brindar una herramienta que ayude a visualizar el progreso sanitario 
          de la crisis del coronavirus en Castilla y León.", 
          style = "text-align: justify"
          ),
        br(),
        tags$img(src = "https://www.jcyl.es/futuretense_cs/JCYL_17/img/junta-de-castilla-y-leon.png")
        ),
      size = "s", 
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = T,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      imageWidth = 100,
      imageHeight = 100,
      animation = TRUE
    )
  )

  shinyjs::addClass(selector = "header", class = "absolute")
  
  onevent(
    event = "mouseenter", 
    id = "sidebarCollapsed", 
    expr = shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse")
    )
  onevent(
    event = "mouseleave", 
    id = "sidebarCollapsed", 
    expr = shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
    )
  
  ##### >> Right menu ###############################################################################
  
  icon_github <- icon("github")
  icon_github[["attribs"]][["class"]] <- "fa fa-github"
  output$messageMenu <- renderMenu({
    dropdownMenu(
      icon = icon("info-circle"), 
      headerText = strong("Información de la aplicación:"),
      messageItem(
        from = "Repositorio en GitHub",
        message = "Documentación, código, citas",
        icon = icon_github,
        href = "https://github.com/carlostorrescubila/COVID-19_CyL"
      ),
      messageItem(
        from = "Ultima actualización:",
        message = Situacion_epidemiologica$fecha %>% unique() %>% last(),
        icon = icon("calendar-check")
      ),
      messageItem(
        from = "Problemas",
        message = "Reportar problemas",
        icon = icon("database"),
        href = "https://github.com/carlostorrescubila/COVID-19_CyL/issues"
      )
    )
  })
 
  ##### >> Actualidad ###############################################################################
  
  output$actualidad_casos_confirmados <- renderValueBox({
    valueBox(
      subtitle = actionButton(
        inputId = "actualidad_casos_confirmados_button",
        label = "Casos confirmados"
      ), 
      value = Situacion_epidemiologica %>% 
        filter(fecha == .$fecha %>% unique %>% last) %>%
        .$casos_confirmados %>% sum,
      icon = icon("viruses"), 
      color = "red"
    )
  })
  
  output$actualidad_nuevos_positivos <- renderValueBox({
    shinydashboard::valueBox(
      subtitle = actionButton(
          inputId = "actualidad_nuevos_positivos_button",
          label = div("Nuevos positivos")
          ), 
      value = Situacion_epidemiologica %>% 
        filter(fecha == .$fecha %>% unique %>% last) %>%
        .$nuevos_positivos %>% sum, 
      icon = icon("virus"), 
      color = "orange"
    )
  })
  
  output$actualidad_altas <- renderValueBox({
    shinydashboard::valueBox(
      subtitle = actionButton(
        inputId = "actualidad_altas_button",
        label = "Altas"
      ),
      value = Situacion_epidemiologica %>%
        filter(fecha == .$fecha %>% unique %>% last) %>%
        .$altas %>% sum, 
      icon = icon("plus-square"), 
      color = "green"
      )
  })
  
  output$actualidad_fallecimientos <- renderValueBox({
    shinydashboard::valueBox(
      subtitle = actionButton(
        inputId = "actualidad_fallecimientos_button",
        label = "Fallecimientos"
      ),
      value = Situacion_epidemiologica %>% 
        filter(fecha == .$fecha %>% unique %>% last) %>%
        .$fallecimientos %>% sum, 
      icon = icon("cross"), 
      color = "purple"
      )
  })
  
  rv <- reactiveValues(actualidad_variable = "casos_confirmados", actualidad_color = "red2")

  observeEvent(input$actualidad_casos_confirmados_button, {
    rv$actualidad_variable <- "casos_confirmados"
    rv$actualidad_color <- "red2"
  })

  observeEvent(input$actualidad_nuevos_positivos_button, {
    rv$actualidad_variable <- "nuevos_positivos"
    rv$actualidad_color <- "darkorange"
  })
  
  observeEvent(input$actualidad_altas_button, {
    rv$actualidad_variable <- "altas"
    rv$actualidad_color <- "springgreen4"
  })
  
  observeEvent(input$actualidad_fallecimientos_button, {
    rv$actualidad_variable <- "fallecimientos"
    rv$actualidad_color <- "purple4"
  })
  
  output$actualidad_plot <- renderPlotly({
    
    Situacion_epidemiologica %>% 
      group_by(fecha) %>% 
      summarise(
        sum(get(rv$actualidad_variable))
        ) %>% 
      dplyr::rename(
        variable = `sum(get(rv$actualidad_variable))`
      ) %>%
      plot_ly(
        x = ~fecha,
        y = ~variable,
        color = I(rv$actualidad_color),
        type = "scatter", mode = 'lines'
      ) %>% 
      layout(
        xaxis = list(type = 'date', title = 'Fecha'),
        yaxis = list(
          type = 'numeric',
          title = rv$actualidad_variable %>%
            str_replace("_", " ") %>%
            str_to_title
        )
      )
  })

  ##### >> Bases de datos#############################################################################
  
  output$Descripcion_datos <- renderText({
    
    paste(
      
      Descripcion_de_Datos[[as.character(input$select_data_base)]][["descripcion"]], 

      "<li>",
      strong("Inicio de la publicación: "), 
      Descripcion_de_Datos[[as.character(input$select_data_base)]][["actualizacion"]], 
      "</li>",
      
      "<li>",
      strong("Inicio de la publicación: "), 
      Descripcion_de_Datos[[as.character(input$select_data_base)]][["fecha"]], 
      "</li>",
      
      "<li>",
      strong("Nota: "), 
      Descripcion_de_Datos[[as.character(input$select_data_base)]][["nota"]],
      "</li>"
      
      )
    
  })
  
  output$Datos = renderDataTable({
    Bases_de_Datos[[as.character(input$select_data_base)]]
  })

  output$Download_data <- downloadHandler(
      filename = function() {Nombres_de_Datos[[as.character(input$select_data_base)]]},
      content = function(file){
        write.csv(Bases_de_Datos[[as.character(input$select_data_base)]], file, row.names = FALSE)
      }
    )

  ##### >> Mapa ######################################################################################
  
  output$map_hospitales <- renderLeaflet({
    
    Hospitales_actual <- Hospitales %>%
      filter(fecha == fecha %>% unique() %>% last()) %>%
      dplyr::mutate(
        latitud = str_split_fixed(.$posicion, ",", 2) %>% .[,1] %>% as.numeric(),
        longitud = str_split_fixed(.$posicion, ",", 2) %>% .[,2] %>% as.numeric()
      )
    
    Hospitales_actual_variable <- Hospitales_actual[, input$map_variable_hospital] %>% 
      unlist(use.names = FALSE)
    
    map_hospital_legend <- Hospitales_actual$hospital %>%
      purrr::map(
        function(x){
          HTML(paste0(
            div(
              strong(x),
              br(),
              input$map_variable_hospital %>% str_replace("_", " ") %>% str_to_title,
              ": ",
              Hospitales_actual %>%
                filter(hospital == x) %>%
                dplyr::select(input$map_variable_hospital),
              style = "font-size:15px", class = "text-center"
            )
          ))
        }
      )
    
    if(input$map_variable_hospital == "hospitalizados_planta"){
      color_circles <- "darkorange"
    }
    if(input$map_variable_hospital == "hospitalizados_uci"){
      color_circles <- "red"
    }
    if(input$map_variable_hospital == "altas"){
      color_circles <- "green"
    }
    if(input$map_variable_hospital == "fallecimientos"){
      color_circles <- "purple"
    }
    
    leaflet() %>%
      addTiles() %>% 
      addCircleMarkers(
        data = Hospitales_actual,
        lng = ~longitud,
        lat = ~latitud,
        radius = ~Hospitales_actual_variable/max(Hospitales_actual_variable)*100,
        label = map_hospital_legend, 
        fillOpacity = 0.5, 
        color = color_circles,
        fillColor = color_circles
      )
    
  })
  
  output$map_provincias <- renderLeaflet({
    
    Situacion_epidemiologica_actual <-
      Situacion_epidemiologica %>%
      filter(fecha == fecha %>% unique() %>% last())
    
    map_provincias_legend <- Situacion_epidemiologica_actual$provincia %>%
      purrr::map(
        function(x){
          HTML(paste0(
            div(
              strong(x),
              br(),
              input$map_variable_provincias %>% str_replace("_", " ") %>% str_to_title,
              ": ",
              Situacion_epidemiologica_actual %>%
                filter(provincia == x) %>%
                dplyr::select(input$map_variable_provincias),
              style = "font-size:15px", class = "text-center"
            )
          ))
        }
      )
    
    if(input$map_variable_provincias == "casos_confirmados"){
      mypal <- colorNumeric(
        palette = "YlOrRd",
        domain = unlist(Situacion_epidemiologica$casos_confirmados)
      )
    }
    if(input$map_variable_provincias == "altas"){
      mypal <- colorNumeric(
        palette = "Greens",
        domain = unlist(Situacion_epidemiologica$altas) %>% na.omit()
      )
    }
    if(input$map_variable_provincias == "fallecimientos"){
      mypal <- colorNumeric(
        palette = "Reds",
        domain = unlist(Situacion_epidemiologica$fallecimientos) %>% na.omit()
      )
    }
    
    leaflet(Map_CyL) %>%
      addTiles() %>%
      addPolygons(
        color = "black",
        weight = 1.5,
        fillColor = ~mypal(Situacion_epidemiologica_actual[, input$map_variable_provincias] %>% unlist),
        fillOpacity = 0.5,
        label = map_provincias_legend
      ) %>%
      addLegend(
        pal = mypal,
        values = ~Situacion_epidemiologica_actual[, input$map_variable_provincias] %>% unlist,
        opacity = 0.7,
        title = input$map_variable_provincias %>%
          str_replace("_", " ") %>%
          str_to_title,
        position = "bottomright"
      )
    
  })

  ##### >> Análisis ##################################################################################

  ##### >>> Atención primaria ########################################################################

  output$analysis_atención_primaria_plot <- renderPlotly({
    
    Selection_Atencion_primaria  <- function(Data, factor) {
      if(factor == "grupo_edad"){
        Selected_data <- Data %>% 
          dplyr::select(-c("hombres", "mujeres"))
      }
      if (factor == "sexo") {
        Selected_data <- Data %>% 
          dplyr::select(-c("grupo_edad", "enfermos")) %>% 
          reshape2::melt(
            id.name = c("fecha", "provincia", "siglas"), 
            measure.vars = c("hombres", "mujeres"), 
            variable.name = "sexo",
            value.name = "enfermos"
          ) %>% 
          group_by(fecha, provincia, siglas, sexo) %>% 
          summarise(sum(enfermos)) %>% 
          rename(enfermos = `sum(enfermos)`) %>% 
          mutate(
            sexo = sexo %>% str_to_title(),
            sexo = factor(sexo, levels = c("Mujeres", "Hombres"))
            )
      }
      Selected_data
    }
    
    Atencion_primaria %>%
      ### Filter date selected
      filter(
        magrittr::extract(provincia != "Castilla y León"), 
        fecha == input$analysis_atención_primaria_date, 
        ) %>% 
      ### Unselect variables not used
      dplyr::select(-c("tasa_hombres_x1000", "tasa_mujeres_x1000")) %>% 
      ### Mutate column to factor
      mutate(provincia = factor(.$provincia)) %>% 
      Selection_Atencion_primaria(factor = input$analysis_atención_primaria_legend) %>% 
      plot_ly(
        x = ~provincia,
        y = ~enfermos,
        color = ~get(input$analysis_atención_primaria_legend), 
        colors = "Set1", 
        name = ~get(input$analysis_atención_primaria_legend),
        type = "bar"
      ) %>%
      layout(
        xaxis = list(type = 'categoric', title = 'Provincias'),
        yaxis = list(type = 'numeric', title = "Enfermos en atención primaria")#, 
        # barmode = "stack"
      )
    
  })
  
  ##### >>> Hospitales ###############################################################################
  
  observe({
    
    Hospitales_filtrado <- 
      Hospitales %>% 
      filter(provincia %in% input$analysis_hospitales_select_provincia)

    updateSelectInput(
      session,
      'analysis_hospitales_select_hospital', 
      label = NULL,
      choices = unique(Hospitales_filtrado$hospital)
    )
    
  }) 
  
  output$analysis_hospitales_plot <- renderPlotly({
    
    Hospitales %>%
      filter(
        provincia %in% input$analysis_hospitales_select_provincia, 
        hospital %in% input$analysis_hospitales_select_hospital
        ) %>%
      # arrange(fecha, provincia) %>%
      plot_ly(
        x = ~fecha,
        y = ~get(input$analysis_hospitales_select_data),
        color = ~hospital,
        colors = "Set1", 
        type = "scatter", mode = 'lines'
      ) %>%
      layout(
        xaxis = list(type = 'date', title = 'Fecha'),
        yaxis = list(
          type = 'numeric',
          title = input$analysis_hospitales_select_data %>%
            str_to_title %>%
            str_replace("_", " ") %>%
            as.character
        )
      )
    
  })

  ##### >>> Situación epidemiológica #################################################################

  output$Line_plot <- renderPlotly({
    
    Situacion_epidemiologica %>%
      filter(provincia %in% input$analysis_situacion_epidemiologica_provincias) %>%
      arrange(fecha, provincia) %>%
      plot_ly(
        x = ~fecha,
        y = ~get(input$analysis_situacion_epidemiologica_select_data),
        color = ~provincia,
        colors = "Set1", 
        type = "scatter", mode = 'lines'
        ) %>%
      layout(
        xaxis = list(type = 'date', title = 'Fecha'),
        yaxis = list(
          type = 'numeric',
          title = input$analysis_situacion_epidemiologica_select_data %>%
            str_replace("_", " ") %>%
            str_to_title %>%
            as.character
          )
      )
    
  })

})

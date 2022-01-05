##### Situación enfermos por coronavirus detectados en atencion primaria por tramos #######################

Atencion_primaria <- 
  read_delim(
    file = "Data/situacion-enfermos-por-coronavirus-detectados-en-atencion-primaria-por-tramos-d0.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE,
    show_col_types = FALSE
  ) %>%
  as.data.frame()%>% 
  mutate(
    fecha = as.Date(.$fecha),
    provincia = as.factor(.$provincia),
    siglas = as.factor(.$siglas), 
    grupo_edad = as.factor(.$grupo_edad), 
    grupo_edad = fct_relevel(.$grupo_edad, ">80", after = Inf)
    ) %>% 
  arrange(fecha, provincia, grupo_edad)

##### Situacion de hospitalizados por coronavirus en Castilla y Leon #######################################

Hospitales <-
  httr::GET(
    "https://analisis.datosabiertos.jcyl.es/api/records/1.0/search/?dataset=situacion-de-hospitalizados-por-coronavirus-en-castilla-y-leon&q=&rows=10000&sort=fecha&facet=fecha&facet=hospital&facet=provincia&facet=codigo_ine"
  ) %>%
  content("text") %>%
  jsonlite::fromJSON() %>%
  .$records %>%
  .$fields %>% 
  relocate(fecha, hospital, provincia, nuevos_hospitalizados_planta, 
           hospitalizados_planta, hospitalizados_planta_incluidos_sospecha, nuevos_hospitalizados_uci, 
           hospitalizados_uci, hospitalizados_uci_incluidos_sospecha, nuevas_altas, altas, 
           nuevos_fallecimientos, fallecimientos, codigo_ine, posicion) %>% 
  mutate(
    fecha = as.Date(.$fecha),
    hospital = as.factor(.$hospital), 
    provincia = as.factor(.$provincia)
  ) %>% 
  dplyr::arrange(fecha, hospital)

##### Situación epidemiologica coronavirus en Castilla y Leon ##############################################

Situacion_epidemiologica <- 
httr::GET(
  "https://analisis.datosabiertos.jcyl.es/api/records/1.0/search/?dataset=situacion-epidemiologica-coronavirus-en-castilla-y-leon&q=&rows=10000&sort=fecha&facet=fecha&facet=provincia"
  ) %>%
  content("text") %>%
  jsonlite::fromJSON() %>%
  .$records %>%
  .$fields %>%
  relocate(fecha, provincia, casos_confirmados, nuevos_positivos, altas, 
           fallecimientos, codigo_ine, posicion) %>%
  mutate(
    fecha = as.Date(.$fecha),
    provincia = as.factor(.$provincia)
    ) %>% 
  dplyr::arrange(fecha, provincia)

##### Lista de bases de datos #############################################################################

Bases_de_Datos <- 
  list(
    "Atención primaria" = Atencion_primaria, 
    "Hospitales" = Hospitales, 
    "Situacion epidemiologica" = Situacion_epidemiologica
    )

Descripcion_de_Datos <- 
  list(
    "Atención primaria" = list(
      "descripcion" = "Datos de número de pacientes por provincia, sexo y tramo de edad en Castilla y León.", 
      "actualizacion" = "de lunes a viernes.",
      "fecha" = "3 de abril de 2020.", 
      "ultima_fecha" = Atencion_primaria$fecha %>% unique() %>% last() %>% format('%d de %B de %Y.'),
      "nota" = "El dato de pacientes enfermos incluye todos los diagnósticos compatibles con coronavirus (neumonía por COVID-19 y enfermedad por COVID-19).", 
      "link" = "https://datosabiertos.jcyl.es/web/jcyl/set/es/salud/situacion-coronavirus-atencion-primaria/1284942898818"
    ),
    "Hospitales" = list(
      "descripcion" = "Evolución del número de hospitalizados en planta, hospitalizados en unidades de críticos, altas y fallecimientos por hospital debidos al coronavirus (COVID-19) en Castilla y León.", 
      "actualizacion" = "diaria.",
      "fecha" = "18 de marzo de 2020.", 
      "ultima_fecha" = Hospitales$fecha %>% unique() %>% last() %>% format('%d de %B de %Y.'),
      "nota" = "En los hospitales Santiago Apóstol, Santos Reyes y de Medina del Campo el valor de ingresados en UCI es 0 porque no disponen esta unidad.", 
      "link" = "https://datosabiertos.jcyl.es/web/jcyl/set/es/salud/situacion-coronavirus-hospitales/1284941728695"
    ),
    "Situacion epidemiologica" = list(
      "descripcion" = "Datos diarios de confirmados, nuevos casos, altas y fallecimientos en hospitales debidos al coronavirus (COVID-19) por provincias en Castilla y León.", 
      "actualizacion" = "de lunes a viernes.",
      "fecha" = "13 de marzo de 2020.", 
      "ultima_fecha" = Situacion_epidemiologica$fecha %>% unique() %>% last() %>% format('%d de %B de %Y.'),
      "nota" = '
        Desde el 18 de mayo, los nuevos casos confirmados solo reflejan resultados de PCR.
        Valores negativos en "Nuevos casos positivos" corresponden a pacientes cuyo resultado ha sido positivo inicialmente y en la prueba de confirmación se ha descartado.
      ', 
      "link" = "https://datosabiertos.jcyl.es/web/jcyl/set/es/salud/situacion-epidemiologica-coronavirus/1284940407131"
    )
  )


##### Mapa ################################################################################################

Map_CyL <- 
  # getData("GADM", country = "ES", level = 2) %>% 
  readRDS("Data/gadm36_ESP_2_sp.rds") %>% 
  subset(NAME_1 == "Castilla y León")

latitudes_hospitales <- c(
  "Complejo Asistencial de Ávila" = 40.665950, 
  "Complejo Asistencial de Segovia" = 40.942685,        
  "Complejo Asistencial de Soria" = 41.772020, 
  "Complejo Asistencial de Zamora" = 41.514459, 
  "Complejo Asistencial Universitario de Burgos" = 42.361854,
  "Complejo Asistencial Universitario de León" = 42.619435,
  "Complejo Asistencial Universitario de Palencia" = 41.996713,
  "Complejo Asistencial Universitario de Salamanca" = 40.964091,
  "Edificio Rondilla" = 41.658930,
  "Hospital Clínico Universitario de Valladolid" = 41.655687,
  "Hospital de Medina del Campo" = 41.295328,
  "Hospital El Bierzo" = 42.572222,
  "Hospital Santiago Apóstol" = 42.669824,
  "Hospital Santos Reyes" = 41.669570,
  "Hospital Universitario Río Hortega" = 41.629669
  )

longitudes_hospitales <- c(
  "Complejo Asistencial de Ávila" = -4.667236, 
  "Complejo Asistencial de Segovia" =  -4.128632,        
  "Complejo Asistencial de Soria" = -2.471435, 
  "Complejo Asistencial de Zamora" = -5.731352, 
  "Complejo Asistencial Universitario de Burgos" = -3.685055,
  "Complejo Asistencial Universitario de León" = -5.573359,
  "Complejo Asistencial Universitario de Palencia" = -4.519746,
  "Complejo Asistencial Universitario de Salamanca" = -5.673282,
  "Edificio Rondilla" = -4.726638, 
  "Hospital Clínico Universitario de Valladolid" = -4.719677,
  "Hospital de Medina del Campo" = -4.911416,
  "Hospital El Bierzo" = -6.643453,
  "Hospital Santiago Apóstol" = -2.988819,
  "Hospital Santos Reyes" = -3.691566,
  "Hospital Universitario Río Hortega" = -4.713756
)

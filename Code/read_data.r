##### Situación enfermos por coronavirus detectados en atencion primaria por tramos #######################

Atencion_primaria <- 
  read_delim(
    file = "Data/situacion-enfermos-por-coronavirus-detectados-en-atencion-primaria-por-tramos-d0.csv", 
    delim = ";", 
    escape_double = FALSE, 
    trim_ws = TRUE
  ) %>% 
  mutate(
    fecha = as.Date(.$fecha),
    provincia = as.factor(.$provincia),
    siglas = as.factor(.$siglas), 
    grupo_edad = as.factor(.$grupo_edad), 
    grupo_edad = fct_relevel(.$grupo_edad, ">80", after = Inf)
    ) %>% 
  arrange(fecha, provincia, grupo_edad)

##### Situacion de hospitalizados por coronavirus en Castilla y Leon #######################################

Hospitales<- 
  read_delim(
    # url("https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/situacion-coronavirus-hospitales/1284941728695.csv"), 
    file = "Data/situacion-de-hospitalizados-por-coronavirus-en-castilla-y-leon.csv",
    delim = ";", 
    escape_double = FALSE, 
    trim_ws = TRUE
  ) %>% 
  mutate(
    fecha = as.Date(.$fecha),
    hospital = as.factor(.$hospital), 
    provincia = as.factor(.$provincia),
  ) %>% 
  arrange(fecha, hospital)

##### Situación epidemiologica coronavirus en Castilla y Leon ##############################################

Situacion_epidemiologica <- 
  read_delim(
    # url("https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/situacion-epidemiologica-coronavirus/1284940407131.csv"),
    file = "Data/situacion-epidemiologica-coronavirus-en-castilla-y-leon.csv", 
    delim = ";", 
    escape_double = FALSE, 
    trim_ws = TRUE
    ) %>% 
  arrange(fecha, provincia)

##### Lista de bases de datos #############################################################################

Bases_de_Datos <- 
  list(
    "Atención primaria" = Atencion_primaria, 
    "Hospitales" = Hospitales, 
    "Situacion epidemiologica" = Situacion_epidemiologica
    )

Nombres_de_Datos <- 
  list(
    "Atención primaria" = "situacion-enfermos-por-coronavirus-detectados-en-atencion-primaria-por-tramos-d0.csv", 
    "Hospitales" = "situacion-de-hospitalizados-por-coronavirus-en-castilla-y-leon.csv", 
    "Situacion epidemiologica" = "situacion-epidemiologica-coronavirus-en-castilla-y-leon.csv"
  )

Descripcion_de_Datos <- 
  list(
    "Atención primaria" = list(
      "descripcion" = "Datos de número de pacientes por provincia, sexo y tramo de edad en Castilla y León.", 
      "actualizacion" = "de lunes a viernes.",
      "fecha" = "3 de abril de 2020.", 
      "nota" = "El dato de pacientes enfermos incluye todos los diagnósticos compatibles con coronavirus (neumonía por COVID-19 y enfermedad por COVID-19)."
    ),
    "Hospitales" = list(
      "descripcion" = "Evolución del número de hospitalizados en planta, hospitalizados en unidades de críticos, altas y fallecimientos por hospital debidos al coronavirus (COVID-19) en Castilla y León.", 
      "actualizacion" = "diaria.",
      "fecha" = "18 de marzo de 2020.", 
      "nota" = "En los hospitales Santiago Apóstol, Santos Reyes y de Medina del Campo el valor de ingresados en UCI es 0 porque no disponen esta unidad."
    ),
    "Situacion epidemiologica" = list(
      "descripcion" = "Datos diarios de confirmados, nuevos casos, altas y fallecimientos en hospitales debidos al coronavirus (COVID-19) por provincias en Castilla y León.", 
      "actualizacion" = "de lunes a viernes.",
      "fecha" = "13 de marzo de 2020.", 
      "nota" = '
        Desde el 18 de mayo, los nuevos casos confirmados solo reflejan resultados de PCR.
        Valores negativos en "Nuevos casos positivos" corresponden a pacientes cuyo resultado ha sido positivo inicialmente y en la prueba de confirmación se ha descartado.
      '
    )
  )


##### Mapa ################################################################################################

Map_CyL <- 
  # getData("GADM", country = "ES", level = 2) %>% 
  readRDS("Data/gadm36_ESP_2_sp.rds") %>% 
  subset(NAME_1 == "Castilla y León")

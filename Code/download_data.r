Hour_now <- Sys.time() %>% str_split_fixed(pattern = " ", n = 3) %>% .[2]

##### Situación enfermos por coronavirus detectados en atencion primaria por tramos ########################

# if(Sys.Date() > Atencion_primaria$fecha %>% unique %>% last && Hour_now > "12:00:00"){
#   download.file(
#     url='https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/situacion-coronavirus-atencion-primaria/1284942898818.csv',
#     destfile='Data/situacion-enfermos-por-coronavirus-detectados-en-atencion-primaria-por-tramos-d0.csv'
#   )
# }

##### Situación epidemiologica coronavirus en Castilla y Leon ##############################################

if(Sys.Date() > Situacion_epidemiologica$fecha %>% unique %>% last && Hour_now > "08:00:00"){
  download.file(
    url='https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/situacion-epidemiologica-coronavirus/1284940407131.csv',
    destfile='Data/situacion-epidemiologica-coronavirus-en-castilla-y-leon.csv'
  )
}

source("./Code/read_data.r", encoding = "UTF-8") 
# 
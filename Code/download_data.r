Hour_now <- Sys.time() %>% str_split_fixed(pattern = " ", n = 3) %>% .[2]

##### Situación enfermos por coronavirus detectados en atencion primaria por tramos ########################

if(Sys.Date() - 1 > Atencion_primaria$fecha %>% unique %>% last && Hour_now > "15:00:00"){
  download.file(
    url='https://datosabiertos.jcyl.es/web/jcyl/risp/es/salud/situacion-coronavirus-atencion-primaria/1284942898818.csv',
    destfile='Data/situacion-enfermos-por-coronavirus-detectados-en-atencion-primaria-por-tramos-d0.csv'
  )
}

source("./Code/read_data.r", encoding = "UTF-8") 
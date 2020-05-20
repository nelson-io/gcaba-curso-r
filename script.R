# Activamos paquetes

library(tidyverse)
library(rio)
library(sf)
library(lubridate)
library(jsonlite)

#importamos datos


recorridos <- import("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/recorridos-realizados-2019.csv",
                     encoding = "UTF-8")

usuarios <- map_df(2015:2018,~
                   import(paste0("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bicicletas-publicas/usuarios-ecobici-",.,".csv")))


# vemos información asociada a cada tabla

glimpse(recorridos)
glimpse(usuarios)


# observamos los valores asociados a la variable de id del usuario:
recorridos$id_usuario

# resolvemos con parse_number
parse_number(recorridos$id_usuario)

# resolvemos con RegEx
as.numeric(str_extract(recorridos$id_usuario,"\\d+"))

# observamos igualdad
table(parse_number(recorridos$id_usuario) == as.numeric(str_extract(recorridos$id_usuario,"\\d+")))

# impactamos en dataframe
recorridos$id_usuario <- as.numeric(str_extract(recorridos$id_usuario,"\\d+"))

# Volvemos a observar si es necesario parsear alguna otra variable
glimpse(recorridos)

# Notamos que las variables:
#  fecha_origen_recorrido y fecha_destino_recorrido son texto y debemos transformarlas en date-time
#  id_estacion_origen e id_estacion_destino deben tener una estructura sin caracteres especiales.
#  duracion_recorrido debe estar expresada en minutos

recorridos$fecha_origen_recorrido <- ymd_hms(recorridos$fecha_origen_recorrido)
recorridos$fecha_destino_recorrido <- ymd_hms(recorridos$fecha_destino_recorrido)

recorridos$id_estacion_origen <- parse_number(recorridos$id_estacion_origen)
recorridos$id_estacion_destino <- parse_number(recorridos$id_estacion_destino)

recorridos$dias_recorrido_minutos <- 
recorridos$duracion_recorrido %>% 
  str_extract("\\d{1,2}") %>% 
  as.numeric()*1440 


recorridos$duracion_recorrido_minutos <-  recorridos$duracion_recorrido %>% 
  str_extract("\\d{2}:\\d{2}:\\d") %>% 
  hms() %>% 
  as.numeric()/60

recorridos$duracion_recorrido_minutos <- recorridos$duracion_recorrido_minutos + recorridos$dias_recorrido_minutos



# Ahora revisamos nuevamente las variables:

glimpse(recorridos)

#Ahora verificamos integridad de los datos: la duración del recorrido en minutos debe dde ser aproximadamente similar a la 
# diferencia calculada entre la fecha de origen y la fecha de destino.

# Primero generamos la variable duracion_calculada

recorridos$duracion_calculada <- as.numeric(recorridos$fecha_destino_recorrido - recorridos$fecha_origen_recorrido)/60

# Ahora generamos una nueva columna con la diferencia entre nuestro cálculo y la información ofrecida ya calculada
# comprobando si es que existe alguna diferencia (observamos los 100 casos más extremos)

recorridos <- recorridos %>% 
  mutate(diff_time = abs(duracion_recorrido_minutos - duracion_calculada)) 
  
#cuántos registros tienen una diferencia en duración mayor a 1 minuto? qué proporción representan?

table(recorridos$diff_time > 1)
prop.table(table(recorridos$diff_time > 1))

# Observemos los 500 casos con mayores duraciones de uso
recorridos %>% 
  arrange(desc(duracion_recorrido_minutos)) %>% 
  head(500) %>% 
  View()



#calculamos distancia entre cada una de las estaciones  y obtenemos velocidad promedio de todas las que salieron de la 
# estacion 1

# primero armamos matriz de origen destino con todas las posibilidades
selection_1 <- recorridos %>% 
  select(id_estacion_origen, id_estacion_destino, 
         lat_estacion_origen,long_estacion_origen, 
         lat_estacion_destino, long_estacion_destino) %>% 
  filter(id_estacion_origen == 1) %>% 
  unique()

# seteamos nuestra API key para consumir datos de la API de DISTANCEMATRIX AI
api_key <- "0cEhha2C4kXOsHHxb9T6IHvrJpwsC"

#definimos una función que genera la consulta a la API y extrae la distancia en kilómetros

km_extractor <- function(df){
  query <- paste0("https://api.distancematrix.ai/maps/api/distancematrix/json?&origins=",
                  df$lat_estacion_origen, ",",
                  df$long_estacion_origen, "&destinations=",
                  df$lat_estacion_destino, ",",
                  df$long_estacion_destino, "&key=",
                  api_key)
  
  JSON <-  fromJSON(txt = query)
  km <- JSON$rows$elements[[1]]$distance$value/100 
  
  return(km)
}

# Hacemos loop para iterar sobre el dataframe y generar la columna de distancia

dist_matrix <- data.frame()

for(i in 1:nrow(selection_1)){
  df <- slice(selection_1,i)
  km <- km_extractor(df)
  dist_matrix <- rbind(dist_matrix, cbind(df, dist_km = km))
}



































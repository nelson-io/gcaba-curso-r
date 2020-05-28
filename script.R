`# Activamos paquetes

library(tidyverse)
library(rio)
library(lubridate)
library(jsonlite)
library(sf)

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


######################PARTE II#########################################


#calculamos distancia entre cada una de las estaciones  y obtenemos velocidad promedio de todas las que salieron de la 
# estacion 1

# primero armamos matriz de origen destino con todas las posibilidades
selection_1 <- recorridos %>% 
  select(id_estacion_origen, id_estacion_destino, 
         lat_estacion_origen,long_estacion_origen, 
         lat_estacion_destino, long_estacion_destino) %>% 
  filter(id_estacion_origen == 175) %>% 
  group_by(id_estacion_destino) %>% 
  mutate(total = n()) %>%
  ungroup() %>% 
  filter(total >100) %>% 
  unique() %>% 
  filter(complete.cases(.))

# seteamos nuestra API key para consumir datos de la API de DISTANCEMATRIX AI
api_key <- import("api_key.txt")

#definimos una función que genera la consulta a la API y extrae la distancia en kilómetros

km_extractor <- function(df){
  query <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?mode=bycicling&origins=",
                  df$lat_estacion_origen, ",",
                  df$long_estacion_origen, "&destinations=",
                  df$lat_estacion_destino, ",",
                  df$long_estacion_destino, "&key=",
                  api_key)

  
  JSON <-  fromJSON(txt = query)
  km_api <- JSON$rows$elements[[1]][[1]][[2]]/1000 
  mins_api <- JSON$rows$elements[[1]][[2]][[2]]/60 
  
  return(data.frame(km_api = km_api,
                    mins_api = mins_api))
}

# Hacemos loop para iterar sobre el dataframe y generar la columna de distancia

selection_1 %>% slice(1) %>% km_extractor()


dist_frame <- data.frame()

for(i in seq_along(selection_1$id_estacion_origen)){
  df <- slice(selection_1,i)
  km <- km_extractor(df)
  dist_frame <- rbind(dist_frame, cbind(df, dist_km = km))
  if(i %% 10 == 0){print(i)}
}

# Ahora vemos para todos los casos las velocidades y observamos el histograma de las velocidades en km/h

km_df <- recorridos %>% 
  filter(id_estacion_origen == 175) %>% 
  left_join(dist_frame) %>% 
  mutate(km_h = dist_km.km_api/(duracion_calculada/60),
         km_h.api = dist_km.km_api/(dist_km.mins_api/60)) %>% 
  filter(km_h > 0)

# hacemos histograma
ggplot(km_df)+
  geom_histogram(aes(x = km_h)) +
  scale_x_log10()

# boxplot
ggplot(km_df)+
  geom_boxplot(aes(y = km_h))+
  lims(y=c(0,30))

#estadísticas
summary(km_df$km_h)


# vemos para cada recorrido, la distribución

for(i in unique(km_df$id_estacion_destino)){
  df <- km_df %>% 
    filter(id_estacion_destino == i,
           km_h<40)
  
  plot <- ggplot(df)+
    geom_histogram(aes(x= km_h))+
    geom_vline(aes(xintercept = mean(km_h.api)), col = 'red')+
    ggtitle(paste0("Origen:",unique(df$nombre_estacion_origen),"\n",
                   "Destino:",unique(df$nombre_estacion_destino)))
  
  print(plot)
}





# Ahora queremos observar los 10 destinos más frecuentes y los menos.

freq_df <- rbind(
  recorridos %>%
    group_by(
      id_estacion = id_estacion_destino,
      latitud = lat_estacion_destino,
      longitud = long_estacion_destino
    ) %>%
    summarise(total = n()) %>%
    arrange(desc(total)) %>%
    head(10) %>%
    mutate(tipo = "Más frecuentes") %>% 
    ungroup(),
  
  
  recorridos %>%
    filter(id_estacion_destino != 0) %>%
    group_by(
      id_estacion = id_estacion_destino,
      latitud = lat_estacion_destino,
      longitud = long_estacion_destino
    ) %>%
    summarise(total = n()) %>%
    arrange(desc(total)) %>%
    tail(10) %>%
    mutate(tipo = "Menos frecuentes") %>% 
    ungroup()
)

### Vamos a visualizarlos en el mapa

#los convertimos en un dataframe geo espacial



geo_freq <- st_as_sf(freq_df,coords = c("longitud", "latitud")) %>% 
  st_set_crs(4326)

#descargamos geojson de caba

caba <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson")

ggplot(data = caba) +
  geom_sf()+
  geom_sf(data = geo_freq, aes(col = tipo, size = total), alpha = .6)+
  theme_bw()+
  ggtitle("Destinos más y menos frecuentes de ECOBICIS")
  
















`
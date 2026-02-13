library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)


# cargar datos

url_mediciones <- "https://www.bilbao.eus/aytoonline/jsp/opendata/movilidad/od_sonometro_mediciones.jsp?idioma=c&formato=json"
sonometros <- fromJSON(url_mediciones) 

url_ubicacion <- "https://www.bilbao.eus/aytoonline/jsp/opendata/movilidad/od_sonometro_ubicacion.jsp?idioma=c&formato=geojson"
ubicacion <- st_read(url_ubicacion)
ubicacion[] <- lapply(ubicacion, function(x) {
  if (is.character(x)) iconv(x, from = "latin1", to = "UTF-8") else x
})

#Cogemos los datos de github ya que las horas no coinciden y en este repositorio se ejecuta de manera
#   automatica  para tener mas datos para que sea mas facil que coincidan los datos y tener tambien horas 
#   nocturnas y dias festivos (Esta todo automatizado).
#De esta forma vamos guardando de forma automatica los datos de todos los dias 
url_trafico <- "https://raw.githubusercontent.com/erlantz9rey/RetoR/master/data/historico_trafico.geojson"
trafico <- st_read(url_trafico)



# ----------------------------------------------------------------------------------------
#cambio nombre fecha columnas para luego
sonometros <- sonometros %>%
  mutate(Fecha_datetime = ymd_hms(fecha_medicion)) 
sonometros$fecha_medicion <- NULL

trafico <- trafico %>%
  mutate(Fecha_datetime_trafico = ymd_hms(FechaHora))
trafico$FechaHora <- NULL

# ----------------------------------------------------------------------------------------
#ver tipos de cada columna para ver si estan bien
str(sonometros)
str(trafico)
str(ubicacion)

#cambiar columnas necesarias


columna <- c("CodigoSeccion", "Ocupacion","Intensidad","Velocidad")
trafico[columna] <- lapply(trafico[columna], as.double)


ubicacion$longitude <- as.double(ubicacion$longitude)
ubicacion$latitude <- as.double(ubicacion$latitude)
ubicacion$status <- as.integer(ubicacion$status)
ubicacion$deviceTypeId <- as.integer(ubicacion$deviceTypeId)

sonometros$decibelios <- as.integer(sonometros$decibelios)
# ----------------------------------------------------------------------------------------
#ver si hay nulos 

sum(is.na(sonometros))
sum(is.na(trafico))
sum(is.na(ubicacion))
# No hay nulos


fecha_corte <- as.POSIXct("2026-01-29 20:35:00", tz = "UTC")

sonometros_filtradas <- sonometros %>%
  filter(Fecha_datetime >= fecha_corte)

trafico_filtrado <- trafico %>%
  filter(Fecha_datetime_trafico >= fecha_corte)

# ----------------------------------------------------------------------------------------
#       Unir datasets

#Todo esto sigue siendo por como estan las horas y munitos de los datos ya que no queremos comparar datos de diferentes momentos
# ver las horas únicas
bloques_horas <- unique(round_date(sonometros_filtradas$Fecha_datetime, "hour"))

lista_resultados <- list()

#Bucle para cruzar datos de sonometros y tráfico hora por hora
for(i in seq_along(bloques_horas)) {
  h_actual <- bloques_horas[i]
  
  # Filtramos las mediciones de sonometros de la hora actual
  med_h <- sonometros_filtradas %>% 
    filter(round_date(Fecha_datetime, "hour") == h_actual)
  
  # Filtramos el tráfico en un margen de 30 min antes/después para que coincidan
  traf_h <- trafico_filtrado %>%
    filter(Fecha_datetime_trafico >= h_actual - 1800, 
           Fecha_datetime_trafico <= h_actual + 1800)
  
  # Si tenemos datos de los dos, realizamos lo juntamos
  if(nrow(traf_h) > 0 && nrow(med_h) > 0) {
    # unir tambien la tabla de ubicacion
    med_geo_h <- ubicacion %>%
      inner_join(med_h, by = c("name" = "nombre_dispositivo"))
    
    # Buscamos el tramo de tráfico más cercano a cada sonómetro
    indices <- st_nearest_feature(med_geo_h, traf_h)
    
    # Guardamoslos datos  de ruido y tráfico cercano
    lista_resultados[[i]] <- cbind(
      st_drop_geometry(med_geo_h),
      st_drop_geometry(traf_h[indices, ])
    )
  }
}

# Unimos todos con tiempo que coincidan
resultado_final_v2 <- bind_rows(lista_resultados) %>%
  mutate(diff_t = abs(as.numeric(difftime(Fecha_datetime, Fecha_datetime_trafico, units = "secs")))) %>%
  group_by(name, Fecha_datetime) %>%
  slice_min(diff_t, with_ties = FALSE) %>% # Evitamos duplicados eligiendo
  ungroup()

#unimos la ubicacion por el nombre 
resultado_final_sf <- ubicacion %>%
  inner_join(resultado_final_v2, by = "name")

# Limpieza de duplicados y columnas que no necesitamos
resultado_final_sf <- resultado_final_sf[, !grepl("\\.y$", names(resultado_final_sf))]
names(resultado_final_sf) <- gsub("\\.x$", "", names(resultado_final_sf))
resultado_final_sf <- subset(resultado_final_sf, select = -c(fecha_ejecucion, diff_t, Fecha_datetime_trafico))

#le cambio el nombre para que sea mas comodo
dfFinal <- resultado_final_sf
dfFinal

#sacamos la hora a otra columna 
dfFinal$Fecha_datetime <- as.POSIXct(dfFinal$Fecha_datetime)
dfFinal$Hora <- hour(dfFinal$Fecha_datetime) 

#creamos columnas sobre los dias
dfFinal$DiaSemana <- wday(dfFinal$Fecha_datetime, label = TRUE, abbr = FALSE)
dfFinal$TipoDia <- ifelse(wday(dfFinal$Fecha_datetime) %in% c(1, 7), "Fin de Semana", "Laboral")

#verlo en mapa
library(mapview)

# Visualizamos el mapa con mapview
mapview(trafico_filtrado, zcol = "Intensidad", layer.name = "Intensidad")+
mapview(dfFinal, zcol = "decibelios", col.regions = hcl.colors(100, "Inferno"),layer.name="sonometro")

# ----------------------------------------------------------------------------------------
#       Analisis
#       GRAFICOS Y CONCLUSIONES


# grafico Ruido medio por hora
ggplot(dfFinal, aes(x = Hora, y = decibelios, fill = decibelios)) +
  stat_summary(fun = "mean", geom = "col", fill = "orange") + 
  labs(title = "Nivel de Ruido Medio por Hora",
       x = "Hora",
       y = "Decibelios") +
  theme_minimal()
#Vemos que a todas hora hay un ruido parecido

#Comparacion ruido medio dia semanal y finde semana
ggplot(dfFinal, aes(x = TipoDia, y = decibelios)) +
  stat_summary(fun = "mean", geom = "col", fill = "blue") + 
  labs(title = "Nivel de Ruido Medio por Hora",
       x = "Hora",
       y = "Decibelios") +
  theme_minimal()
#de media es muy parecido pero el fin de semana hay un poco mas de ruido


#Relacion entre trafico y ruido 
ggplot(dfFinal, aes(x = Intensidad, y = decibelios)) +
  geom_point(alpha = 0.4, color = "green") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Relación entre Intensidad de Tráfico y Ruido",
    x = "Intensidad de Tráfico",
    y = "Ruido"
  ) +
  theme_minimal()
#Como los sonometros no son solo para el trafico no se ve una relacion directa de mas intensidad mas ruido

# ver la diferencia de la intensidad del trafico durante las horas del dias laborale y fin de semana 
ggplot(dfFinal, aes(x = Hora, y = Intensidad, group = TipoDia, color = TipoDia)) +
  geom_line(size = 1) +
  facet_wrap(~TipoDia) + 
  scale_color_viridis_d(option = "viridis") +
  labs(
    title = "Intensidad en las horas del finde semana",
    x = "Hora del Día",
    y = "Intensidad"
  ) +
  theme_minimal()






library(sf)
library(dplyr)
library(lubridate)

# Crear carpeta data si no existe (requisito de estructura de carpetas) [cite: 110]
if (!dir.exists("data")) {
  dir.create("data")
}

url_traf <- "https://www.bilbao.eus/aytoonline/srvDatasetTrafico?formato=geojson"
archivo_final <- "data/historico_trafico.geojson" 
temp_file <- "temp_descarga.geojson"

# 1. Descarga del dato actual usando curl (más estable en GitHub Actions)
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
comando <- paste0("curl -k -L -A '", user_agent, "' '", url_traf, "' -o ", temp_file, " --max-time 150 --retry 3")
system(comando)

# 2. Verificación y Procesamiento
if (file.exists(temp_file) && file.info(temp_file)$size > 500) {
  
  # Leer la descarga actual
  trafico_nuevo <- st_read(temp_file, quiet = TRUE)
  
  # Añadir marca de tiempo (imprescindible para el análisis de franjas horarias) [cite: 56, 110]
  trafico_nuevo <- trafico_nuevo %>% 
    mutate(fecha_ejecucion = format(now(), "%Y-%m-%d %H:%M:%S"))
  
  # 3. Lógica de Acumulación (Append)
  if (file.exists(archivo_final)) {
    # Leer histórico, combinar con lo nuevo y guardar
    historico_previo <- st_read(archivo_final, quiet = TRUE)
    historico_actualizado <- rbind(historico_previo, trafico_nuevo)
    
    # Guardar sobreescribiendo el archivo único
    st_write(historico_actualizado, archivo_final, delete_dsn = TRUE, quiet = TRUE)
    print(paste("✅ Datos añadidos al histórico. Filas totales:", nrow(historico_actualizado)))
  } else {
    # Si es la primera vez, crear el archivo histórico
    st_write(trafico_nuevo, archivo_final, quiet = TRUE)
    print("✅ Primer archivo histórico creado.")
  }
  
  # Limpiar archivo temporal
  file.remove(temp_file)
  
} else {
  stop("❌ Error en la descarga: El archivo es inexistente o corrupto.")
}

# Registro básico de ejecución (Requisito fase 26-29 enero) [cite: 110]
cat(paste("Ejecución completada:", now(), "\n"), file = "data/log_ejecucion.txt", append = TRUE)
# TrafficNoise-Bilbao-Analytics 🚦🔊

## 1. IDENTIDAD
**Nombre del Software:** TrafficNoise-Bilbao-Analytics  
**Propósito Técnico:** Sistema de integración y análisis geoespacial diseñado para correlacionar la intensidad del tráfico rodado con los niveles de ruido (decibelios) en tiempo real. El proyecto utiliza técnicas de unión por proximidad geográfica para vincular sensores estáticos de sonido con segmentos dinámicos de tráfico.

---

## 2. STACK TECNOLÓGICO
* **Lenguaje:** R (v4.x+)
* **Librerías Clave:**
    * `sf`: Procesamiento vectorial y cálculos geométricos.
    * `httr2` & `jsonlite`: Gestión de APIs.
    * `dplyr` & `tidyr`: Limpieza de datos.
    * `lubridate`: Gestión de series temporales.
    * `ggplot2` & `mapview`: Visualización y mapas.
* **Automatización:** GitHub Actions para actualización de GeoJSON.

---

## 3. MODELADO DE DATOS
| Entidad | Origen | Formato |
| :--- | :--- | :--- |
| **Tabla Sonómetros** | Open Data Bilbao | JSON |
| **Esquema Ubicación** | Open Data Bilbao | GeoJSON |
| **Histórico Tráfico** | GitHub Repo | GeoJSON |
| **dfFinal** | Procesado | sf/Dataframe |

---

## 4. RENDIMIENTO Y MÉTRICAS
* **Ventana Temporal:** 1800 segundos ($\pm 30$ min) para sincronización.
* **Algoritmo de Unión:** `st_nearest_feature` (Vecino más cercano).
* **Calidad:** 0% nulos tras limpieza; codificación forzada a `UTF-8`.

---

## 5. ARQUITECTURA
1. **Ingesta:** Captura desde API Open Data Bilbao.
2. **Normalización:** Casting de tipos y estandarización `ymd_hms`.
3. **Procesamiento Geoespacial:** Unión por proximidad entre sensores y tramos.
4. **Enriquecimiento:** Clasificación de días (Laboral/Finde).
5. **Persistencia:** Actualización automática del histórico en el repositorio.

---

## 6. KEYWORDS EXPERTAS
`Spatial Join`, `Euclidean Distance`, `Simple Features (sf)`, `Time-series Alignment`, `Acoustic Pollution`, `Flow Intensity`.

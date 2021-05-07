message("Descargando datos")

############################################## Funcion para descargar los datasets
fecha_actual <- format(Sys.time(), "%Y_%m_%d")

descarga_archivo <- function(ruta_a_descargar, url_origen, nombre_versionado) {
  #la funcion requiere la ruta al directorio de descrga, el link para descargar el dataset y el nombre versionado, que en nuestro caso es por fecha
  if (length(list.files(ruta_a_descargar)) == 0 | substr(max(list.files(ruta_a_descargar)), 1, 10) < fecha_actual) { #si el directorio esta vacio o si la fecha del archivo no coincide con la de hoy, descarga el archivo.
    download.file(url_origen,paste0(ruta_a_descargar,nombre_versionado), method = 'auto', quiet = FALSE)
    message("Se descargo la ultima publicacion del archivo: ", nombre_versionado)
  } else {
    message("Ya se cuenta con la version mas reciente de la publicacion: ", nombre_versionado)
  }
}

############################################## Descargar datos de Muertes por Covid-19 Global

url_time_series <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
nombre_archivo_muertes <- "time_series_covid19_deaths_global.csv"
url_archivo_muertes  <- paste0(url_time_series, nombre_archivo_muertes) #url de descarga a usar en la funcion

dir.create(file.path("../datos/muertes"),recursive = TRUE) #crea la carpeta datos/muertes en el directorio de trabajo, y si ya existe solo tira una advertencia.
dir_muertes <- "../datos/muertes/"

nombre_archivo_muertes_versionado <- paste0(fecha_actual, "_", nombre_archivo_muertes) #genero el nombre con el que voy a guardar el archivo de muertes por covid, con el versionado de acuerdo a la fecha
ruta_archivo_muertes_versionado <- paste0(dir_muertes, nombre_archivo_muertes_versionado)

#descargando el archivo
descarga_archivo(dir_muertes,url_archivo_muertes,nombre_archivo_muertes_versionado)

############################################## Leer el ultimo Archivo publicado de Movilidad Apple
Apple_URL_json <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/current/v3/index.json" #link del archivo json con los metadatos del csv a descargar
apple_json_info = fromJSON(Apple_URL_json)
toJSON(apple_json_info, pretty=TRUE, flatten=TRUE, auto_unbox=TRUE) #me fijo como esta armado

#armo el link
url_apple_mobility <- "https://covid19-static.cdn-apple.com" #url base

base_path <- apple_json_info$basePath #path base 

csv_path <- apple_json_info$regions$`en-us`$csvPath #path del csv.

url_apple_mobility_archivo <- paste(url_apple_mobility, base_path, csv_path, sep = "") #arma la url completa y traeria el csv publicado mas reciente

dir.create(file.path("../datos/mobility"),recursive = TRUE) #crea la carpeta cache/muertes en el directorio de trabajo, y si ya existe solo tira una advertencia.
dir_mobility <- "../datos/mobility/"

nombre_archivo_mobility_versionado <- paste0(fecha_actual, "_", "apple_mobility_trends.csv") #genero el nombre con el que voy a guardar el archivo  con el versionado de acuerdo a la fecha
ruta_archivo_mobility_versionado <- paste0(dir_mobility, nombre_archivo_mobility_versionado)

#descargando el archivo
descarga_archivo(dir_mobility , url_apple_mobility_archivo, nombre_archivo_mobility_versionado)
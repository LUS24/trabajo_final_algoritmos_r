
############################################## Librerias

# Package names
packages <- c("tidyverse", "zoo", "patchwork", "hrbrthemes", "rjson", "jsonlite")
              
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


############################################## Funcion para descargar los datasets
fecha_actual <- format(Sys.time(), "%Y_%m_%d")

descarga_archivo <- function(ruta_a_descargar, url_origen, nombre_versionado) { #la funcion requiere la ruta al directorio de descrga, el link para descargar el dataset y el nombre versionado, que en nuestro caso es por fecha
  if (length(list.files(ruta_a_descargar)) == 0 | substr(max(list.files(ruta_a_descargar)), 1, 10) < fecha_actual) { #si el directorio esta vacio o si la fecha del archivo no coincide con la de hoy, descarga el archivo.
    download.file(url_origen,paste0(ruta_a_descargar,nombre_versionado), method = 'auto', quiet = FALSE)
    message("Se descargo la ultima publicacion del archivo: ", nombre_versionado)
  } else {
    message("Ya se cuenta con la version mas reciente de la publicacion: ", nombre_versionado)
  }
}

############################################## Leer Archivo de Muertes por Covid-19 Global
# time_series_19-covid-Deaths_archived_0325.csv
url_time_series <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
nombre_archivo_muertes <- "time_series_covid19_deaths_global.csv"
url_archivo_muertes  <- paste0(url_time_series, nombre_archivo_muertes) #url de descarga a usar en la funcion

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #para setear como directorio de trabajo la carpeta en donde se encuentre el script

dir.create(file.path("../cache/muertes"),recursive = TRUE) #crea la carpeta cache/muertes en el directorio de trabajo, y si ya existe solo tira una advertencia.
dir_muertes <- "../cache/muertes/"
nombre_archivo_muertes_versionado <- paste0(fecha_actual, "_", nombre_archivo_muertes) #genero el nombre con el que voy a guardar el archivo de muertes por covid, con el versionado de acuerdo a la fecha
ruta_archivo_muertes_versionado <- paste0(dir_muertes, nombre_archivo_muertes_versionado)

#descargando el archivo
descarga_archivo(dir_muertes,url_archivo_muertes,nombre_archivo_muertes_versionado)

COVID_19_h  <- read.csv(ruta_archivo_muertes_versionado, sep = ",", header = T)  #leyendo el archivo en la carpeta cache correspondiente

############################################## preparo los datos de muertes acumuladas por covid
COVID_19_h$Province.State <- NULL
COVID_19_h$Lat <- NULL
COVID_19_h$Long <- NULL

COVID_19_deaths <- COVID_19_h %>% gather(fecha, casos, 2:ncol(COVID_19_h))
colnames(COVID_19_deaths) <- c("pais", "fecha", "casos")
COVID_19_deaths$fecha <- as.Date(as.character(COVID_19_deaths$fecha), format = "X%m.%d.%y")
argentina_deaths<- subset(COVID_19_deaths,pais == 'Argentina', -pais)


############################################## Leer el ultimo Archivo publicado de Movilidad Apple
Apple_URL_json <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/current/v3/index.json" #link del archivo json con los metadatos del csv a descargar
apple_json_info = fromJSON(Apple_URL_json)
toJSON(apple_json_info, pretty=TRUE, flatten=TRUE, auto_unbox=TRUE) #me fijo como esta armado

#armo el link
url_apple_mobility <- "https://covid19-static.cdn-apple.com" #url base
base_path <- apple_json_info$basePath #path base 
csv_path <- apple_json_info$regions$`en-us`$csvPath #path del csv.
url_apple_mobility_archivo <- paste(url_apple_mobility, base_path, csv_path, sep = "") #arma la url completa y traeria el csv publicado mas reciente
dir.create(file.path("../cache/mobility"),recursive = TRUE) #crea la carpeta cache/muertes en el directorio de trabajo, y si ya existe solo tira una advertencia.
dir_mobility <- "../cache//mobility/"
nombre_archivo_mobility_versionado <- paste0(fecha_actual, "_", "apple_mobility_trends.csv") #genero el nombre con el que voy a guardar el archivo  con el versionado de acuerdo a la fecha
ruta_archivo_mobility_versionado <- paste0(dir_mobility, nombre_archivo_mobility_versionado)

#descarando el archivo
descarga_archivo(dir_mobility , url_apple_mobility_archivo, nombre_archivo_mobility_versionado)

mobility <- read.csv(ruta_archivo_mobility_versionado, sep = ",", header = T) #leyendo el archivo en la carpeta cache correspondiente

############################################## preparo los datos Movilidad
#eliminando NA, modificando los formatos de la fecha y descartando campos que no utilizamos

mobility$geo_type <- NULL
mobility$alternative_name <- NULL
mobility$sub.region <- NULL
mobility$country <- NULL

mobility <- mobility %>% gather(fecha, movilidad, 3:ncol(mobility))
mobility$fecha <- as.Date(as.character(mobility$fecha), format = "X%Y.%m.%d")
colnames(mobility) <- c("pais", "tipo_transporte","fecha" ,"movilidad")
argentina_mobility <- subset(mobility,pais == 'Argentina', -pais)
argentina_mobility <- argentina_mobility %>% group_by(fecha) %>% summarise(movilidad = mean(movilidad))


############################################## Uniendo ambos datasets
datos <- merge(argentina_deaths,argentina_mobility, by = "fecha")
colnames(datos) <- c("fecha","muertes_covid19", "movilidad")


############################################## Estimando la correlación de ambas variables para el momento en que iniciaron las muertes
correlacion <- cor.test(datos$movilidad[47:nrow(datos)],datos$muertes_covid19[47:nrow(datos)])
correlacion


############################################## Calculo de las muertes por dia (por diferencia)
datos$muertes_diarias <- datos$muertes_covid19 - lag(datos$muertes_covid19)


############################################## revisando datos faltantes
sum(is.na(datos)) # hay (cuantos) NA en el df datos ??
apply(is.na(datos), 2, which)# busca los hay NA en el df datos, y te trae la columna y posicion.
datos$movilidad <- na.locf(datos$movilidad) #Reemplaza los valores faltantes con el valor del registro anterior
datos$muertes_diarias[is.na(datos$muertes_diarias)] <- 0 # reemplazo el 1er NA de la columna calculada de muertes diarias
sum(is.na(datos)) # busco de vuelta, si hay (cuantos) NA en el df datos

rownames(datos) <- NULL #Reseteando los indices


############################################## KPI objetivos
# N° de muertos por millón de habitantes
# Día de mayor tasa movilidad
# Día de menor rasa movilidad
# Día con mayor cantidad de muertes
# Día con menor cantidad de muertes

proyeccion_poblacion_arg <- 45376763 #es un nro magico, de acuerdo a proyecciones (al 2020) del indec https://www.argentina.gob.ar/pais/poblacion/proyecciones 
datos_inicio_cuarentena <- subset(datos, fecha >'2020-03-19') #recorte, para estos KPI, los datos desde la fecha de inicio de la cuarentena
#el/los dia/s de mayor tasa movilidad
Dia_Mayor_Movilidad <- subset(datos_inicio_cuarentena, movilidad == max(movilidad, na.rm = T)) #no se porque en el 1er run no devuelve nada <-- Hay que remover NAs
message("El ", weekdays(Dia_Mayor_Movilidad$fecha)," ", format(as.Date(Dia_Mayor_Movilidad$fecha),format= "%d/%m/%y")," fue el dia de mayor movilidad desde el inicio de la pandemia, con una tasa de ",Dia_Mayor_Movilidad$movilidad)
#Dia_Mayor_Movilidad[c(1,3)]
#el/los dias de menor tasa de movilidad
Dia_Menor_Movilidad <- subset(datos_inicio_cuarentena,movilidad == min(movilidad, na.rm = T)) #no se porque en el 1er run no devuelve nada <-- Hay que remover NAs
#Dia_Menor_Movilidad[c(1,3)]
message("El ", weekdays(Dia_Menor_Movilidad$fecha)," ", format(as.Date(Dia_Menor_Movilidad$fecha),format= "%d/%m/%y") ," fue el dia de menor movilidad desde el inicio de la pandemia, con una tasa de ", Dia_Menor_Movilidad$movilidad)

#el/los dias con menor cantidad de muertes
#Dia_Menor_NroMuertes<- subset(datos_inicio_cuarentena,muertes_diarias == min(muertes_diarias)) #0 muertes al inicio de la cuarentena y en navidad...
#Dia_Menor_NroMuertes[c(1,4)]

#el/los días con mayor cantidad de muertes
Dia_Mayor_NroMuertes<- subset(datos_inicio_cuarentena,muertes_diarias == max(muertes_diarias)) #revisar si mostrar este valor/dia porque creo que coincide cuando hubo ajustes en la registracion de casos por parte de GobAr.
Dia_Mayor_NroMuertes[c(1,4)] 
#esto muestra un valor irreal, ya que ese dia se registraron muertes que no habian sido contabilizadas, para este caso no lo tendremos en cuenta.
Dia_Mayor_NroMuertes<- subset(datos_inicio_cuarentena[-which.max(datos_inicio_cuarentena$muertes_diarias),], muertes_diarias == max(muertes_diarias)) #desestimo el valor previo 
message("El ", weekdays(Dia_Mayor_NroMuertes$fecha)," ", format(as.Date(Dia_Mayor_NroMuertes$fecha),format= "%d/%m/%y") ," fue el dia de mayor numero de muertes desde el inicio de la pandemia, con ", Dia_Mayor_NroMuertes$muertes_diarias, " muertes registradas")

#N° de muertos por millón de habitantes
Muertos_por_millon <- round(max(datos$muertes_covid19)/proyeccion_poblacion_arg,4)
message(Muertos_por_millon, " muertos, debido al COVID-19, por millon de hab.")


############################################## Graficando:

# Movilidad vs Muertes acumuladas por Covid

mov_color <- rgb(0.2, 0.6, 0.9, 1)
mu_color <- "#69b3a2"
coefi <- 500 #Coeficiente utilizado para llevar a escala a la variable con el eje Y

g1 <-  ggplot(datos, aes(x=fecha)) +
  geom_line( aes(y= movilidad), size=1, color=mov_color) + 
  geom_line( aes(y= muertes_covid19/coefi), size=1, color=mu_color) +
  scale_y_continuous(name = "Movilidad (%)",sec.axis = sec_axis(~.*coefi, name="Muertes acumuladas por Covid-19")) + 
  theme_bw() +
  theme(axis.title.y = element_text(color = mov_color, size=13),
        axis.title.y.right = element_text(color = mu_color, size=13)) +
  labs(caption = "Fuente de los datos: JHU CSSE // apple.com/covid19/mobility") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  scale_x_date(name="", date_breaks = "30 day", date_labels =  "%d %b") +
  ggtitle("Movilidad vs Muertes por Covid-19 Argentina")
g1


# Muertes diarias por Covid
# Se quita valor outlier con datos[-which.max(datos$muertes_diarias),]

g2 <-  ggplot(datos[-which.max(datos$muertes_diarias),], aes(x=fecha)) +
  #geom_line( aes(y= movilidad), size=1, color=mov_color) + 
  geom_line( aes(y= muertes_diarias), size=1, color=mu_color) +
  scale_y_continuous(name = "Muertes por Covid-19") + 
  theme_bw() +
  theme(axis.title.y = element_text(color = mov_color, size=13)) +
  labs(caption = "Fuente de los datos: JHU CSSE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  scale_x_date(name= "",date_breaks = "30 day", date_labels =  "%d %b") +
  ggtitle("Muertes diarias por Covid-19 Argentina")
g2

# Muertes diarias y movilidad

mov_color <- rgb(0.2, 0.6, 0.9, 1)
mu_color <- "#69b3a2"
coeficiente_g3 <- 4 #Coeficiente utilizado para llevar a escala a la variable con el eje Y

g3 <-  ggplot(datos[-which.max(datos$muertes_diarias),], aes(x=fecha)) +
  geom_line( aes(y= movilidad), size=0.75, color=mov_color) + 
  geom_line( aes(y= muertes_diarias/coeficiente_g3), size=0.75, color=mu_color) +
  scale_y_continuous(name = "Movilidad (%)",sec.axis = sec_axis(~.*coeficiente_g3, name="Muertes diarias por Covid-19")) + 
  theme_bw() +
  theme(axis.title.y = element_text(color = mov_color, size=13),
        axis.title.y.right = element_text(color = mu_color, size=13)) +
  labs(caption = "Fuente de los datos: JHU CSSE // apple.com/covid19/mobility") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  scale_x_date(name="", date_breaks = "30 day", date_labels =  "%d %b") +
  ggtitle("Movilidad y Muertes Diarias por Covid-19 Argentina")
g3

######################################################
###################################################### prueba boxplots
######################################################
datos_box <- subset(datos, fecha >'2020-03-31')
library(lubridate)
datos_box$mes <- format(floor_date(datos_box$fecha, "month"), "%Y-%m")
library(ggplot2)
library(viridis)

# Boxplot basic
ggplot(datos_box[-which.max(datos_box$muertes_diarias),], aes(x=mes, y=muertes_diarias)) +
  geom_boxplot(outlier.colour="red") +
  scale_fill_viridis(discrete = TRUE, alpha=1, option="A") +
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")
#####################################################
#####################################################
#####################################################
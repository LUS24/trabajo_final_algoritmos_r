
############################################## Librerias

# Package names
packages <- c("tidyverse", "zoo", "patchwork", "hrbrthemes", "hrbrthemes", "rjson", "jsonlite", "plotly")

              
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

############################################## Leer Muertes por Covid-19
# time_series_19-covid-Deaths_archived_0325.csv
URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
nombre_archivo <- "time_series_covid19_deaths_global.csv"
url_archivo  <- paste0(URL,nombre_archivo)

# Para implementar el cacheo se me ocurrió colocar en el nombre del archivo la fecha que fue descargado
# En las subsiguientes descargas antes de descargar el archivo deberia leerse el último archivo descargado
# usando sort(list.files("../cache/"), decreasing = T) y compararlo con la fecha actual
# Si la fecha actual es mayor que la fecha que figura en el nombre del archivo se descarga el archivo nuevo
# Si la fecha actual es igual o menor (núnca debería ser menor) que la fecha que figura en el nombre del archivo
# Se lee el archivo.
# También, si la carpeta cache no existe, no tiene archivos o tiene archivos con el formato incorrecto, debería
# descargar el archivo


fecha_actual <- Sys.time()

COVID_19_h  <- read.csv(url_archivo, sep = ",", header = T)

COVID_19_h %>% write_csv(paste0("../cache/", format(fecha_actual, "%Y_%m_%d"), "_", nombre_archivo))

sort(list.files("../cache/"), decreasing = T)



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
mobility <- read.csv(url_apple_mobility_archivo, sep = ",", header = T)


############################################## preparo los datos Movilidad
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
Dia_Mayor_Movilidad[c(1,3)]
#el/los dias de menor tasa de movilidad
Dia_Menor_Movilidad <- subset(datos_inicio_cuarentena,movilidad == min(movilidad, na.rm = T)) #no se porque en el 1er run no devuelve nada <-- Hay que remover NAs
Dia_Menor_Movilidad[c(1,3)]

#el/los dias con menor cantidad de muertes
#Dia_Menor_NroMuertes<- subset(datos_inicio_cuarentena,muertes_diarias == min(muertes_diarias)) #0 muertes al inicio de la cuarentena y en navidad...
#Dia_Menor_NroMuertes[c(1,4)]

#el/los días con mayor cantidad de muertes
Dia_Mayor_NroMuertes<- subset(datos_inicio_cuarentena,muertes_diarias == max(muertes_diarias)) #revisar si mostrar este valor/dia porque creo que coincide cuando hubo ajustes en la registracion de casos por parte de GobAr.
Dia_Mayor_NroMuertes[c(1,4)]

#N° de muertos por millón de habitantes
Muertos_por_millon <- round(max(datos$muertes_covid19)/proyeccion_poblacion_arg,4)
Muertos_por_millon


############################################## revisando datos faltantes
sum(is.na(datos)) # hay (cuantos) NA en el df datos ??
apply(is.na(datos), 2, which)# busca los hay NA en el df datos, y te trae la columna y posicion.
datos$movilidad <- na.locf(datos$movilidad) #Reemplaza los valores faltantes con el valor del registro anterior
datos$muertes_diarias[is.na(datos$muertes_diarias)] <- 0 # reemplazo el 1er NA de la columna calculada de muertes diarias
sum(is.na(datos)) # busco de vuelta, si hay (cuantos) NA en el df datos

rownames(datos) <- NULL #Reseteando los indices


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


g1.1 <- plot_ly(datos, x = ~fecha, y = ~movilidad, type = "scatter", mode= "lines",name = "Movilidad (%)") %>%
  add_trace(x = ~fecha, y = ~muertes_covid19, mode = "lines", yaxis = "y2",name = "Muertes acumuladas por Covid-19") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right")) %>% layout(title = "Movilidad vs Muertes por Covid-19 Argentina",
                                                                     xaxis = list(title = "Fecha"),
                                                                     yaxis = list (title = "Movilidad (%)"))


g1.1



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
g2 <- ggplotly(g2)
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


g3.1 <- plot_ly(datos, x = ~fecha, y = ~movilidad, type = "scatter", mode= "lines",name = "Movilidad (%)") %>%
  add_trace(x = ~fecha, y = ~muertes_diarias, mode = "lines", yaxis = "y2",name = "Muertes diarias por Covid-19") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right")) %>% layout(title = "Movilidad y Muertes Diarias por Covid-19 Argentina",
                                                                     xaxis = list(title = "Fecha"),
                                                                     yaxis = list (title = "Movilidad (%)"))

g3.1

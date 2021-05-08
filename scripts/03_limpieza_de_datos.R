message("Limpiando datos")

############################################## Limpieza de los datos de muertes acumuladas por covid

# Busca en la carpeta muertes los archivos y los ordena por nombre de manera descendente,
# La fecha mas reciente queda en primer lugar
COVID_19_h <- read.csv(sort(list.files("../datos/muertes/", full.names = T), decreasing = T)[1])

COVID_19_h$Province.State <- NULL
COVID_19_h$Lat <- NULL
COVID_19_h$Long <- NULL

COVID_19_deaths <- COVID_19_h %>% gather(fecha, casos, 2:ncol(COVID_19_h))
colnames(COVID_19_deaths) <- c("pais", "fecha", "casos")
COVID_19_deaths$fecha <- as.Date(as.character(COVID_19_deaths$fecha), format = "X%m.%d.%y")
argentina_deaths<- subset(COVID_19_deaths,pais == 'Argentina', -pais)


############################################## Limpieza de los datos de movilidad
mobility <- read.csv(sort(list.files("../datos/mobility/",full.names = T), decreasing = T)[1])

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

############################################## Unión de datasets
datos <- merge(argentina_deaths,argentina_mobility, by = "fecha")
colnames(datos) <- c("fecha","muertes_covid19", "movilidad")

############################################## Calculo de las muertes por día (por diferencia)
datos$muertes_diarias <- datos$muertes_covid19 - lag(datos$muertes_covid19, n = 1)

# TODO: Calcular diferencia también por diferencia de logaritmos para tener una aproximación a la tasa de
# Crecimiento

############################################## Lidiando con datos faltantes
# sum(is.na(datos)) # N° de NAs en el set de datos
# apply(is.na(datos), 2, which)# busca los hay NA en el df datos, y te trae la columna y posicion.

#Reemplaza los valores faltantes de movilidad con el valor del último registro previo que no es un NA
datos$movilidad <- na.locf(datos$movilidad)

# reemplazo el 1er NA de la columna calculada de muertes diarias por 0
datos$muertes_diarias[is.na(datos$muertes_diarias)] <- 0

# Se verifica si quedaron NAs
# sum(is.na(datos))

############################################## Lidiando valor anómalo

# Se encontró un valor anómalo correspondiente a la fecha 01/10/2020 donde se informaron
# 3351 muertes en un mismo día, esto se debió a que se cargaron datos de varias semanas
# Nota periodística: https://www.infobae.com/sociedad/2020/10/02/coronavirus-fueron-cargados-los-mas-de-3000-muertos-de-buenos-aires-y-argentina-marco-la-mayor-cantidad-de-fallecidos-por-millon-de-habitantes-en-un-dia/

# TODO: se distribuye los valores del outlier en los días de 3 semanas anteriores

datos <- datos %>% 
  filter(muertes_diarias != max(muertes_diarias, na.rm = T))

############################################## Se guardan los datos limpios
dir.create(file.path("..", "datos", "limpios"),recursive = TRUE)
write.csv(datos, file.path("..", "datos", "limpios", "datos_limpios_argentina.csv"), fileEncoding = "UTF-8", row.names = F)


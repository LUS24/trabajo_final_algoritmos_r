message("Analizando datos")

############################################## Limpieza inicial de entorno de trabajo

rm(list = ls())

############################################## Carga de datos

datos_argentina <- read.csv(file.path("..", "datos", "limpios", "datos_limpios_argentina.csv"))

############################################## Cálculo de indicadores

# Proyecciones (al 2020) de la población Argentinadel
# Fuente: indec https://www.argentina.gob.ar/pais/poblacion/proyecciones 
proyeccion_poblacion_arg <- 45376763

# Datos desde la fecha de inicio de la cuarentena
datos_inicio_cuarentena <- subset(datos_argentina, fecha >'2020-03-19')

# Día de mayor movilidad
Dia_Mayor_Movilidad <- subset(datos_inicio_cuarentena, movilidad == max(movilidad))
Dia_Mayor_Movilidad[c(1,3)]

#Día con mayor cantidad de muertes
Dia_Mayor_NroMuertes<- subset(datos_inicio_cuarentena,muertes_diarias == max(muertes_diarias))
Dia_Mayor_NroMuertes[c(1,4)]

#N° de muertos por millón de habitantes
Muertos_por_millon <- round(max(datos_inicio_cuarentena$muertes_covid19)/proyeccion_poblacion_arg * 1000000)
Muertos_por_millon
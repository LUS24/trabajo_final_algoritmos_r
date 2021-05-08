## Ejecutar scripts del proyecto
options(warn=-1,
        encoding = 'UTF-8')

run_project <- function(){
  message("Ejecutando pipeline del proyecto")
  source("./01_carga_de_paquetes.R")
  source("./02_descargar_datos.R")
  source("./03_limpieza_de_datos.R")
  source("./04_analisis_de_datos.R")
  source("./05_creacion_de_graficos_y_vistas.R")
  rmarkdown::render("./06_creacion_de_dashboard.rmd", 
                    output_file = file.path("..", "reportes", "covid_2019_muertes_movi_dashboard.html"),
                    encoding = "UTF-8")
}

run_project()

file.show(file.path("..", "reportes", "covid_2019_muertes_movi_dashboard.html"))

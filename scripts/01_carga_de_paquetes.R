message("Cargando paquetes")

# Paquetes a cargar
packages <- c("tidyverse", 
              "zoo", 
              "patchwork", 
              "hrbrthemes", 
              "hrbrthemes", 
              "rjson", 
              "jsonlite", 
              "plotly", 
              "viridis", 
              "DT",
              "lubridate",
              "rmarkdown",
              "flexdashboard")

              
# Instalar paquetes que no están instalados
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Cargar paquetes
invisible(lapply(packages, library, character.only = TRUE))
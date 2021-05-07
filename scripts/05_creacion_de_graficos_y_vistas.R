message("Creando gráficos y vistas")

datos_argentina <- read.csv(file.path("..", "datos", "limpios", "datos_limpios_argentina.csv"), 
                            colClasses = c("Date", "numeric", "numeric", "numeric"))



mov_color <- rgb(0.2, 0.6, 0.9, 1)
mu_color <- "#69b3a2"
coefi <- 500 #Coeficiente utilizado para llevar a escala a la variable con el eje Y

# Movilidad vs Muertes acumuladas por Covid
mov_vs_muertes_acum <- 
  plot_ly(datos_argentina, 
                x = ~fecha, 
                y = ~movilidad, 
                type = "scatter", 
                mode= "lines",
                name = "Movilidad (%)") %>%
  add_trace(x = ~fecha, 
            y = ~muertes_covid19, 
            mode = "lines", 
            yaxis = "y2", 
            name = "Muertes acumuladas por Covid-19") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right")) %>% 
  layout(title = "Movilidad vs Muertes por Covid-19 Argentina",
         xaxis = list(title = "Fecha"),
         yaxis = list (title = "Movilidad (%)"))

# Muertes diarias por Covid vs Movilidad

mov_vs_muertes_diarias <- 
  plot_ly(datos_argentina, 
          x = ~fecha, 
          y = ~movilidad, 
          type = "scatter", 
          mode= "lines",
          name = "Movilidad (%)") %>%
  add_trace(x = ~fecha, 
            y = ~muertes_diarias, 
            mode = "lines", 
            yaxis = "y2",
            name = "Muertes diarias por Covid-19") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right")) %>% 
  layout(title = "Movilidad y Muertes Diarias por Covid-19 Argentina",
         xaxis = list(title = "Fecha"),
         yaxis = list (title = "Movilidad (%)"))

# Boxplots de datos agrupados por mes

boxplot_muertes_diarias_por_mes <- 
  datos_argentina %>% 
    mutate(mes = format(floor_date(fecha, "month"), "%Y-%m")) %>% 
    ggplot(aes(x=mes, y=muertes_diarias)) +
      geom_boxplot(outlier.colour="red") +
      scale_fill_viridis(discrete = TRUE, alpha=1, option="A") +
      theme_classic() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11, hjust = 0.5)
      ) +
    labs(title="Muertes diarias acumuladas por mes", x= "", y="Muertes diarias")

boxplot_muertes_diarias_por_mes <- ggplotly(boxplot_muertes_diarias_por_mes)

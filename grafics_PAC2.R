#  Carreguem llibreries i fitxer CSV

library(tidyverse)
library(igraph)



# Gràfic en spiral

seattle <- read.csv("seattleWeather_1948-2017.csv", header = TRUE, dec = ".", sep = ",")


# Transformem de graus Farenheit a graus Celcius les columnes TMAX i TMIN, creem columnes per any, 
#mes i dia per separat. També creem un numerador.

seattle_transf<- seattle %>% 
        mutate(TMAX_C = round((TMAX - 32) * 5/9, 2),
               TMIN_C = round((TMIN - 32) * 5/9, 2),
               YEAR = year(DATE),
               MONTH = month(DATE),
               DAY = day(DATE)) %>% 
        filter(DATE >= "2015-01-01") %>% 
        mutate(DAY_NUM = 1:length(DATE))

                     
# Construim el gràfic en espiral. Podem observar els mesos més calurosos contra els mesos més freds

ggplot(seattle_transf, aes(DAY_NUM %% 365,
                            0.15*DAY_NUM + TMAX_C/2, height = TMAX_C, fill = TMAX_C)) + 
        geom_tile() +
        scale_y_continuous(limits = c(-20, NA)) +
        scale_x_continuous(breaks = 30*0:11, minor_breaks = NULL, labels = month.abb) +
        coord_polar() + 
        scale_fill_viridis_c() + theme_minimal() +
        scale_fill_gradient2(low="blue", mid="#FFFFA4", high="red", midpoint=10) +
        labs(title = "Evolució de temperatura màxima",
             x = "Mes",
             y = "Temperatura (°C)",
             fill = "Temp. màx.")


# Bibliografia:

# https://stackoverflow.com/questions/52939337/how-to-create-a-time-series-spiral-graph-using-r




# Crear un grafo de ejemplo

nodes <- data.frame(name = c("A", "B", "C", "D", "E"))
edges <- data.frame(from = c("A", "A", "A", "B", "C", "D"), 
                    to = c("B", "C", "D", "E", "D", "E")) # Convertir a objeto grafo 
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE) # Configurar el layout dirigido por fuerza 
layout <- layout_with_fr(g) # Plotear el grafo 
plot(g, layout = layout)
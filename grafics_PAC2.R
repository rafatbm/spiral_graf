#  Carreguem llibreries i fitxer CSV

library(tidyverse)
library(igraph)
library(ggraph)



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




# Modifiquem el fitxer que hem trobat a Kaggle sobre accidents a la ciutat de Barcelona per treballar.lo amb
# Tableau.

barcelona <- read.csv("bcn_2016_accidents.csv", header = TRUE, sep = ",")

# Seleccionem nomes aquelles columnes amb les quals treballarem.

barcelona_accidents <- barcelona %>%
        mutate(Accidents = 1) %>% 
        filter(Nom.districte != "Desconegut") %>% 
        select("Nom.districte","Nom.barri","Mes.de.any","Accidents") %>% 
        group_by(Nom.districte,Nom.barri,Mes.de.any) %>% 
        summarize(Accidents = sum(Accidents)) %>%
        ungroup() %>%
        #select("Nom.districte", "Nom.barri", "Dia.setmana","Mes.de.any","Accidents") %>% 
        arrange(Nom.districte)

# Contruim un df amb les arestes basat en els accidents

edges <- barcelona_accidents %>% 
        select(Nom.districte, Nom.barri)

# Creem el graf

g <- graph_from_data_frame(d = edges, directed = FALSE)



# Gràfic

ggraph(g, layout = "fr") + 
        geom_edge_link(aes(edge_alpha = 0.8, edge_width = 0.5)) + 
        geom_node_point(aes(color = factor(name)), size = 5) + 
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void() +
        labs(title = "Force-Directed Graph de Accidentes")

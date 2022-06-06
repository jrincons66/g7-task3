# Universidad de los Andes 
# Taller R y estadistica 
# Task 2: CHIP
# Integrantes: 1. Diana Carolina Bastidas Melo   
#              Codigo: 201921893                
#              2.Jose Alejandro Rincon 
#              Codigo: 202013328                          
# R version 4.1.2

#Para comenzar a desarrollar el taller, debemos limpiar el entorno y posteriormente,instalar los paquetes necesarios

rm(list=ls())
setwd("C:/Users/josea/OneDrive/Escritorio/UNIANDES/Taller de R/task-3/")
require(pacman) # require pacman
p_load(tidyverse,rio,sf,leaflet,rvest, # require and/or install packages
       osmdata, 
       xml2,  
       ggsn,    
       XML,modelsummary, stargazer,ggplot2,margins) 
## Taller A --------------------------------------------------------------------
#1. Datos Espaciales 
###Descargar datos:

getbb("Bogotá Colombia")

# Estaciones de Bus, Vias principales , MIO , Limites de barrios.
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf()
osm %>% class()
osm

amenities = osm$osm_points %>% select(osm_id,amenity)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=amenities , weight=1 , col="green")

street = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

street = street$osm_lines %>% select(osm_id,name)
street = street %>%
  subset(str_detect(name,"Avenida")==T | str_detect(name,"transmilenio")==T)

leaflet() %>% 
  addTiles() %>% 
  addPolylines(data=street, col="red")

## load data
load("input/mhv_blocks_bog.rds")
leaflet() %>% addTiles() %>% addPolygons(data=mhv_bog, col = "blue")

### Visualizar informacion:

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=amenities , weight=1 , col="green") %>% 
  addPolylines(data=street, col="red") %>% addPolygons(data=mhv_bog, col = "blue")

##  Estimar distancias:
c_barrios = st_centroid(x= mhv_bog)
c_bus = st_centroid(x = amenities)
bus_barrios = st_crop(x=c_bus , y=st_bbox(c_barrios))
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_barrios,col="red") %>% addCircleMarkers(data=c_barrios,col="blue")
mean_bs = st_distance(c_barrios , c_bus)
mhv_bog$mean_bs = mean_bs
mhv_bog
mhv_bog$MANZ_CCNCT = NULL
mhv_bog$MANZ_CCNCT = NULL
mhv_bog$mhv = NULL
bs = plot(mhv_bog) 
ggsave(bs,filename = "output/map_bogota_bs.pdf" , width=6.5 , height=8)

c_street = st_centroid(x = street)
street_barrios = st_crop(x=c_street , y=st_bbox(c_barrios))
leaflet() %>% addTiles() %>% addCircleMarkers(data=street_barrios,col="red") %>% addCircleMarkers(data=c_barrios,col="blue")
leaflet() %>% addTiles() %>% addCircleMarkers(data=c_street,col="blue")  
via = st_distance(c_barrios , c_street)
mean_via = apply(via,1,mean)
mhv_bog$mean_via = mean_via
mhv_bog
mhv_bog$mean_bs = NULL
via_map = plot(mhv_bog)

#2. Regresiones (35%)

rm(list=ls())
base = readRDS("C:/Users/josea/OneDrive/Escritorio/UNIANDES/Taller de R/task-3/input/f_mapmuse.rds")
variable.names(base)
ols = lm(fallecido ~ tipo_accidente + year + month + condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data = base)

grafo = modelplot(ols) + coord_flip() + 
  labs(title = "Probability to die" , subtitle = "MAP") 
capture.output(grafo , file = "output/grafo.jpeg")

logit = glm(fallecido ~ tipo_accidente + year + month + condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data = base,  family = binomial(link="logit"))

probit = logit = glm(fallecido ~ tipo_accidente + year + month + condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data = base,  family = binomial(link="probit"))

resultados = stargazer(ols , logit , probit,  type = "text")
export(resultados, "output/resultados.doc")
capture.output(resultados , file = "output/resultados.doc")

modelos.plot=function(
  Logit=logit ,
  Probit=probit ,
  Plotted_Variables="dist_hospi"
){
logit_marg = margins(logit, variables = Plotted_Variables)
logit_marg %>% tidy(conf.int = TRUE)
probit_marg = margins(probit, variables= Plotted_Variables)
probit_marg %>% tidy(conf.int = TRUE)
mods = list('Logit' = logit_marg , 'Probit' = probit_marg , "OLS" = ols)
OLS_marg = margins(ols, variables= Plotted_Variables)
Resumen_modelo <- modelplot(mods) + coord_flip() + labs(title = "El efecto marginal sobre la probabibilidad de muerte" , subtitle = "Modelo lineal")
}
modelos.plot()
capture.output(Resumen_modelo , file = "output/Resumen_modelp.jpeg")

#3. Web-scraping (15%)
myurl="https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
myhtml = read_html(myurl)
class(myhtml)
titulo = myhtml %>% html_nodes("h1") %>% html_text() 
parse = read_html(myurl) %>% htmlParse()
tablas = parse %>% readHTMLTable(header = T)
tablas[[4]] %>% class()
tablas[[4]]
Dpts_col = tablas[[4]]


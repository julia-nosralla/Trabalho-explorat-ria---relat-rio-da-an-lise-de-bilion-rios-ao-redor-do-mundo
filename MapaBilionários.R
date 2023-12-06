library(leaflet)
library(tidyverse)
library(ggplot2)
library(sf)
library(raster)
library(RColorBrewer)
library(htmlwidgets)

spdf_fortified$NAME[spdf_fortified$NAME == "Korea, Republic of"] <- "South Korea"
spdf_fortified$NAME[spdf_fortified$NAME == "United Republic of Tanzania"] <- "Tanzania"
spdf_fortified$NAME[spdf_fortified$NAME == "Viet Nam"] <- "Vietnam"

join_data <- full_join(spdf_fortified, paises1, by = "NAME")

mapa <- shapefile("shapefile mundo/TM_WORLD_BORDERS_SIMPL-0.3.shp")

mybins <- c(0,10,20,50,100,500,800)
mypalette <- colorBin(palette="YlOrBr", domain= join_data$n, na.color="transparent", bins=mybins)

mytext <- paste(
  "País: ", join_data$NAME,"<br/>", 
  "Número de bilionários: ", join_data$n, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa) %>% 
  addTiles()  %>% 
  setView(lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(join_data$n), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend(pal=mypalette, values=~join_data$n, opacity=0.9, title = "Concentração de bilionários", position = "bottomleft" )

m 
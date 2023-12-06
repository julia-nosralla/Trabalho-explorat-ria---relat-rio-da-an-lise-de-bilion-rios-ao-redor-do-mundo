library(highcharter)
library(tidyverse)
library(patchwork)
library(hrbrthemes)
library(circlize)

banco1 <- banco[,36:37]

banco1 <- banco1 %>% 
  select(continentOfCitizenship, continent) %>%
  group_by(continentOfCitizenship, continent)%>%
  summarise(total = n())

banco1 <- na.omit(banco1)

colnames(banco1) <- c("Continente de origem", "Continente de destino", "Migração")

library(networkD3)

banco1$`Continente de origem` <- paste(banco1$`Continente de origem`, " ", sep="")

nodes <- data.frame(name=c(as.character(banco1$`Continente de destino`), as.character(banco1$`Continente de origem`)) %>% unique())

banco1$IDcontinentOfCitizenship=match(banco1$`Continente de origem`, nodes$name)-1 
banco1$IDcontinent=match(banco1$`Continente de destino`, nodes$name)-1

ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

sankeyNetwork(Links = banco1, Nodes = nodes,
              Source = "IDcontinentOfCitizenship", Target = "IDcontinent",
              Value = "Migração", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

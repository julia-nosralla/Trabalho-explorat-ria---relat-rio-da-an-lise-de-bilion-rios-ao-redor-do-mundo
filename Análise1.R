library(leaflet)
library(tidyverse)
library(naniar)
library(countrycode)

banco <- read.csv("Billionaires Statistics Dataset (1).csv", na.strings = "")

banco$continent <-countrycode(sourcevar = banco[, "country"], 
                              origin = "country.name",
                              destination = "continent")

banco$continentOfCitizenship <-countrycode(sourcevar = banco[, "countryOfCitizenship"], 
                              origin = "country.name",
                              destination = "continent")

paises1 <- banco %>%
  filter(! duplicated(personName)) %>%
  group_by(country) %>%
  count()

paises1 <- rename(paises1, NAME = country)

paises1 <- na.omit(paises1)

Tabela1 <- banco %>%
  filter(! duplicated(personName)) %>%
  group_by(continent) %>%
  count()

Tabela1 <- na.omit(Tabela1)
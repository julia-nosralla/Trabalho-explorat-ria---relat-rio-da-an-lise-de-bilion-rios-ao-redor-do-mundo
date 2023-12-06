library(tidyverse)

paises1 %>% 
  arrange(desc(n)) %>% 
  {.[0:15,]}


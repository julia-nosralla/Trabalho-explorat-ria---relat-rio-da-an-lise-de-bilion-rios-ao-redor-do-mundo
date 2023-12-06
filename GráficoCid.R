library(tidyverse)
library(scales)
library(ggthemes)

cidadania <- banco %>%
  filter(!is.na(continentOfCitizenship)) %>%
  filter(! duplicated(personName)) %>%
  count(continentOfCitizenship) %>%
  mutate(continentOfCitizenship = case_when(
    continentOfCitizenship %>% str_detect("Asia") ~ "Ásia",
    continentOfCitizenship %>% str_detect("Americas") ~ "Américas",
    continentOfCitizenship %>% str_detect("Europe") ~ "Europa",
    continentOfCitizenship %>% str_detect("Oceania") ~ "Oceania",
    continentOfCitizenship %>% str_detect("Africa") ~ "África"
  )) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(cidadania) +
  aes(x = fct_reorder(continentOfCitizenship, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A1D68B", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0,1100)) +
  labs(x = "Continente de origem", y = "Número de bilionários") +
  theme_minimal() +
  scale_fill_brewer(palette = "Accent")
  

ggsave("colunas-uni-freq2.pdf", width = 158, height = 93, units = "mm")
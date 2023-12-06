library(tidyverse)
library(scales)
library(ggthemes)

continente <- banco %>%
  filter(!is.na(continent)) %>%
  filter(! duplicated(personName)) %>%
  count(continent) %>%
  mutate(continent = case_when(
    continent %>% str_detect("Asia") ~ "Ásia",
    continent %>% str_detect("Americas") ~ "Américas",
    continent %>% str_detect("Europe") ~ "Europa",
    continent %>% str_detect("Oceania") ~ "Oceania",
    continent %>% str_detect("Africa") ~ "África"
  )) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(continente) +
  aes(x = fct_reorder(continent, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A1D68B", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0,1100)) +
  labs(x = "Continente", y = "Número de bilionários") +
  theme_minimal()

ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm")
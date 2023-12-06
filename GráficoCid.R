library(tidyverse)
library(scales)

cores <- c("#e11f25", "#749e5d", "#701d1a", "#e4b784", "#7d4f1b", "#999966", "#006606", "#008091", "#041835", "#666666")
theme_1 <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores),
      scale_colour_manual(values = cores)
    )
  )
}

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
  geom_bar(stat = "identity", fill = "#e11f25", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0,1100)) +
  labs(x = "Continente de origem", y = "Número de bilionários") +
  theme_1()

ggsave("colunas-uni-freq2.pdf", width = 158, height = 93, units = "mm")
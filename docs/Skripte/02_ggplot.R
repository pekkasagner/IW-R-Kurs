library(tidyverse)

# Funktionen für das Laden der Dateien im .csv oder .xls Format
read_custom_csv <- function(path, value_name) {
  data <- 
    read_csv2(path,
              skip = 1) %>% 
    rename('Kennziffer' = 'X1',
           'Raumeinheit' = 'X2',
           'Aggregat' = 'X3') %>% 
    pivot_longer(!c(Kennziffer, Raumeinheit, Aggregat), 
                 names_to = 'Jahr', 
                 values_to = value_name) %>% 
    mutate(Jahr = as.double(Jahr)) 
  return(data)
}

read_custom_xls <- function(path, value_name) {
  data <- 
    readxl::read_xls(path,
                     skip = 1) %>% 
    rename('Kennziffer' = 1,
           'Raumeinheit' = 2,
           'Aggregat' = 3) %>% 
    pivot_longer(!c(Kennziffer, Raumeinheit, Aggregat), 
                 names_to = 'Jahr', 
                 values_to = value_name) %>% 
    mutate(Jahr = as.double(Jahr)) 
  return(data)
}


## Laden der Daten
Arbeitslosenquote <- 
  read_custom_xls('daten_beispiele/Arbeitslosenquote.xls',
                  value_name = 'Arbeitslosenquote')

Wahlbeteiligung <- 
  read_custom_csv('daten_beispiele/Wahlbeteiligung.csv',
                  value_name = 'Wahlbeteiligung') %>% 
  select(-c(Raumeinheit, Aggregat))

Durchschnittsalter <- 
  read_custom_csv('daten_beispiele/Durchschnittsalter_Kreise.csv',
                  value_name = 'Durchschnittsalter')

# Join der Daten in ein kohärentes tidy data frame
data <-
  Arbeitslosenquote %>% 
  left_join(Wahlbeteiligung, by = c("Kennziffer", "Jahr")) %>% 
  right_join(Durchschnittsalter, by = c("Kennziffer", "Jahr", 
                                        "Raumeinheit", "Aggregat"))

# Scatter Plot
data %>% 
  filter(Jahr == 2017) %>% 
  ggplot(aes(x = Wahlbeteiligung, y = Arbeitslosenquote)) +
  geom_point() +
  geom_smooth(method = "lm",
              col = "grey40") +
  theme_minimal()

# Facet scatter plot
data %>% 
  filter(!is.na(Wahlbeteiligung)) %>% 
  ggplot(aes(x = Wahlbeteiligung, y = Arbeitslosenquote,
             col = Durchschnittsalter)) + 
  geom_point() +
  scale_color_viridis_c(direction = -1) +
  facet_wrap(~Jahr) +
  labs(title = "Verhältnis von Wahlbeteiligung, Arbeitslosenquote und Durchschnittsalter",
       subtitle = "Deutsche Landkreise und kreisfreie Städte (1998-2017)") +
  theme_minimal() +
  theme(plot.title.position = 'plot')

# ggsave('facet_plot.pdf')

# Line plot (Durchschnittswahlalter über Zeit)
data %>% 
  filter(!is.na(Wahlbeteiligung)) %>% 
  group_by(Jahr) %>% 
  summarise(Arbeitslosenquote = mean(Arbeitslosenquote)) %>% 
  ggplot(aes(x = Jahr, y = Arbeitslosenquote)) +
  geom_line(size = 1.0) +
  theme_minimal()

# Barplot
data %>% 
  filter(!is.na(Wahlbeteiligung),
         Jahr == 2017) %>% 
  sample_n(10) %>% 
  ggplot(aes(x = fct_reorder(Raumeinheit, Wahlbeteiligung), 
             y = Wahlbeteiligung)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal()

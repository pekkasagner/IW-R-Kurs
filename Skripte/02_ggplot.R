library(tidyverse)

# Funktionen für das Laden der Dateien im .csv Format
read_custom_csv <- function(path, value_name) {
  data <- 
    read_csv2(path,
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

# Funktion für das Laden der Dateien im .xls Format
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

BIP_Einwohner <- 
  read_custom_xls('daten_beispiele/BIP_je_Einwohner.xls',
                  value_name = 'BIP') %>% 
  select(-c(Raumeinheit, Aggregat))

Stadt_Land <- 
  read_custom_xls('daten_beispiele/Stadt_Land.xls',
                  value_name = 'Stadt_Land') %>% 
  select(-c(Raumeinheit, Aggregat)) %>% 
  mutate(Stadt_Land = Stadt_Land - 1)


# Join der Daten in ein kohärentes tidy data frame
Landkreise <-
  Arbeitslosenquote %>% 
  left_join(Wahlbeteiligung, by = c("Kennziffer", "Jahr")) %>% 
  right_join(Durchschnittsalter, by = c("Kennziffer", "Jahr", 
                                        "Raumeinheit", "Aggregat")) %>% 
  left_join(BIP_Einwohner, by = c("Kennziffer", "Jahr")) %>% 
  left_join(Stadt_Land, by = c("Kennziffer", "Jahr"))

write_csv(Landkreise, 'daten_beispiele/Landkreise_merged.csv')

theme_set(theme_minimal())

# Scatter Plot
data %>% 
  filter(Jahr == 2017) %>% 
  ggplot(aes(x = Wahlbeteiligung, y = Arbeitslosenquote)) +
  geom_point()

# aesthetics size
data %>% 
  filter(Jahr == 2017) %>% 
  ggplot(aes(x = Wahlbeteiligung, 
             y = Arbeitslosenquote,
             size = Durchschnittsalter)) +
  geom_point()

# aestetics color
data %>% 
  filter(Jahr == 2017) %>% 
  ggplot(aes(x = Wahlbeteiligung, 
             y = Arbeitslosenquote,
             color = Durchschnittsalter)) +
  geom_point()

# geom point
data %>% 
  filter(Jahr == 2017) %>% 
  ggplot(aes(x = Wahlbeteiligung, 
             y = Arbeitslosenquote,
             color = Durchschnittsalter)) +
  geom_point()

# geom line
data %>% 
  filter(Raumeinheit %in% c("Elbe-Elster", "Oberspreewald-Lausitz",
                            "Prignitz", "Spree-Neiße", "Vogtlandkreis"),
         Jahr > 1997) %>% 
  ggplot(aes(x = Jahr,
             y = Arbeitslosenquote,
             col = Raumeinheit)) +
  geom_line(size = 1.0)

# geom bar
data %>% 
  filter(Raumeinheit %in% c("Elbe-Elster", "Oberspreewald-Lausitz",
                            "Prignitz", "Spree-Neiße", "Vogtlandkreis"),
         Jahr == 1998) %>% 
  ggplot(aes(x = Raumeinheit,
             y = Arbeitslosenquote)) +
  geom_bar(stat = "identity")


  






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

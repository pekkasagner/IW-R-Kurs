library(tidyverse)
library(IWcoloRs)

# Einlesen der Daten
Arbeitslosenquote <- 
  readxl::read_xls('daten_beispiele/Arbeitslosenquote.xls',
                         skip = 1) %>% 
  rename('Kennziffer' = '...1',
         'Raumeinheit' = '...2',
         'Aggregat' = '...3') %>% 
  pivot_longer(!c(Kennziffer, Raumeinheit, Aggregat), 
               names_to = 'Jahr', 
               values_to = 'Arbeitslosenquote')

# EDA
glimpse(Arbeitslosenquote)

# Wie hat sich die Arbeitslosenquote über die Zeit entwickelt?
Arbeitslosenquote %>% 
  group_by(Jahr) %>% 
  summarise(Arbeitslosenquote = mean(Arbeitslosenquote))



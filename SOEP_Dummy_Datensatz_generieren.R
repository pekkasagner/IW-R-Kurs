#SOEP Dummy Daten aufbereiten
rm(list = ls())
library(haven)
library(tidyverse)
library(EnvStats)
set.seed(58008) 

(soep_lebensz <- haven::read_dta("daten_beispiele/soep_dummy/soep_lebensz.dta") %>% 
  mutate(weight = runif(12922,0,1000)) %>% 
    sample_frac(1L) %>% 
    mutate(income = EnvStats::rpareto(12922, 1000, 1)) %>% 
    mutate(income = ifelse(jahr == 2001, income * 1.2, income )) %>% 
    mutate(income = ifelse(jahr == 2002, income * 1.35, income )) %>% 
    mutate(income = ifelse(jahr == 2003, income * 1.45, income )) %>% 
    mutate(income = ifelse(jahr == 2004, income * 1.4, income )) 
)

  
(ppathl <- soep_lebensz %>% 
  select(persnr, jahr, sex, weight) %>% 
  sample_frac(1L)
)
 
(phealth <- soep_lebensz %>% 
  select(persnr, jahr, gesund_org, gesund_std, lebensz_org, lebensz_std) %>% 
  sample_frac(.95) 
)

(pl <- soep_lebensz %>% 
  select(persnr, jahr, bildung, anz_kind, income) %>% 
  sample_frac(1L) %>% 
  mutate(bula = sample(1:16, 12922, replace = TRUE, prob = NULL)) %>% 
  mutate(region = sample(c("Stadt", "Stadt", "Land"), 12922, replace = TRUE, prob = NULL))
)



haven::write_dta(ppathl, path = "daten_beispiele/soep_dummy/ppathl.dta")
haven::write_dta(phealth, path = "daten_beispiele/soep_dummy/phealth.dta")
haven::write_sav(pl, path = "daten_beispiele/soep_dummy/pl.sav")


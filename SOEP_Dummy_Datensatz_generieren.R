#SOEP Dummy Daten aufbereiten
rm(list = ls())
library(haven)
library(tidyverse)
library(EnvStats)
set.seed(58008) 

(soep_lebensz <- haven::read_dta("daten_beispiele/soep_dummy/soep_lebensz.dta") %>% 
  mutate(weight = runif(12922,0,1000)) %>% 
    sample_frac(1L) %>% 
    mutate(income = EnvStats::rpareto(12922, 1000, 1))
)
  
(ppathl <- soep_lebensz %>% 
  select(persnr, jahr, sex, weight) %>% 
  sample_frac(1L)
)
 
(phealth <- soep_lebensz %>% 
  select(persnr, jahr, gesund_org, gesund_std, lebensz_org, lebensz_std) %>% 
  sample_frac(1L)
)

(pl <- soep_lebensz %>% 
  select(persnr, jahr, bildung, anz_kind) %>% 
  sample_frac(1L) %>% 
  mutate(bula = sample(1:16, 12922, replace = TRUE, prob = NULL)) %>% 
  mutate(region = sample(c("Stadt", "Stadt", "Land"), 12922, replace = TRUE, prob = NULL))
)

haven::write_dta(ppathl, path = "daten_beispiele/soep_dummy/ppathl.dta")
haven::write_dta(phealth, path = "daten_beispiele/soep_dummy/phealth.dta")
haven::write_dta(pl, path = "daten_beispiele/soep_dummy/pl.dta")


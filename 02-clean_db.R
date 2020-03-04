
# paquetes ----------------------------------------------------------------

library(mosaicData)
library(tidyverse)
library(skimr)
library(naniar)
library(compareGroups)
library(janitor)
library(epiR)
library(broom)
library(avallecam)

# importar ----------------------------------------------------------------

data("Whickham")
smoke <- Whickham %>% as_tibble()
smoke %>% miss_var_summary()
smoke %>% skim()

# limpieza ----------------------------------------------------------------

smoke_clean <- smoke %>% 
  mutate(
    #desenlace
    outcome_1=as.numeric(outcome),
    outcome_1=outcome_1-1,
    outcome_2=fct_rev(outcome),
    #exposición
    smoker_2=fct_rev(smoker),
    #confusor
    #agegrp=cut(age,breaks = c(18,44,64,Inf),include.lowest = T))
    agegrp=case_when(
      age %in% 18:44 ~ "18-44",
      age %in% 45:64 ~ "45-64",
      age > 64 ~ "65+"),
    agegrp=as.factor(agegrp)
  )

write_rds(smoke_clean,"data/smokeclean_20190906.rds")
write_csv(smoke_clean,"data/smokeclean_20190906.csv")

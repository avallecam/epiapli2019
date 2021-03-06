
# paquetes ----------------------------------------------------------------

library(mosaicData)
library(tidyverse)
library(skimr)
library(rlang)
library(naniar)
library(compareGroups)
library(janitor)
library(epiR)
library(epiDisplay)
library(broom)
library(writexl)
library(avallecam)

# importar ----------------------------------------------------------------

data("Whickham")
smoke <- Whickham %>% as_tibble()
smoke %>% miss_var_summary()
smoke %>% skim()

# limpieza ----------------------------------------------------------------

smoke_clean <- read_rds("data/smokeclean_20190906.rds")

# tabla 1 y 2 -------------------------------------------------------------
# byrow = T / show.all = T,sd.type = 2
# export xls


# modelo ------------------------------------------------------------------
# outcome_1
# epi_tidymodel_rr

smoke_clean %>% glimpse()

#simple

#multiple: controlar por confusión

#exportar





# medidas de asociación ---------------------------------------------------

#epiR
smoke_tab1 <- with(smoke_clean,table(smoker_2,outcome_2)) %>% print()
epi.2by2(smoke_tab1,method = "cohort.count")

#epiDisplay
smoke_tab2 <- with(smoke_clean,table(outcome,smoker)) %>% print()
cs(cctable = smoke_tab2)

# evaluar confusión -------------------------------------------------------

#Mantel-Haenszel
smoke_tab3 <- with(smoke_clean,table(smoker_2,outcome_2,agegrp)) %>% print()
epi.2by2(smoke_tab3,method = "cohort.count")
mhor(mhtable=smoke_tab3,graph = F,design = "cohort")





# time to event -----------------------------------------------------------

library(survival)
library(survminer)
url <- "http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/valung.csv"
valung <- read_csv(url) %>%
  mutate(dead=as.factor(dead),
         dead_1=as.numeric(dead)-1)
valung %>% skim()
valung %>% miss_var_summary()
fit <- survfit(Surv(t, dead_1) ~ therapy, data =  valung)
fit %>% broom::tidy() %>% print(n=Inf)
ggsurvplot(fit, data = valung, risk.table = TRUE,
           conf.int = T,ggtheme = theme_bw(),#ggtheme = theme_minimal(),
           pval = T,censor=FALSE,tables.theme = theme_cleantable())

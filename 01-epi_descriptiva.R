#' tutorial fuente
#' https://www.reconlearn.org/post/real-time-response-1.html
#' 
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(naniar)
library(compareGroups)

# importar ----------------------------------------------------------------

#https://github.com/reconhub/learn/raw/master/static/data/linelist_20140701.xlsx
casos <- read_xlsx("data-raw/linelist_20140701.xlsx")
casos %>% glimpse()

# limpiar -----------------------------------------------------------------

casos_limpio <- casos %>% 
  mutate_at(.vars = vars(starts_with("date_")),.funs = ymd)

# evaluar -----------------------------------------------------------------

casos_limpio %>% miss_var_summary()

# guardar -----------------------------------------------------------------

write_rds(casos_limpio,"data/casoslimpio_20190916.rds")

# en tiempo ---------------------------------------------------------------

casos_limpio %>% 
  ggplot(aes(date_of_onset)) +
  geom_histogram(binwidth = 7,color="white") +
  scale_x_date(date_breaks = "7 day",date_labels = "%b-%d") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# en espacio --------------------------------------------------------------

casos_limpio %>% 
  mutate(epiweek=epiweek(date_of_onset)) %>% 
  ggplot(aes(x = lon,y = lat,colour=date_of_onset)) +
  geom_point() +
  scale_color_gradient(low = "red",high = "yellow",trans = "date") +
  theme_bw() +
  facet_wrap(~epiweek)

# table -------------------------------------------------------------------

casos_limpio %>% 
  mutate(hospital=fct_infreq(hospital),
         outcome=fct_infreq(outcome)) %>% 
  select(outcome:hospital) %>% 
  compareGroups(~.,data = .) %>% 
  createTable()



library(tidyverse)

#Data found [here](https://datadryad.org/stash/dataset/doi:10.5061/dryad.rj849k6)

 rest_data <- read.csv("~/Downloads/latdata.csv")


 head(rest_data)

 # How many papers?
 rest_data %>% select(Citation) %>% distinct()

 #life forms
 rest_data %>% select(LifeFormBroad) %>% distinct()

 #Trophic levels
 rest_data %>% select(TrophLevel) %>% distinct()

 rest_data %>% select( LifeFormBroad, TrophLevel) %>% distinct()


 rest_data %>% select(RV) %>% distinct()

 rest_data %>% select(DisturbCat) %>% distinct()
 rest_data %>% select(DisturbCat, AllActivities, ActiveRestor, Control) %>% distinct()

 rest_data %>% select(HabitatCat) %>% distinct()
 rest_data %>% select(HabitatCat, DisturbCat) %>% distinct()



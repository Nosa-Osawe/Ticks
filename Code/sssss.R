require(tidyverse)

ticks_S <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\Tick_June_july.csv")
view(ticks_S)
attach(ticks_S)

glimpse(ticks_S)
summary(ticks_S)

unique(Collector)
unique(Predilection)

head(ticks_S, 3)

ticks_S %>%
  select(-Collector,-Cattle) %>% 
  head()
  
ticks_S %>% 
  select(-Collector,-Predilection) %>%
  filter(Predilection=="Head") %>% 
  group_by(Cattle) %>% 
  summarise(Total_ticks = sum(across(where(is.numeric))),
            across(where(is.numeric), sum)
            ) %>% 
  as.data.frame() %>% 
  mutate(A.Variegatum_b = if_else(A.Variegatum>0,1,0)) %>% 
  mutate(H.impressum_b = if_else(H.impressum>0,1,0)) %>% 
  mutate(B.geigyi_b = if_else(B.geigyi>0,1,0)) %>%
  mutate(R.sangiuneus_b = if_else(R.sangiuneus>0,1,0))%>% 
  select(-c(3:6)) %>% 
  as.data.frame() %>%
  group_by(Cattle) %>% 
  summarise(Total_ticks = sum(Total_ticks),
            species_richness = sum(across(2:5))) %>% 
  as.data.frame()
 


length(unique(ticks_S$Cattle))  # number of unique cattle



  



require(tidyverse)
d_ticks <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\Tick_June_july.csv")
view(d_ticks)

d_pred <- d_ticks %>% 
  select(-Cattle,-Collector) %>% 
  group_by(Predilection) %>% 
  summarise(total= sum(across(where(is.numeric))),
            across(where(is.numeric), sum))%>% 
  as.data.frame()
d_pred


d_ticks %>% select(-Cattle,-Collector) %>% 
  summarise(total= sum(across(where(is.numeric))), across(where(is.numeric), sum))
            
            
d_cattle <-d_ticks %>% 
  select(-Collector, - Predilection) %>%
  group_by(Cattle) %>% 
  summarise(total = sum(across(where(is.numeric))),
            across(where(is.numeric), sum)) %>% 
  mutate(cattle_infected_ = if_else(total > 0, 1, 0)) %>% 
  mutate(A.Variegatum_infected = if_else(A.Variegatum> 0,1,0)) %>% 
  mutate(H.impressum_infected = if_else(H.impressum > 0,1,0)) %>% 
  mutate(B.geigyi_infected= if_else(B.geigyi> 0,1,0)) %>% 
  mutate(R.sangiuneus_infected = if_else(R.sangiuneus > 0,1,0)) %>% 
  select(-c(3:6)) %>% 
  as.data.frame()
d_cattle

# Belly
  d_ticks %>% 
  select(-Collector, ) %>%
  group_by(Cattle,Predilection) %>% 
  summarise(total = sum(across(where(is.numeric))),
            across(where(is.numeric), sum)) %>% 
  mutate(cattle_infected_ = if_else(total > 0, 1, 0)) %>% 
  mutate(A.Variegatum_infected = if_else(A.Variegatum> 0,1,0)) %>% 
  mutate(H.impressum_infected = if_else(H.impressum > 0,1,0)) %>% 
  mutate(B.geigyi_infected= if_else(B.geigyi> 0,1,0)) %>% 
  mutate(R.sangiuneus_infected = if_else(R.sangiuneus > 0,1,0)) %>% 
  select(-c(3:6)) %>% 
  as.data.frame() %>% 
  filter(Predilection== "Belly") %>% 
  select(-cattle_infected_) %>% 
  group_by(Cattle) %>% 
  summarise(total_infected = sum(across(where(is.numeric)))) %>% 
  as.data.frame() %>% 
  mutate(total_infected = as.factor(total_infected)) %>% 
  select(-Cattle) %>% 
  group_by(total_infected) %>% 
  summarise(count = n()) %>% 
  as.data.frame()

  # Head
  d_ticks %>% 
    select(-Collector, ) %>%
    group_by(Cattle,Predilection) %>% 
    summarise(total = sum(across(where(is.numeric))),
              across(where(is.numeric), sum)) %>% 
    mutate(cattle_infected_ = if_else(total > 0, 1, 0)) %>% 
    mutate(A.Variegatum_infected = if_else(A.Variegatum> 0,1,0)) %>% 
    mutate(H.impressum_infected = if_else(H.impressum > 0,1,0)) %>% 
    mutate(B.geigyi_infected= if_else(B.geigyi> 0,1,0)) %>% 
    mutate(R.sangiuneus_infected = if_else(R.sangiuneus > 0,1,0)) %>% 
    select(-c(3:6)) %>% 
    as.data.frame() %>% 
    filter(Predilection== "Head") %>% 
    select(-cattle_infected_) %>% 
    group_by(Cattle) %>% 
    summarise(total_infected = sum(across(where(is.numeric)))) %>% 
    as.data.frame() %>% 
    mutate(total_infected = as.factor(total_infected)) %>% 
    select(-Cattle) %>% 
    group_by(total_infected) %>% 
    summarise(count = n()) %>% 
    as.data.frame()

  # Neck
  d_ticks %>% 
    select(-Collector, ) %>%
    group_by(Cattle,Predilection) %>% 
    summarise(total = sum(across(where(is.numeric))),
              across(where(is.numeric), sum)) %>% 
    mutate(cattle_infected_ = if_else(total > 0, 1, 0)) %>% 
    mutate(A.Variegatum_infected = if_else(A.Variegatum> 0,1,0)) %>% 
    mutate(H.impressum_infected = if_else(H.impressum > 0,1,0)) %>% 
    mutate(B.geigyi_infected= if_else(B.geigyi> 0,1,0)) %>% 
    mutate(R.sangiuneus_infected = if_else(R.sangiuneus > 0,1,0)) %>% 
    select(-c(3:6)) %>% 
    as.data.frame() %>% 
    filter(Predilection== "Neck") %>% 
    select(-cattle_infected_) %>% 
    group_by(Cattle) %>% 
    summarise(total_infected = sum(across(where(is.numeric)))) %>% 
    as.data.frame() %>% 
    mutate(total_infected = as.factor(total_infected)) %>% 
    select(-Cattle) %>% 
    group_by(total_infected) %>% 
    summarise(count = n()) %>% 
    as.data.frame()
  

  # Shoulder
  d_ticks %>% 
    select(-Collector, ) %>%
    group_by(Cattle,Predilection) %>% 
    summarise(total = sum(across(where(is.numeric))),
              across(where(is.numeric), sum)) %>% 
    mutate(cattle_infected_ = if_else(total > 0, 1, 0)) %>% 
    mutate(A.Variegatum_infected = if_else(A.Variegatum> 0,1,0)) %>% 
    mutate(H.impressum_infected = if_else(H.impressum > 0,1,0)) %>% 
    mutate(B.geigyi_infected= if_else(B.geigyi> 0,1,0)) %>% 
    mutate(R.sangiuneus_infected = if_else(R.sangiuneus > 0,1,0)) %>% 
    select(-c(3:6)) %>% 
    as.data.frame() %>% 
    filter(Predilection== "Shoulder") %>% 
    select(-cattle_infected_) %>% 
    group_by(Cattle) %>% 
    summarise(total_infected = sum(across(where(is.numeric)))) %>% 
    as.data.frame() %>% 
    mutate(total_infected = as.factor(total_infected)) %>% 
    select(-Cattle) %>% 
    group_by(total_infected) %>% 
    summarise(count = n()) %>% 
    as.data.frame()
 

# Leg
d_ticks %>% 
  select(-Collector, ) %>%
  group_by(Cattle,Predilection) %>% 
  summarise(total = sum(across(where(is.numeric))),
            across(where(is.numeric), sum)) %>% 
  mutate(cattle_infected_ = if_else(total > 0, 1, 0)) %>% 
  mutate(A.Variegatum_infected = if_else(A.Variegatum> 0,1,0)) %>% 
  mutate(H.impressum_infected = if_else(H.impressum > 0,1,0)) %>% 
  mutate(B.geigyi_infected= if_else(B.geigyi> 0,1,0)) %>% 
  mutate(R.sangiuneus_infected = if_else(R.sangiuneus > 0,1,0)) %>% 
  select(-c(3:6)) %>% 
  as.data.frame() %>% 
  filter(Predilection== "Leg") %>% 
  select(-cattle_infected_) %>% 
  group_by(Cattle) %>% 
  summarise(total_infected = sum(across(where(is.numeric)))) %>% 
  as.data.frame() %>% 
  mutate(total_infected = as.factor(total_infected)) %>% 
  select(-Cattle) %>% 
  group_by(total_infected) %>% 
  summarise(count = n()) %>% 
  as.data.frame()
 

# Tail
d_ticks %>% 
  select(-Collector, ) %>%
  group_by(Cattle,Predilection) %>% 
  summarise(total = sum(across(where(is.numeric))),
            across(where(is.numeric), sum)) %>% 
  mutate(cattle_infected_ = if_else(total > 0, 1, 0)) %>% 
  mutate(A.Variegatum_infected = if_else(A.Variegatum> 0,1,0)) %>% 
  mutate(H.impressum_infected = if_else(H.impressum > 0,1,0)) %>% 
  mutate(B.geigyi_infected= if_else(B.geigyi> 0,1,0)) %>% 
  mutate(R.sangiuneus_infected = if_else(R.sangiuneus > 0,1,0)) %>% 
  select(-c(3:6)) %>% 
  as.data.frame() %>% 
  filter(Predilection== "Tail") %>% 
  select(-cattle_infected_) %>% 
  group_by(Cattle) %>% 
  summarise(total_infected = sum(across(where(is.numeric)))) %>% 
  as.data.frame() %>% 
  mutate(total_infected = as.factor(total_infected)) %>% 
  select(-Cattle) %>% 
  group_by(total_infected) %>% 
  summarise(count = n()) %>% 
  as.data.frame()

colnames(d_cattle)

#________________________________________________________________________________

no_of_tick_sp <- d_cattle %>% 
  select(-total,-cattle_infected_) %>% 
  group_by(Cattle) %>% 
  summarise(total_infected = sum(across(where(is.numeric)))) %>% 
  as.data.frame() %>% 
  mutate(total_infected = as.factor(total_infected)) %>% 
  select(-Cattle) %>% 
  group_by(total_infected) %>% 
  summarise(count = n()) %>% 
  as.data.frame()


av<- sum(d_cattle$A.Variegatum_infected)
bg<- sum(d_cattle$B.geigyi_infected)
hi<-sum(d_cattle$H.impressum_infected)
rs<- sum(d_cattle$R.sangiuneus_infected)

(av_prevalence <-av/30)
(bg_prevalence<- bg/30)
(hi_prevalence <- hi/30)   
(rs_prevalence <- rs/30) 

# ----------------------------------------------------------------------------------












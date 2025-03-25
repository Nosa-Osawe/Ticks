require(tidyverse)
require(readxl)
library(emmeans)
library(performance)
library(MASS)

lice <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\Lice.xlsx",
                   sheet = "workingsheet")
view(lice)

lice %>% 
dplyr::select(-Data_collector) %>% 
  group_by(Location) %>% 
  summarise(total= sum(across(where(is.numeric))),
            mean_Menacanthis_straminus = mean(Menacanthis_straminus),
            mean_Menopon_galinae= mean(Menopon_galinae),
across(where(is.numeric), sum))%>% 
  as.data.frame()

      #       ----------- Menacanthis_straminus-------------------
M.straminus.model <- glm(Menacanthis_straminus ~ Location, data = lice, 
                         family =  quasipoisson(link = "log"))
summary(M.straminus.model)
check_overdispersion(M.straminus.model)
emmeans(M.straminus.model, pairwise ~ Location,
                           adjust = "Tukey")

check_model(M.straminus.model)
check_homogeneity(M.straminus.model)
model_performance(M.straminus.model)


#       ----------- Menopon_galinae-------------------

M.galinae.quasi <- glm(Menopon_galinae ~ Location, data = lice, 
                       family = quasipoisson(link = "log")
                         )
summary(M.galinae.quasi)
check_overdispersion(M.galinae.quasi)
emmeans(M.galinae.quasi, pairwise ~ Location,
        adjust = "Tukey")
check_model(M.galinae.quasi)
check_homogeneity(M.galinae.quasi)    # Heteroskedastic mfk!
check_zeroinflation(M.galinae.quasi)  # model OK
model_performance(M.galinae.quasi)



lm1 <- lm (Menopon_galinae ~ Location, data = lice)
check_model(lm1)
model_performance(lm1)
check_homogeneity(lm1)
check_normality(lm1)



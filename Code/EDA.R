library(tidyverse)

Tick <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\Organized_Tick_sheet.csv", 
                 stringsAsFactors = TRUE) %>% # We made some taxonomy corrections after review
  rename("A. variegatum" = "A..variegatum",
         "A. cohaerens" = "A..coharenses",
         "R. annulatus"= "B..annulatus",
         "R. decoloratus" = "B..decoloratus",
         "R. geigyi" = "B..geigyi",
         "H. leachi" =  "H..laechi",
         "R. lunulatus" = "R..lunulatus",
         "R. muhsamae" = "R..muhsame",
  
         "R. senegalensis" = "R..senegalensis",
         "Rhipicephalus sp." = "Boophilus.sp.",
      
         ) %>%  
  mutate("R. sanguineus" = rowSums(across(c('R..sanguineus', 'R.fanguineus'))),
         "R. guilhoni" = rowSums(across(c('R..quilhoni','R..gulhoni'))),
         "Amblyomma sp." = rowSums(across(c("A.gemma", "R..gemma"))),
         "A. variegatum" = rowSums(across(c("A. variegatum",  "A. cohaerens")))
         ) %>% 
  select(-c("R..quilhoni","R.fanguineus",
            "R..gemma", "R..gulhoni", "R..sanguineus", "A.gemma", "A. cohaerens")) %>% 
  select(-X) # The useless (serial number) column

colnames(Tick)

# -----------------------
 

                                                                                                            
life_stage_summary <- Tick %>%
  select(-Cattle_ID) %>% 
  group_by(
    Life_stage, 
    Predeliction) %>%
  summarise(total= sum(across(where(is.numeric))))
view(life_stage_summary)


total_sum <- sum(Tick %>% 
                   select(-Cattle_ID) %>% 
                   select(where(is.numeric)) %>%
                   unlist())
 

sum_life_stage <- Tick %>%
  dplyr::select(-Cattle_ID) %>% 
  group_by(Life_stage) %>%
  summarise(total= sum(across(where(is.numeric))))

# sex of adults ticks

Tick %>%
  filter(Life_stage == "Adult") %>% 
  select(-Cattle_ID) %>% 
  group_by(Sex) %>%
  summarise(total= sum(across(where(is.numeric))))

Tick %>%
  filter(Sample == "S1", Predeliction == "Neck") %>% 
  dplyr::group_by(Cattle_ID) %>%
  summarise(total= sum(across(where(is.numeric))))

Tick %>% 
  select(-Cattle_ID) %>% 
  group_by(Sex) %>%
  summarise(total= sum(across(where(is.numeric))))


Tick %>% 
  select(-Cattle_ID) %>% 
  group_by(Sex) %>%
  summarise(total= sum(across(where(is.numeric))))

Tick %>%
 dplyr::select(-Cattle_ID, - Sex) %>% 
  group_by(Predeliction) %>%
  summarise(across(where(is.numeric), sum)) %>% 
  as.data.frame() %>% 
  write.csv(file = "C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\abundance.csv")

write.csv(Tick %>% 
            select(-Cattle_ID, -Sex,-Sample,-Life_stage) %>% 
            group_by(Predeliction) %>% 
            summarise(across(where(is.numeric), sum)), 
          file = "C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\Tick_Abundance.csv")

Tick %>% 
  select(-Cattle_ID) %>%
  filter(Life_stage == "Nymph") %>% 
  group_by(Predeliction) %>% 
  summarise(total= sum(across(where(is.numeric))),
            across(where(is.numeric), sum))

Predeliction_CA <-Tick %>% 
  select(-Cattle_ID, -Sex,-Sample,-Life_stage) %>% 
  group_by(Predeliction) %>% 
  summarise(across(where(is.numeric), sum))
# view(Predeliction_CA)


nmds_pred<- Tick %>% 
  filter(Life_stage == "Adult") %>% # Only adults
  select(-Sex,-Sample,-Life_stage) %>% 
  group_by(Cattle_ID, Predeliction) %>% 
  summarise(total= sum(across(where(is.numeric))),
            across(where(is.numeric), sum)) %>% 
  filter(total >0) %>% 
  select(-total)
# view(nmds_pred)

# view(nmds_pred)
nmds_pred <- as.data.frame(nmds_pred)

sum(is.na(nmds_pred))



nmds_sex<- Tick %>% 
  filter(Life_stage == "Adult") %>% # Only adults
  select(-Predeliction,-Sample,-Life_stage) %>% 
  group_by(Cattle_ID,Sex ) %>% 
  summarise(total= sum(across(where(is.numeric))),
            across(where(is.numeric), sum)) %>% 
  filter(total >0) %>% 
  select(-total)
# view(nmds_sex)

nmds_stage<- Tick %>% 
  select(-Predeliction,-Sample,-Sex) %>% 
  group_by(Cattle_ID,Life_stage ) %>% 
  summarise(total= sum(across(where(is.numeric))),
            across(where(is.numeric), sum)) %>% 
  filter(total >0) %>% 
  select(-total)
# view(nmds_stage)

#-----------------------------------------------------------------------------------------
library(vegan)
library(pairwiseAdonis)  


pred1 <- nmds_pred %>% 
  select(-c("Cattle_ID", "Predeliction"))
pred2 <- nmds_pred[,1:2]

pred <- metaMDS(pred1, distance = "bray", k=2, na.rm = TRUE)
pred$stress
stressplot(pred)

scores(pred) #### very important 

pred_distance<- vegdist(pred1, method = "bray")
anova(betadisper(pred_distance, pred2$Predeliction))

adonis2 (pred1~Predeliction, 
         data = pred2, permutations = 9999, 
         method = "bray")

pairwise <- pairwise.adonis(pred_distance,pred2$Predeliction)
pairwise

write.csv(pairwise, 
          "C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\nmds1_permanova.csv")

pred_nmds <- as.data.frame(pred$points)
pred_nmds_sc <- as.data.frame(scores(pred)$species)  
combined_pred <- as.data.frame(cbind(nmds_pred, pred_nmds))
view(combined_pred)



predilection_colors <- c(
  "Belly" = "#4daf4a",    # Green
  "Head" = "#377eb8",     # Blue
  "Leg" = "#ff7f00",      # Orange
  "Neck" = "purple",     # Purple
  "Shoulder" = "#e31a1c", # Red
  "Tail" = "yellow"      # Yellow
)

pred_plot  <- ggplot() +
  geom_point(data = combined_pred, aes(x = MDS1, y = MDS2, 
                                       color = Predeliction, 
                                       fill = Predeliction
                                       ), 
             size = 3) + 
  scale_colour_manual(values = predilection_colors)+
  scale_fill_manual(values = predilection_colors)+
  theme(
    text = element_text(family = "Times New Roman", size = 20)
  ) + labs(x = "NMDS1", y = "NMDS2")+
  xlim(-4, NA) +  # Set x-axis to start at -4, leave upper limit automatic
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  theme_minimal()+
  
  guides(
    color = guide_legend(title = "Predeliction"),   
    shape = "none", 
    fill = "none"  # To hide fill legend
  )
pred_plot

ggsave(plot= pred_plot, 
       file = "C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Notes & Figures\\Pred_nmds.jpg",
       height = 6, width= 8)
# -----------------------------------------------------------------------------------

sex1 <- nmds_sex[,-c(1,2)]
sex2 <- nmds_sex[,1:2]

sex <- metaMDS(sex1, distance = "bray", k=2, na.rm = TRUE)
sex$stress
stressplot(sex)

scores(sex) 

sex_distance<- vegdist(sex1, method = "bray")
anova(betadisper(sex_distance, sex2$Sex))

adonis2 (sex1~Sex, 
         data = sex2, permutations = 9999, 
         method = "bray")

pairwise_sex <- pairwise.adonis(sex_distance,sex2$Sex)
pairwise_sex

sex_nmds <- as.data.frame(sex$points)
sex_nmds_sc <- as.data.frame(scores(sex)$species)  
combined_sex <- as.data.frame(cbind(nmds_sex, sex_nmds))
# view(combined_sex)


sex_plot  <- ggplot() +
  stat_ellipse(data = combined_sex,  geom = "polygon", 
               aes(x = MDS1, y = MDS2, 
                   group = Sex, fill = Sex),
               level = 0.95, 
               linewidth = 0.1,
               alpha = 0.1,
               show.legend = NA)+ 
  geom_point(data = combined_sex, aes(x = MDS1, y = MDS2, 
                                      color = Sex, 
                                      fill = Sex, 
                                      shape = Sex), 
             size = 3) + 
  stat_ellipse(data = combined_sex, 
               aes(x = MDS1, y = MDS2, 
                   group = Sex, 
                   color = Sex), 
               geom = "path", 
               level = 0.95, 
               linewidth = 0.5,  # Adjust this value to make the line bold
               show.legend = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  theme(
    text = element_text(family = "Times New Roman", size = 20)
  ) + labs(x = "NMDS1", y = "NMDS2")+
  xlim(-4, NA) +  # Set x-axis to start at -4, leave upper limit automatic
  theme_minimal()+
  guides(
    color = guide_legend(title = "Sex"),   
    shape = "none", 
    fill = "none"  # To hide fill legend
  )
sex_plot

ggsave(plot= sex_plot, 
       file = "C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Notes & Figures\\sex_nmds.jpg",
       height = 6, width= 8)
#-------------------------------------------------------------------------------------


stage1 <- nmds_stage[,-c(1,2)]
stage2 <- nmds_stage[,1:2]

stage <- metaMDS(stage1, distance = "bray", k=2, na.rm = TRUE)
stage$stress
stressplot(stage)

scores(stage) #### very important 

stage_distance<- vegdist(stage1, method = "bray")
anova(betadisper(stage_distance, stage2$Life_stage))

adonis2 (stage1~Life_stage, 
         data = stage2, permutations = 9999, 
         method = "bray")

pairwise_stage <- pairwise.adonis(stage_distance, stage2$Life_stage)
pairwise_stage

stage_nmds <- as.data.frame(stage$points)
stage_nmds_sc <- as.data.frame(scores(stage)$species)  
combined_stage <- as.data.frame(cbind(stage_nmds, nmds_stage))
view(combined_stage)


stage_plot  <- ggplot() +
  geom_point(data = combined_stage, aes(x = MDS1, y = MDS2, 
                                      color = Life_stage, 
                                      fill = Life_stage,
                                      shape = Life_stage), 
             size = 3) + 
  stat_ellipse(data = combined_stage,  geom = "polygon", 
               aes(x = MDS1, y = MDS2, 
                   group = Life_stage, fill = Life_stage),
               level = 0.95, 
               linewidth = 0.1,
               alpha = 0.1,
               show.legend = NA)+ 
  stat_ellipse(data = combined_stage, 
               aes(x = MDS1, y = MDS2, 
                   group = Life_stage, 
                   color = Life_stage), 
               geom = "path", 
               level = 0.95, 
               linewidth = 0.5,  # Adjust this value to make the line bold
               show.legend = NA) +
  scale_colour_manual(values = c('red',"blue"))+
  scale_fill_manual(values = c("red","blue"))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")+
  theme(
    text = element_text(family = "Times New Roman", size = 20)
  ) + labs(x = "NMDS1", y = "NMDS2")+
  xlim(-3, NA) +  # Set x-axis to start at -4, leave upper limit automatic
  theme_minimal()+
  guides(
    color = guide_legend(title = "Life stage"),   
    shape = "none", 
    fill = "none"  # To hide fill legend
  )
stage_plot

ggsave(plot= stage_plot, 
       file = "C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Notes & Figures\\stage_nmds.jpg",
       height = 6, width= 8)


predilection <- Tick %>% 
  select(-Sex,-Sample,-Life_stage,-Cattle_ID) %>% 
  group_by(Predeliction) %>% 
  summarise(total= sum(across(where(is.numeric))),
            across(where(is.numeric), sum)) %>% 
  filter(total >0) %>% 
  select(-total)%>% 
  t()  # Transpose 

colnames(predilection) <- predilection[1, ]
predilection <- predilection[-1, ]

predilection <- as.data.frame(predilection)
predilection <- predilection %>%
  tibble::rownames_to_column(var = "Species")
rownames(predilection) <- NULL


predilection %>% 
  mutate(across(2:7, as.numeric)) %>%
  group_by(Species) %>% 
  summarise(across(where(is.numeric), sum), 
            total= sum(across(where(is.numeric)))) 


write.csv(predilection,"C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\Predilection.csv")





Cattle<- Tick %>%
  group_by(Cattle_ID) %>%
  summarise(across(where(is.numeric), sum)) 
  

# view(Cattle)

write.csv(Cattle,
          "C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\cattle.csv")




unique(Tick$Predeliction)

Leg <- Tick %>% 
  select(-Sex,-Sample,-Life_stage) %>% 
  filter(Predeliction == "Leg") %>% 
  group_by(Cattle_ID) %>% 
  summarise(across(where(is.numeric), sum))
# view(Leg)

Belly <- Tick %>% 
  select(-Sex,-Sample,-Life_stage) %>% 
  filter(Predeliction == "Belly") %>% 
  group_by(Cattle_ID) %>% 
  summarise(across(where(is.numeric), sum))
# view(Belly)

Head <- Tick %>% 
  select(-Sex,-Sample,-Life_stage) %>% 
  filter(Predeliction == "Head") %>% 
  group_by(Cattle_ID) %>% 
  summarise(across(where(is.numeric), sum))
# view(Head)


Tail <- Tick %>% 
  select(-Sex,-Sample,-Life_stage) %>% 
  filter(Predeliction == "Tail") %>% 
  group_by(Cattle_ID) %>% 
  summarise(across(where(is.numeric), sum))
# view(Tail)

Neck <- Tick %>% 
  select(-Sex,-Sample,-Life_stage) %>% 
  filter(Predeliction == "Neck") %>% 
  group_by(Cattle_ID) %>% 
  summarise(across(where(is.numeric), sum))
# view(Neck)

Shoulder <- Tick %>% 
  select(-Sex,-Sample,-Life_stage) %>% 
  filter(Predeliction == "Shoulder") %>% 
  group_by(Cattle_ID) %>% 
  summarise(across(where(is.numeric), sum))
# view(Shoulder)

overall_pred <- as.data.frame(rbind(Leg, Belly, Head, Tail, Neck, Shoulder))

write.csv(overall_pred,
          "C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\overall_pred.csv")


diversity <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\PAST diversity.csv")





abundance <- diversity %>%
  select(Predilection, Individuals) %>%
  mutate(Predilection = as.factor(Predilection)) %>%  # Convert Site to a factor
  ggplot() +
  geom_violin(aes(x = Predilection, y = Individuals, fill = Predilection), color = NA, alpha = 0.3)+
  geom_boxplot(aes(x = Predilection, y = Individuals, fill = Predilection),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,
               width= 0.2) +
  scale_fill_manual(values = predilection_colors) +  # Set fill colors
  scale_color_manual(values = predilection_colors) + 
  labs(x= "Predilection",
       y= "Abundance", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_classic()
print(abundance)

ggsave(plot = abundance, 
       file ="C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Notes & Figures\\abundance.jpg",
       height = 4, width = 8)



Taxa_S <- diversity %>%
  select(Predilection, Taxa_S) %>%
  mutate(Predilection = as.factor(Predilection)) %>%  # Convert Site to a factor
  ggplot() +
  geom_violin(aes(x = Predilection, y = Taxa_S, fill = Predilection), color = NA, alpha = 0.3)+
  geom_boxplot(aes(x = Predilection, y = Taxa_S, fill = Predilection),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,
               width= 0.2) +
  scale_fill_manual(values = predilection_colors) +  # Set fill colors
  scale_color_manual(values = predilection_colors) + 
  labs(x= "Predilection",
       y= "Taxa richness", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_classic()
print(Taxa_S)

ggsave(plot = Taxa_S, 
       file ="C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Notes & Figures\\Taxa_S.jpg",
       height = 4, width = 8)

Shannon <- diversity %>%
  select(Predilection, Shannon_H) %>%
  mutate(Predilection = as.factor(Predilection)) %>%  # Convert Site to a factor
  ggplot() +
  geom_violin(aes(x = Predilection, y = Shannon_H, fill = Predilection), color = NA, alpha=0.3)+
  geom_boxplot(aes(x = Predilection, y = Shannon_H, fill = Predilection),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,
               width= 0.2) +
  scale_fill_manual(values = predilection_colors) +  # Set fill colors
  scale_color_manual(values = predilection_colors) + 
  labs(x= "Predilection",
       y= "Shannon index", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_classic()
print(Shannon)


ggsave(plot = Shannon, 
       file ="C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Notes & Figures\\shannon.jpg",
       height = 4, width = 8)

Margalef <- diversity %>%
  dplyr::select(Predilection, Margalef) %>%
  mutate(Predilection = as.factor(Predilection)) %>%  # Convert Site to a factor
  ggplot() +
  geom_violin(aes(x = Predilection, y = Margalef, fill = Predilection), color = NA, alpha=0.3)+
  geom_boxplot(aes(x = Predilection, y = Margalef, fill = Predilection),
               notch = FALSE,
               outlier.colour = NULL,
               outlier.color = "transparent",
               outlier.fill = NULL,
               width= 0.2) +
  scale_fill_manual(values = predilection_colors) +  # Set fill colors
  scale_color_manual(values = predilection_colors) + 
  labs(x= "Predilection",
       y= "Margalef index", legend = FALSE)+
  theme(
    text = element_text(family = "Times New Roman", size = 24))+# Color of the mean points
  theme_classic()
print(Margalef)


ggsave(plot = Margalef, 
       file ="C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Notes & Figures\\Margalef.jpg",
       height = 4, width = 8)

###############################################################################################

# --        Negative binomial regression testing

library(lme4)
library(lmerTest)
library(MASS)
library(ggfortify)
library(multcomp)
library(emmeans)
library(car)


diversity$Predilection <- factor(diversity$Predilection)

Individuals_compare <- glm.nb(Individuals ~ Predilection,
                            data = diversity,
                            link = log)
summary(Individuals_compare)        ### No sig. difference
multi_comp_individuals <- glht(Individuals_compare,
                             linfct = mcp(Predilection = "Tukey"))
summary(multi_comp_individuals)

 
#-----------------------------------------------------------------------------------------

Taxa_S_compare <- glm.nb(Taxa_S ~ Predilection,
                              data = diversity,
                              link = log)
summary(Taxa_S_compare)        ### No sig. difference
multi_comp_Taxa_S <- glht(Taxa_S_compare,
                               linfct = mcp(Predilection = "Tukey"))
summary(multi_comp_Taxa_S)

#--------------------------------------------------------------------------------------
library(dunn.test)
Margalef_compare <- lm(Margalef ~ Predilection,
                         data = diversity)
summary(Margalef_compare)        ### No sig. difference
multi_comp_Margalef <- glht(Margalef_compare,
                          linfct = mcp(Predilection = "Tukey"))
summary(multi_comp_Margalef)

attach(diversity)

kruskal.test(Margalef ~ Predilection, data = diversity)  


###########################################################################################



tick_famd_data<-Tick %>% # trying for sums
  select(-Cattle_ID) %>% 
  group_by(Predeliction,Sex,Life_stage) %>% 
  summarise(
    across(where(is.numeric), sum)
  ) %>% 
  rename("A.va" =4,
         "R.an"= 5,
         "R.de" =6,
         "R.ge" = 7,
         "H.le" = 8,
         "R.lu"= 9,
         "R.mu" = 10,
         "R.se" = 11,
         "R.sp"= 12,
         "R.sa" =13,
         "R.gu"=14,
         "A.sp"= 15) %>% 
  as.data.frame()

colnames(Tick)


tick_famd_data_mean<-Tick %>%  # trying for mean
  select(-Cattle_ID) %>% 
  group_by(Predeliction,Sex,Life_stage) %>% 
  summarise(
    across(where(is.numeric), mean)
  ) %>% 
  rename("A.va" =4,
         "R.an"= 5,
         "R.de" =6,
         "R.ge" = 7,
         "H.le" = 8,
         "R.lu"= 9,
         "R.mu" = 10,
         "R.se" = 11,
         "R.sp"= 12,
         "R.sa" =13,
         "R.gu"=14,
         "A.sp"= 15) %>% 
  as.data.frame()
#view(tick_famd_data_mean)

Tick %>% 
  select(-Cattle_ID) %>% 
  group_by(Predeliction, Sex, Life_stage) %>% 
  summarise(
    across(where(is.numeric), ~ sum(. > 0, na.rm = TRUE))
  ) %>% 
  rename("A.va" =4,
         "R.an"= 5,
         "R.de" =6,
         "R.ge" = 7,
         "H.le" = 8,
         "R.lu"= 9,
         "R.mu" = 10,
         "R.se" = 11,
         "R.sp"= 12,
         "R.sa" =13,
         "R.gu"=14,
         "A.sp"= 15) %>% 
  as.data.frame()

library("FactoMineR")
library("factoextra")
library(tidyverse)
library("corrplot")
 

Tick.famd <- FAMD(tick_famd_data, graph = FALSE)
eig. <- get_eigenvalue(Tick.famd)
head(eig.)
 

famd_quant <- as.data.frame(Tick.famd$quali.var$coord[,1:2])
famd_quali <- as.data.frame(Tick.famd$quanti.var$coord[,1:2])

library(ggrepel)

famd_plot <- ggplot() +
  geom_text_repel(data = famd_quant, 
                  aes(x = Dim.1, y = Dim.2, label = rownames(famd_quant)
                  ), color = "darkblue")+ 
  geom_text_repel(data = famd_quali, 
                  aes(x = Dim.1, y = Dim.2, label = rownames(famd_quali)
                  ), color = "red")+
  labs(x = "Dim. 1 (25.18%)",
       y = "Dim. 2 (20.60%)")+
  theme_bw()

print(famd_plot)

ggsave(plot = famd_plot, 
       file="C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Notes & Figures\\famd_plot.jpg",
       height = 6, width = 8)
#   -------------------------------------------------------------------------------------

# just checking the head and shoulder collections
Tick %>% 
  select(-Cattle_ID, -Sample,-Sex) %>% 
  filter(Predeliction== "Head"| Predeliction == "Shoulder") %>% 
  group_by(Life_stage,Predeliction) %>% 
  summarise(
    across(where(is.numeric), sum)
  )



diversity %>% 
 dplyr:: select(Predilection, Margalef, Individuals, Taxa_S) %>% 
  group_by(Predilection) %>%
  summarise(abundance = mean(Individuals),
            sd = sd(Individuals),
            taxaS = mean(Taxa_S),
            sdtax = sd(Taxa_S),
            Margalef = mean(Margalef),
            sdMargalef = sd(Margalef, na.rm = TRUE))

colnames(diversity)
  

diversity %>% 
 dplyr:: select(Predilection, Margalef) %>% 
  group_by(Predilection) %>%
  summarise(sd = sd(Margalef),
            se = sd/sqrt(length(Margalef)))

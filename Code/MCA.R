library("FactoMineR")
library("factoextra")
library(tidyverse)
library("corrplot")
library(gplots)

data(poison)
head(poison[, 1:7], 3)

view(poison)
#Subset only active individuals and variables for multiple correspondence analysis:
poison.active <- poison[1:55, 5:15] # active rows and active columns.
# The rest would be predicted 

# Summary of the 4 first variables
summary(poison.active)[, 1:4]
# viz it
for (i in 1:4) {
  plot(poison.active[,i], main=colnames(poison.active)[i],
       ylab = "Count", col="steelblue", las = 2)
}

res.mca <- MCA(poison.active, graph = FALSE)
print(res.mca)


eig.val <- get_eigenvalue(res.mca)
head(eig.val)
#To visualize the percentages of inertia explained by each MCA dimensions 
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

# viz
fviz_mca_biplot(res.mca,
                repel = TRUE, 
                ggtheme = theme_minimal())


var <- get_mca_var(res.mca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factor map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)


#Correlation between variables and principal dimensions
fviz_mca_var(res.mca, choice = "mca.cor",
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
#It can be seen that, the variables Diarrhae, Abdominals and Fever are
# the most correlated with dimension 1. Similarly, the variables
# Courgette and Potato are the most correlated with dimension 2.

head(round(var$coord, 2), 4)

fviz_mca_var(res.mca,
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
# change colour and shape of the variables
fviz_mca_var(res.mca, col.var="black", shape.var = 15,
             repel = TRUE)


#Quality of the representation
head(var$cos2, 4)
# Color by cos2 values: quality on the factor map
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())


# Change the transparency by cos2 values
fviz_mca_var(res.mca, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.mca, choice = "var", axes = 1:2)

# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)



#Cos2 of individuals
fviz_cos2(res.mca, choice = "ind", axes = 1:2, top = 20)
# Contribution of individuals to the dimensions
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)

fviz_mca_ind(res.mca,
             label = "none", # hide individual labels
             habillage = 2, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())
# or 
fviz_mca_ind(res.mca,
             label = "none", # hide individual labels
             habillage = "Vomiting", # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())   # same thing

# habillage = index of the column to be used as grouping variable
fviz_mca_ind(res.mca, habillage = 2, addEllipses = TRUE)
# habillage = external grouping variable
fviz_mca_ind(res.mca, label = "none", habillage = poison$Vomiting, addEllipses =
               TRUE)

fviz_ellipses(res.mca, c("Vomiting", "Fever"),
              geom = "point")

# Alternatively, you can specify categorical variable indices:
fviz_ellipses(res.mca, 1:4, geom = "point")

# dimdesc can be used to identify the most correlated variables with a given dimension:
res.desc <- dimdesc(res.mca, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]

#supplementary  individuals
res.mca <- MCA(poison, ind.sup = 53:55,
               quanti.sup = 1:2, quali.sup = 3:4, graph=FALSE)


# The predicted results for supplementary individuals/variables can be extracted 
# Supplementary qualitative variable categories
res.mca$quali.sup
# Supplementary quantitative variables
res.mca$quanti
# Supplementary individuals
res.mca$ind.sup
# Biplot of individuals and variable categories
fviz_mca_biplot(res.mca, repel = TRUE,
                ggtheme = theme_minimal())
#to highlight the correlation between variables (active & supplementary) and dimensions

fviz_mca_var(res.mca, choice = "mca.cor",
             repel = TRUE)    
fviz_mca_var(res.mca, repel = TRUE,
             ggtheme= theme_minimal())

fviz_mca_var(res.mca, choice = "quanti.sup",
             ggtheme = theme_minimal())

fviz_mca_ind(res.mca,
             label = "ind.sup", #Show the label of ind.sup only
             ggtheme = theme_minimal())

# Visualize variable categories with cos2 >= 0.4
fviz_mca_var(res.mca, select.var = list(cos2 = 0.4))
# Top 10 active variables with the highest cos2
fviz_mca_var(res.mca, select.var= list(cos2 = 10))
# Select by names
name <- list(name = c("Fever_n", "Abdo_y", "Diarrhea_n",
                      "Fever_Y", "Vomit_y", "Vomit_n"))
fviz_mca_var(res.mca, select.var = name)
# top 5 contributing individuals and variable categories
fviz_mca_biplot(res.mca, select.ind = list(contrib = 5),
                select.var = list(contrib = 5),
                ggtheme = theme_minimal(), repel = TRUE)
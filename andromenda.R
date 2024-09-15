#getwd()

setwd("C:/Users/lberg/Desktop/DR_project")

fire.an <- read.csv("Andromeda_d.csv", sep= ",", dec = ".")

head(fire.an)
str(fire.an)

#install.packages("FactoMineR")
library(FactoMineR)

# Filter csv file to only perform PCA for lines 3 -7 
pc <- PCA(fire.an[3:8], graph=FALSE, scale= T)
summary(pc)

#install.packages("factoextra")
#install.packages("ggplot2")
#install.packages("rio")
#install.packages("sp")
library(sp)

library(ggplot2)
library(iNEXT)

library(rio)

#fire_eig_an<- pc$eig
#export(fire_eig_an, "fire_eig_an.csv")

#fire_var_an <- pc$var$coord
#export(fire_var_an, "fire_var_an.csv")

pc$ind

fire.an$PC1 <- pc$ind$coord[,1]
head(fire.an)

str(fire.an)

png(filename= "PCAfireplot_an.png", width= 7, height= 5, units= "in", res= 500)

ggplot(data= fire.an, aes(x= leaf.dry.matter.content, y= PC1, color=sp)) + geom_point() + geom_smooth(method="glm", formula = y ~ x, alpha = 0.2, size = 0.7, aes(color = sp, fill = sp)) + xlab("LDMC") + ylab("Flammability") + theme_classic()
  

dev.off()

shapiro.test(fire.an$PC1)
#test to get the P-value and W-value 

kruskal.test(PC1 ~ sp, data= fire.an)
#statistical test proving that there are differences

dev.off()


png(filename= "LWCA_an.png", width= 7, height= 5, units= "in", res= 500)

ggplot(data= fire.an, aes(x= leaf.water.content.per.area, y= PC1, color=sp)) + geom_point() + geom_smooth(method="glm", formula = y ~ x, alpha = 0.2, size = 0.7, aes(color = sp, fill = sp)) + xlab("LWCA") + ylab("Flammability") + theme_classic()

dev.off()


png(filename= "LWCAvsLdmc_an.png", width= 7, height= 5, units= "in", res= 500)

ggplot(data= fire.an, aes(x= leaf.water.content.per.area, y= leaf.dry.matter.content, color=sp)) + geom_point() + geom_smooth(method="glm", formula = y ~ x, alpha = 0.2, size = 0.7, aes(color = sp, fill = sp)) + xlab("LWCA") + ylab("LDMC") + theme_classic()

dev.off()

m1 <- lm(PC1~leaf.dry.matter.content + sp, data=fire.an)

m2 <- summary(m1)

summ <- m2$coefficients

export(summ, "models.LDMC.csv", row.names= T)

#-----------------------------------------------------------------

m2 <- lm(PC1~leaf.water.content.per.area + sp, data=fire.an)

m3 <- summary(m2)

summ <- m3$coefficients

export(summ, "models.LWCA.csv", row.names= T)

#-----------------------------------------------------------------

m3 <- lm(leaf.dry.matter.content~leaf.water.content.per.area  + sp, data=fire.an)

m4 <- summary(m3)

summ <- m4$coefficients

export(summ, "models.csv", row.names= T)

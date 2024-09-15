setwd("C:/Users/lberg/Desktop/DR research project")
#wd <- getwd()
#setwd(wd)

fire <- read.csv("______", sep= ",", dec = ".")

head(fire)
str(fire)

#install.packages("FactoMineR")
library(FactoMineR)

pc <- PCA(fire[3:5], graph=FALSE, scale= T)
summary(pc)

#install.packages("factoextra")
#install.packages("ggplot2")
#install.packages("rio")

library(ggplot2)
library(iNEXT)

library(rio)

fire_eig<- pc$eig
export(fire_eig, "fire_eig.csv")

fire_var <- pc$var$coord
export(fire_var, "fire_var.csv")

pc$ind

fire$PC1 <- pc$ind$coord[,1]
head(fire)

png(filename= "PCAfireplot.png", width= 7, height= 5, units= "in", res= 500)

ggplot(fire, aes(x=reorder(sp,-PC1, na.rm=T), y=PC1, fill=site)) +
  geom_boxplot(position=position_dodge(1)) + labs(x="Species")

dev.off()

shapiro.test(fire$PC1)
#test to get the P-value and W-value 

kruskal.test(PC1 ~ sp, data= fire)
#statistical test proving that there are differences

png(filename= "PCAfireplot.png", width= 7, height= 5, units= "in", res= 500)

ggplot(fire, aes(x=reorder(sp,-PC1, na.rm=T), y=PC1, fill=site)) +
  geom_boxplot(position=position_dodge(1)) + labs(x="Species")

dev.off()

library(datasets, lib.loc = "C:/Program Files/R/R-4.2.1/library")
data("ToothGrowth")

#PART ONE

TG <- ToothGrowth
par(mfrow = c(1,2))
with (ToothGrowth, interaction.plot (dose, supp, len, legend = T))
with (ToothGrowth, interaction.plot (supp, dose, len, legend = T))
boxplot(len ~ supp*dose, data = TG, col = c("orange","blue"), main = "Box Plot of Tooth Growth for Various supp:dose" )




#PART TWO

hist(TG$len, col = "blue", labels = TRUE, main = "Histogram of Tooth Length")
shapiro.test(TG$len)


TG <- ToothGrowth
TG$dose <- as.factor(TG$dose)
lin1 <- lm(len ~ dose* supp, TG)
plot(lin1)
 
  
  
  
  
#PART THREE
  
  
anova(lin1)
  
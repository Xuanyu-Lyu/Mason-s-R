setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(haven)
library(tidyverse)
library(lmerTest)

###### Multilevel model

MLM <- read_sav("MLM.sav")

MLM <- MLM |> filter(is.na(Person) == FALSE)

MLM$Person <- as.factor(MLM$Person)
#MLM$Ext <- as.factor(MLM$Ext)

# MLM$Hap_c <- MLM$Hap - MLM$meanHap

MLM <- MLM |> group_by(Person) |> mutate(meanNumpeople = mean(NumPeople)) 
MLM <- MLM |> mutate(Ext_c = Ext - 0.5)

MLM$NumPeople_c <- MLM$NumPeople - MLM$meanNumpeople

lmer(Hap ~ NumPeople_c + Ext_c + NumPeople_c*Ext_c + (NumPeople | Person),
     data = MLM) |> summary()

lmer(Hap ~ NumPeople_c + (NumPeople | Person) ,
     data = MLM[which(MLM$Ext==1),]) |> summary()

lmer(Hap ~ NumPeople_c + (NumPeople | Person) ,
     data = MLM[which(MLM$Ext==0),]) |> summary()


lmer(Hap ~ NumPeople_c + Ext + NumPeople_c*Ext + (NumPeople | Person),
     data = MLM)|> summary()
lmer(Hap ~ NumPeople_c + (NumPeople | Person) ,
     data = MLM[which(MLM$Ext==1),]) |> summary()

###### Manova

homeless <- read_sav("Homelessness.sav")

cor.test(homeless$QOL, homeless$GHS)

cor(homeless)
library(psych)
cortest.bartlett(cor(homeless$QOL, homeless$GHS), n = 60)

library(heplots)
homeless$Homeless <- as.factor(homeless$Homeless)
boxM(cbind(QOL,GHS) ~ Homeless, data = homeless)

l1 <- manova(cbind(QOL,GHS) ~ Homeless, data = homeless)
summary(l1,test = c( "Wilks"))

library(heplots)
etasq(l1, test = c( "Wilks"))

# library(effectsize)
# effectsize::eta_squared(l1)


l2 <- aov(cbind(QOL,GHS) ~ Homeless, data = homeless)
summary(l2)
l3 <- aov(GHS ~ Homeless, data = homeless)
etasq(l3)

library(multcomp)

# Tukey HSD test:
post_test <- glht(aov(GHS~Homeless, data = homeless), 
                  linfct = mcp(Homeless = "Tukey")
)

summary(post_test)


library(mvnormalTest)
mardia(homeless[, c("QOL", "GHS")])




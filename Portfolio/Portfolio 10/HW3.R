setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## #1
library(haven)
df1 <- read_sav("df1.sav")

library(psych)
library(GPArotation)


nofactors = fa.parallel(df1, fm="pa", fa="fa")

round1 = fa(df1, nfactors=3, rotate = "promax", fm = "pa")
round1
summary(round1)


## #2
model1 <- lm(q1 ~ q2 + q3  + q4 +q5 + q6 +q7 + q8 +q9, data = df1)
summary(model1)

model2 <- lm(q2 ~ q1 + q3  + q4 +q5 + q6 +q7 + q8 +q9, data = df1)
summary(model2)

round2 = fa(df1, nfactors=2, rotate = "promax", fm = "pa")
round2

round3 = fa(df1, nfactors=2, rotate = "none", fm = "pa")
round3


## #3
df2 <- read_sav("df2.sav")[,-1]


#df2[,c(2,6,8,9,12,18,21,23,24,27,31,34,35,37,41,43)] <- 6 - df2[,c(2,6,8,9,12,18,21,23,24,27,31,34,35,37,41,43)]

nofactors = fa.parallel(df2, fm="pa", fa="fa")

mf1 <- fa(df2, nfactors = 5, rotate ="promax", fm = "pa" )
mf1
loadings(mf1, sort=TRUE)

print(loadings(mf1), digits=2, cutoff=.2, sort=TRUE)

mf2 <- fa(df2, nfactors = 3, rotate ="promax", fm = "pa" )
mf2
loadings(mf2, sort=TRUE)

print(loadings(mf2), digits=2, cutoff=.2, sort=TRUE)

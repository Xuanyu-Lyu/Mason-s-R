Portfolio 2
================
Xuanyu
2/22/2022

### This document is Xuanyu’s second portfolio for Mason’s R course. This portfolio tells a story about the first ACE model to model the height of DZ twins and ambiguious MZ twins, in order to get the heritability of height using a sample of CFPS. By building the model, I learnt the basic knowledge of using Openmx package to build coviriance models.

### The Openmx is using a structure of building models on Matrixes, which is more compatible for ACE model. Also it is also possible to use “path” to create the ACE model, but it is hard to assign costumised coviriance for the latent variables (here the A, C and E).

``` r
library(OpenMx)
library(umx)
library(psych)

mzData    <- df_link.twins.AM[,c(15,18)]
dzData    <- df_link.twins.DZ[,c(15,18)]

covMZ <- cov(mzData, use = "pairwise")
covDZ <- cov(dzData, use = "pairwise")

mean(rbind(mzData,dzData)[,1], na.rm = TRUE)

testmodel <- umxACEv(
  name='ACE',
  selDVs='currentheight_S', # the phenotype
  selCovs=NULL, # fixed covariates ... none here
  dzData=dzData, # the DZ data frame
  mzData=mzData, # the MZ data frame
  dzAr=.5,
  sep='') 
summary(testmodel)

nv <- 1
ntv <- 2
selVars   <- c('currentheight_S1','currentheight_S2')
#aceVars   <- c("A1","C1","E1","A2","C2","E2")

#start values
svBe <- .01
svMu <- 0
svVa <- .2
svVe <- .5

#variance matrix
covA <- mxMatrix(type = "Symm", nrow = nv, ncol = nv,free = TRUE, values = svVa, label = "VA11", name = "VA")
covC <- mxMatrix(type = "Symm", nrow = nv, ncol = nv,free = TRUE, values = svVa, label = "VC11", name = "VC")
covE <- mxMatrix(type = "Symm", nrow = nv, ncol = nv,free = TRUE, values = svVe, label = "VE11", name = "VE")

#expected variance matrix
covP <- mxAlgebra(expression = VA+VC+VE, name = "V")
covMZ <- mxAlgebra(expression = 0.8333*VA+VC, name = "cMZ")
covDZ <- mxAlgebra(expression = 0.5*VA+VC, name = "cDZ")
expCovMz <- mxAlgebra(expression = rbind(cbind(V,cMZ), cbind(t(cMZ),V)), name = "expCovMz")
expCovDz <- mxAlgebra(expression = rbind(cbind(V,cDZ), cbind(t(cDZ),V)), name = "expCovDz")

#create data
dataMZ       <- mxData( observed=mzData, type="raw" )
dataDZ       <- mxData( observed=dzData, type="raw" )


# Mean Matrix
intercept <- mxMatrix(type = "Full", nrow= 1 , ncol = ntv, free = TRUE, values = 105.112, labels = "interC", name = "intercept")
expMean <- mxAlgebra(expression = 1*intercept , name = "expMean")

# Create expectation objects
expMZ <- mxExpectationNormal(covariance = "expCovMz", means ="expMean", dimnames = selVars)
expDZ <- mxExpectationNormal(covariance = "expCovDz", means ="expMean", dimnames = selVars)
funML <- mxFitFunctionML()

#Create models
pars <- list(intercept, covA, covC, covE, covP)
modelMZ <- mxModel(pars, expMean,covMZ,expCovMz,dataMZ,expMZ,funML,name = "MZ")
MZfit <- mxRun(modelMZ, intervals = TRUE)
summary(MZfit)
modelDZ <- mxModel(pars, expMean,covDZ,expCovDz,dataDZ,expDZ,funML,name = "DZ")
DZfit <- mxRun(modelDZ, intervals = TRUE)
summary(DZfit)
```

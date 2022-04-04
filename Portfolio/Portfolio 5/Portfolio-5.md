Portfolio 5
================
Xuanyu
4/4/2022

### This document is Xuanyu’s 5th portfolio for Mason’s R course. This portfolio demontrates how to do multilevel analysis and Manova in R, which is derived from Homework 2 in Mike’s stats course. Although it is not very creative or very arduous to do those analysis, I felt I retained some new knowledge for doing multivariate analysis and some new insights on how the wrappers for lm() function can be applied. And those knowledge can be important for Eric’s Contrast package.

#### MLM

``` r
MLM <- read_sav("MLM.sav")

MLM <- MLM |> filter(is.na(Person) == FALSE)

MLM$Person <- as.factor(MLM$Person)

MLM <- MLM |> group_by(Person) |> mutate(meanNumpeople = mean(NumPeople)) 
MLM <- MLM |> mutate(Ext_c = Ext - 0.5)

MLM$NumPeople_c <- MLM$NumPeople - MLM$meanNumpeople

# The basic multilevel model. In the parentheses, we can assign the level one observations and the level two restriction for the level one observations. It can also be understood as the random effects of person for the level one IV. 
lmer(Hap ~ NumPeople_c + Ext_c + NumPeople_c*Ext_c + (NumPeople | Person),
     data = MLM) |> summary()
```

    ## boundary (singular) fit: see ?isSingular

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Hap ~ NumPeople_c + Ext_c + NumPeople_c * Ext_c + (NumPeople |  
    ##     Person)
    ##    Data: MLM
    ## 
    ## REML criterion at convergence: 1683.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.74195 -0.28089 -0.08517  0.25177  2.29368 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  Person   (Intercept) 151.254  12.299        
    ##           NumPeople     3.983   1.996   -1.00
    ##  Residual              56.957   7.547        
    ## Number of obs: 240, groups:  Person, 24
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)        47.7500     1.0270 21.5043  46.495  < 2e-16 ***
    ## NumPeople_c        -0.3333     0.5210 25.5385  -0.640    0.528    
    ## Ext_c              12.5000     2.0540 21.5043   6.086 4.40e-06 ***
    ## NumPeople_c:Ext_c   9.1111     1.0420 25.5385   8.744 3.74e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NmPpl_ Ext_c 
    ## NumPeople_c -0.671              
    ## Ext_c       -0.340  0.155       
    ## NmPpl_c:Ex_  0.155  0.000 -0.671
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
# Decompose the significant interaction
lmer(Hap ~ NumPeople_c + (NumPeople | Person) ,
     data = MLM[which(MLM$Ext==1),]) |> summary()
```

    ## boundary (singular) fit: see ?isSingular

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Hap ~ NumPeople_c + (NumPeople | Person)
    ##    Data: MLM[which(MLM$Ext == 1), ]
    ## 
    ## REML criterion at convergence: 849.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.75278 -0.32713 -0.05592  0.13616  2.13412 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  Person   (Intercept) 273.455  16.536        
    ##           NumPeople     6.729   2.594   -1.00
    ##  Residual              59.315   7.702        
    ## Number of obs: 120, groups:  Person, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  54.0000     1.5702 11.2778   34.39 8.92e-13 ***
    ## NumPeople_c   4.2222     0.8834 11.7546    4.78 0.000475 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## NumPeople_c -0.758
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
lmer(Hap ~ NumPeople_c + (NumPeople | Person) ,
     data = MLM[which(MLM$Ext==0),]) |> summary()
```

    ## boundary (singular) fit: see ?isSingular

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Hap ~ NumPeople_c + (NumPeople | Person)
    ##    Data: MLM[which(MLM$Ext == 0), ]
    ## 
    ## REML criterion at convergence: 824
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5777 -0.2487  0.1114  0.3683  2.1513 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  Person   (Intercept) 67.777   8.233         
    ##           NumPeople    3.004   1.733    -1.00
    ##  Residual             51.838   7.200         
    ## Number of obs: 120, groups:  Person, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  41.5000     0.9073 14.0900  45.742  < 2e-16 ***
    ## NumPeople_c  -4.8889     0.6651 12.4933  -7.351 7.02e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## NumPeople_c -0.519
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
# Investigate the effects of centering in the multilevel models. 
lmer(Hap ~ NumPeople_c + Ext + NumPeople_c*Ext + (NumPeople | Person),
     data = MLM)|> summary()
```

    ## boundary (singular) fit: see ?isSingular

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Hap ~ NumPeople_c + Ext + NumPeople_c * Ext + (NumPeople | Person)
    ##    Data: MLM
    ## 
    ## REML criterion at convergence: 1683.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.74195 -0.28089 -0.08517  0.25177  2.29368 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  Person   (Intercept) 151.254  12.299        
    ##           NumPeople     3.983   1.996   -1.00
    ##  Residual              56.957   7.547        
    ## Number of obs: 240, groups:  Person, 24
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)      41.5000     1.6814 19.6005  24.682 3.13e-16 ***
    ## NumPeople_c      -4.8889     0.7368 25.5385  -6.635 5.34e-07 ***
    ## Ext              12.5000     2.0540 21.5043   6.086 4.40e-06 ***
    ## NumPeople_c:Ext   9.1111     1.0420 25.5385   8.744 3.74e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) NmPpl_ Ext   
    ## NumPeople_c -0.713              
    ## Ext         -0.819  0.584       
    ## NmPpl_c:Ext  0.504 -0.707 -0.671
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

``` r
lmer(Hap ~ NumPeople_c + (NumPeople | Person) ,
     data = MLM[which(MLM$Ext==1),]) |> summary()
```

    ## boundary (singular) fit: see ?isSingular

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Hap ~ NumPeople_c + (NumPeople | Person)
    ##    Data: MLM[which(MLM$Ext == 1), ]
    ## 
    ## REML criterion at convergence: 849.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.75278 -0.32713 -0.05592  0.13616  2.13412 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  Person   (Intercept) 273.455  16.536        
    ##           NumPeople     6.729   2.594   -1.00
    ##  Residual              59.315   7.702        
    ## Number of obs: 120, groups:  Person, 12
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  54.0000     1.5702 11.2778   34.39 8.92e-13 ***
    ## NumPeople_c   4.2222     0.8834 11.7546    4.78 0.000475 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## NumPeople_c -0.758
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see ?isSingular

#### MANOVA

``` r
homeless <- read_sav("Homelessness.sav")

# Assumption: Medium correlation between DVs.
cor.test(homeless$QOL, homeless$GHS)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  homeless$QOL and homeless$GHS
    ## t = 4.0509, df = 58, p-value = 0.0001535
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2448912 0.6464512
    ## sample estimates:
    ##       cor 
    ## 0.4696135

``` r
# Assumption: Homogeneity of variance across all groups--the restriction for least square estimation
library(heplots)
```

    ## Loading required package: car

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
homeless$Homeless <- as.factor(homeless$Homeless)
boxM(cbind(QOL,GHS) ~ Homeless, data = homeless)
```

    ## 
    ##  Box's M-test for Homogeneity of Covariance Matrices
    ## 
    ## data:  Y
    ## Chi-Sq (approx.) = 6.4968, df = 6, p-value = 0.3699

``` r
# Assumption: Multivariate normality
library(mvnormalTest)
mardia(homeless[, c("QOL", "GHS")])
```

    ## $mv.test
    ##           Test Statistic p-value Result
    ## 1     Skewness    2.9988   0.558    YES
    ## 2     Kurtosis    0.2021  0.8399    YES
    ## 3 MV Normality      <NA>    <NA>    YES
    ## 
    ## $uv.shapiro
    ##     W      p-value UV.Normality
    ## QOL 0.8876 0       No          
    ## GHS 0.8961 1e-04   No

``` r
# The omnibus manova model
l1 <- manova(cbind(QOL,GHS) ~ Homeless, data = homeless)
summary(l1,test = c( "Wilks"))
```

    ##           Df  Wilks approx F num Df den Df  Pr(>F)  
    ## Homeless   2 0.8018   3.2698      4    112 0.01412 *
    ## Residuals 57                                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# The effect size for the omnibus model. It requires another package. In our contrast package, we can possibly combine those typical effect sizes and criterions in psychology into our package, or at least, put them as dependencies of our package. 
library(heplots)
etasq(l1, test = c( "Wilks"))
```

    ##              eta^2
    ## Homeless 0.1045665

``` r
# The anova model for each DV
l2 <- aov(cbind(QOL,GHS) ~ Homeless, data = homeless)
summary(l2)
```

    ##  Response QOL :
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Homeless     2    3.1  1.5500   1.374 0.2613
    ## Residuals   57   64.3  1.1281               
    ## 
    ##  Response GHS :
    ##             Df Sum Sq Mean Sq F value  Pr(>F)  
    ## Homeless     2  8.233  4.1167  4.3174 0.01795 *
    ## Residuals   57 54.350  0.9535                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# The effect size for the significant anova.
l3 <- aov(GHS ~ Homeless, data = homeless)
etasq(l3)
```

    ##           Partial eta^2
    ## Homeless      0.1315579
    ## Residuals            NA

``` r
# Post-hoc comparisons of the sinificant anova model
library(multcomp)
```

    ## Loading required package: mvtnorm

    ## Loading required package: survival

    ## Loading required package: TH.data

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## 
    ## Attaching package: 'TH.data'

    ## The following object is masked from 'package:MASS':
    ## 
    ##     geyser

``` r
# Tukey HSD test:
post_test <- glht(aov(GHS~Homeless, data = homeless), 
                  linfct = mcp(Homeless = "Tukey")
)
summary(post_test)
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: aov(formula = GHS ~ Homeless, data = homeless)
    ## 
    ## Linear Hypotheses:
    ##            Estimate Std. Error t value Pr(>|t|)  
    ## 2 - 1 == 0  -0.9000     0.3088  -2.915   0.0139 *
    ## 3 - 1 == 0  -0.5500     0.3088  -1.781   0.1851  
    ## 3 - 2 == 0   0.3500     0.3088   1.133   0.4977  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- single-step method)

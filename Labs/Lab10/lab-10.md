Lab 10 - Grading the professor, Pt. 2
================
Xuanyu
04/08/2022

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

### Exercise 1

``` r
df <- evals
lm(score~bty_avg, data = df) |> summary()
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

### Exercise 2

``` r
lm(score~bty_avg+gender, data = df) |> summary()
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

``` r
df |> group_by(gender) |> summarise(mean(score))
```

    ## # A tibble: 2 x 2
    ##   gender `mean(score)`
    ##   <fct>          <dbl>
    ## 1 female          4.09
    ## 2 male            4.23

### Exercise 3

The intercept of 3.75 means for female and the people scoring 0 on
beauty score, the predicted level of teaching score is 3.75. The 0.07
estimate of beauty score indicates a positive association between beauty
score and teaching score, controlling for gender. Controlling for beauty
score, being a male can lead to a .17 increase in teaching score.

### Exercise 4

5.5%

### Exercise 5

Male

### Exercise 6

3.74 + .07*beauty + .17*1

### Exercise 7

``` r
lm(score~bty_avg+gender+bty_avg*gender, data = df) |> summary()
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender + bty_avg * gender, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8084 -0.3828  0.0903  0.4037  0.9211 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         3.95006    0.11800  33.475   <2e-16 ***
    ## bty_avg             0.03064    0.02400   1.277   0.2024    
    ## gendermale         -0.18351    0.15349  -1.196   0.2325    
    ## bty_avg:gendermale  0.07962    0.03247   2.452   0.0146 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5258 on 459 degrees of freedom
    ## Multiple R-squared:  0.07129,    Adjusted R-squared:  0.06522 
    ## F-statistic: 11.74 on 3 and 459 DF,  p-value: 1.997e-07

The positive association is stronger for males than females.

### Exercise 8

The R squared value increase a little bit. Gender helps us better
understand the relationship between beauty and teaching score.

### Exercise 9

The slope becomes larger after controlling for gender.

### Exercise 10

``` r
lm(score~bty_avg+rank, data = df) |> summary()
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

### Exercise 11

I think ethnicity will not have a great predictive power on the teaching
scores. While professors of each ethnicity can be a good professor.

### Exercise 12

``` r
lm(score~ethnicity, data = df) |> summary()
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8912 -0.3816  0.1088  0.4088  0.9281 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            4.07188    0.06786  60.003   <2e-16 ***
    ## ethnicitynot minority  0.11935    0.07310   1.633    0.103    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5429 on 461 degrees of freedom
    ## Multiple R-squared:  0.005749,   Adjusted R-squared:  0.003593 
    ## F-statistic: 2.666 on 1 and 461 DF,  p-value: 0.1032

My guess is true. There’s no sig association between ethnicity and
teaching score.

### Exercise 13

cls_did_eval should not be included as its information is the same as
the conduct of cls_perc_eval and cls_students.

### Exercise 14

``` r
lm(score~rank+ ethnicity+ gender+ language+ age+ cls_perc_eval+cls_students+ cls_level+ cls_profs+ cls_credits+ bty_avg, data = df) |> summary()
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

### Exercise 15

``` r
all <- lm(score~rank+ ethnicity+ gender+ language+ age+ cls_perc_eval+cls_students+ cls_level+ cls_profs+ cls_credits+ bty_avg, data=df)
backward <- stats::step(all, direction = "backward", scope = formula(all), trace = 0)
summary(backward)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_credits + bty_avg, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9067 -0.3103  0.0849  0.3712  1.0611 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.446967   0.203191  16.964  < 2e-16 ***
    ## ethnicitynot minority  0.204710   0.074710   2.740 0.006384 ** 
    ## gendermale             0.184780   0.049889   3.704 0.000238 ***
    ## languagenon-english   -0.161463   0.103213  -1.564 0.118427    
    ## age                   -0.005008   0.002606  -1.922 0.055289 .  
    ## cls_perc_eval          0.005094   0.001438   3.543 0.000436 ***
    ## cls_creditsone credit  0.515065   0.104860   4.912 1.26e-06 ***
    ## bty_avg                0.064996   0.016327   3.981 7.99e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.503 on 455 degrees of freedom
    ## Multiple R-squared:  0.1576, Adjusted R-squared:  0.1446 
    ## F-statistic: 12.16 on 7 and 455 DF,  p-value: 2.879e-14

### Exercise 16

While after controlling age, gender, language, credit approach, beauty
score and evaluation percentile, ethnicity becomes a significant
predictor for teaching score. Being a non-minority teacher will lead to
higher teaching score.

Also, the more student finish the evaluation, the higher score the
teacher will get.

### Exercise 17

A white, male, handsome professor offering a one-credit evaluation
course will receive a high evaluation score from students.

### Exercise 18

I think some of the conclusion will be generalized to other universities
like the positive association between good looking and score, and also
the one credit evaluation approach.

However, I don’t think for a university located in more liberal states
or cities, the ethnicity and gender may not be a key factor for the
course evaluation score.

Lab 01 - Hello R
================
Xuanyu
01/12/2022

## Load packages and data

``` r
library(tidyverse) 
library(datasauRus)
```

## Exercises

### Exercise 1

There’s 1846 rows and 3 columns in the data file. Three variables
including the names of 13 datasets, the x value and the y values.

### Exercise 2

The plot of the dino dataset

``` r
dino_data <- datasaurus_dozen %>%
  filter(dataset == "dino")

ggplot(data = dino_data, mapping = aes(x = x, y = y)) +
  geom_point()
```

![](lab-01-hello-r_files/figure-gfm/plot-dino-1.png)<!-- -->

The correlation in the dino dataset.

``` r
dino_data %>%
  summarize(r = cor(x, y))
```

    ## # A tibble: 1 x 1
    ##         r
    ##     <dbl>
    ## 1 -0.0645

### Exercise 3

The plot in the star dataset

``` r
star_data <- datasaurus_dozen %>% filter(dataset == "star")
star_data %>% ggplot(mapping = aes(x,y)) + geom_point()
```

![](lab-01-hello-r_files/figure-gfm/plot-star-1.png)<!-- -->

The correlation coefficient in the star dataset. The correlation
coefficient is quite similar to the one in the dino dataset.

``` r
star_data %>% summarise(r = cor(x,y)) 
```

    ## # A tibble: 1 x 1
    ##         r
    ##     <dbl>
    ## 1 -0.0630

### Exercise 4

The plot in the circle dataset

``` r
circle_data <- datasaurus_dozen %>% filter(dataset == "circle")
circle_data %>% ggplot(mapping = aes(x,y)) + geom_point()
```

![](lab-01-hello-r_files/figure-gfm/plot-circle-1.png)<!-- -->

The correlation coefficient in the circle dataset. The coefficient is a
little greater than the previous two.

``` r
circle_data %>% summarise(r = cor(x,y)) 
```

    ## # A tibble: 1 x 1
    ##         r
    ##     <dbl>
    ## 1 -0.0683

### Exercise 5

The plot for all datasets

``` r
ggplot(datasaurus_dozen, aes(x = x, y = y, color = dataset))+
  geom_point()+
  facet_wrap(~ dataset, ncol = 4) +
  theme(legend.position = "none")
```

![](lab-01-hello-r_files/figure-gfm/plot%20for%20all-1.png)<!-- -->

The r coeffcients for all datasets

``` r
datasaurus_dozen %>%
  group_by(dataset) %>%
  summarize(r = cor(x, y)) %>%
    print(13)
```

    ## # A tibble:
    ## #   13 x 2
    ##    dataset   
    ##    <chr>     
    ##  1 away      
    ##  2 bullseye  
    ##  3 circle    
    ##  4 dino      
    ##  5 dots      
    ##  6 h_lines   
    ##  7 high_lines
    ##  8 slant_down
    ##  9 slant_up  
    ## 10 star      
    ## 11 v_lines   
    ## 12 wide_lines
    ## 13 x_shape   
    ## # ... with 1
    ## #   more
    ## #   variable:
    ## #   r <dbl>

## Bonus Tips by Yoo Ri

### Thanks Yoo Ri! *–Xuanyu*

Here are some helpful tips :)

-   filter() is for extracting rows

-   group_by() is for grouping datasets by assigned column

-   ungroup() cancels the grouping

-   summarize() is often used with group_by(). This function can print
    the output according to the group_by().

-   facet_grid(y\~x,…) creates a grid with variable y as a row, variable
    x as a column  

-   facet_wrap(x,… ) is useful when there is only one variable

Portfolio 4
================
Xuanyu
3/31/2022

### This document is Xuanyu’s 4th portfolio for Mason’s R course. This portfolio was a function to generate the coefficients matrix for contrasts analysis in anova. The function is also the symbol that our ambitious contrasts.eric package is on its way. ### Also, it is also a sigh that I have grasp the basic knowledge to initialize a package.

### More specifically, I learned the expand.grid function to generate a full combination of different elements. Also, I learned the value of list object in writing functions. It is much more compitable than any other objects.

#### In order to make your hypotheses to be correctly converted to a coefficient matrix, you need to follow the rules below:

#### levels: a list of character vectors indicating which levels of the factor your hypotheses are related to

#### coe: a list of numeric vectors indicating what the coefficients for each of your hypotheses, corresponding to the character vector in levels

#### col_factor: The columns of all different factors involved in the testing

``` r
#### A function to generate coefficients matrix


#### In order to make your hypotheses to be correctly converted to a coefficient matrix, you need to follow the rules below:
#### levels: a list of character vectors indicating which levels of the factor your hypotheses are related to
#### coe: a list of numeric vectors indicating what the coefficients for each of your hypotheses, corresponding to the character vector in levels
#### col_factor: The columns of all different factors involved in the testing.

coe_matrix_multi <- function(colFactor, levels, coe){
      colFactor <- as.data.frame(colFactor)
      ## Create the empty coefficients table
      numFactor <- ifelse(is.null(ncol(colFactor)), 1, ncol(colFactor))
      l <- list()
      for (i in 1:numFactor) {
        l[[i]] <- as.character(unique(colFactor[,i]))
      }
      allcomb <- expand.grid(l)
      #print(allcomb)

      all_levels <- character()
      if(ncol(allcomb)==1){
        for(i in 1:nrow(allcomb)){
          all_levels[i] <- as.character(allcomb[i,1])
        }
      }else{
        for(i in 1:nrow(allcomb)){
          all_levels[i] <- do.call(paste, c(allcomb[i,], sep = "X"))
        }
      }

      #print(all_levels)

      coe_matrix <- as.data.frame(matrix(
        data = 0,
        ncol = length(all_levels),
        nrow = length(coe)
      ))
      #print(coe_matrix)
      colnames(coe_matrix) <- all_levels

      ## put the selected coefficient list into the full coefficient matrix
      for (i in 1: length(levels)){
        #temp_coe_matrix <- matrix(coe[i], nrow = 1, ncol = length(coe[i]) )
        #tempcol <- levels[i]
        coe_matrix[i,][levels[[i]]] <- coe[[i]]
      }

      return(coe_matrix)
}
```

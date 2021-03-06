Portfolio1
================
Xuanyu Lyu
2/5/2022

# Two functions with active objects inside the functions

My first portfolio is two functions that I used for Laura’s limit
breaking project. What I learnt by coding those two functions is two
approaches to make active parameters inside a function.

## the First function

The first function is to get costomized dataframe with specific
parenting style, the year of the relative parenting styles and the year
of behaviors. In this function I used the paste() function to achieve
the activity of parameters, which is the specific columns with assigned
years or parenting styles.And it’s a mandotory function for the second
function to operize.

``` r
# the function for getting comparison dataframe
get_a_df <- function(Ptype.x, Ptype.y,
                     yearPtype = 1997, yearBtype = 1997){
  df_temp <- df_double_clean[1,]
  j <- 1
  var_PtypeS1 = paste("Pstyle_", yearPtype, "_S1", sep = "")
  var_PtypeS2 = paste("Pstyle_", yearPtype, "_S2", sep = "")
  for(i in 1:nrow(df_double_clean)){
    if(is.na(df_double_clean[i,var_PtypeS1]) == TRUE | is.na(df_double_clean[i,var_PtypeS2]) == TRUE){
      next
    }
    if(df_double_clean[i,var_PtypeS1] == Ptype.x & df_double_clean[i,var_PtypeS2] == Ptype.y | df_double_clean[i,var_PtypeS2] == Ptype.x & df_double_clean[i,var_PtypeS1] == Ptype.y  ){
      df_temp[j,] = df_double_clean[i,]
      j = j+1
      next
    }
  }
  
  df_Ptype <- df_temp[,2:9] %>% select(grep(as.character(yearPtype), names(df_temp[,2:9])))
  df_Btype <- df_temp[,10:ncol(df_temp)] %>% select(grep(as.character(yearBtype), names(df_temp[,10:ncol(df_temp)])))
  
  df_target <- cbind("pairid" = df_temp[,1], df_Ptype, df_Btype)
  
  
  return(df_target)
}
```

## the Second function

The second function is designed to get the results of discordant kinship
model for a specific year and a specific combination of parenting
styles. I used the “{{}}” and “:=” to call the active objects produced
in the same function. For example, I created a character factor called
“ave” and I want to use the value of this factor rather than the “ave”
charactor in the mutate() of dplyr. By default, the metate() will treat
ave as “ave” and doesn’t call its value. But with the two symbols, we
can create new columns named by the value of ave rather than “ave”.

``` r
### a function to get results


getResults <- function(yearPS, PS1, PS2, Btype){
  df1 = merge(get_a_df(PS1, PS1, yearPS, 1997),y = df_double_clean, by = "pairid")[,c(1:3,34:36,49:51)]
  df2 = merge(get_a_df(PS1, PS2, yearPS, 1997),y = df_double_clean, by = "pairid")[,c(1:3,34:36,49:51)]
  df3 = merge(get_a_df(PS2, PS2, yearPS, 1997),y = df_double_clean, by = "pairid")[,c(1:3,34:36,49:51)]
  
  df1$group = as.factor(paste(PS1,"_",PS1, sep = ""))
  df2$group = as.factor(paste(PS1,"_",PS2, sep = ""))
  df3$group = as.factor(paste(PS2,"_",PS2, sep = ""))
  
  df_PS12 = rbind(df1,df2,df3)
  colnames(df_PS12)[2:3] = c(paste("Pstyle",yearPS,"_S1", sep = ""),
                             paste("Pstyle",yearPS,"_S2", sep = ""))
  if(grepl(Btype,"staylate")){
    c1 = 4
    c2 = 7
  } else if(grepl(Btype,"tvkind")){
    c1 = 5
    c2 = 8
  } else if(grepl(Btype,"hangout")){
    c1 = 6
    c2 = 9
  } else{
    stop("should be staylate, tvkind or hangout")
  }
  df_model = df_PS12 |> filter(!is.na(df_PS12[,c1]) & !is.na(df_PS12[,c2]))
  diffe = paste("diff",Btype, sep = "_")
  ave = paste("average",Btype, sep = "_")
  df_model = df_model |> mutate({{diffe}} := df_model[,c1] - df_model[,c2],{{ave}} := (df_model[,c1] + df_model[,c2])/2)
  fomula = paste(diffe, "~group+",ave,sep = "")
  mySummary = df_model |> lm(formula =  as.formula(fomula)) |> summary()
  return(mySummary)
}
```

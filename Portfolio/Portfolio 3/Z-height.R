heightSD_boy <- read.csv("Height_Boy.csv", header = TRUE)
colnames(heightSD_boy)[1] <- "Year"
heightSD_girl <- read.csv("Height_Girl.csv", header = TRUE)
colnames(heightSD_girl)[1] <- "Year"

df_links_zscore <- df_links.fullsibonly

calculateCurrentMonth <- function(birthyear, birthmonth, surveyyear, surveymonth){
  CurrentMonth <- (surveyyear-birthyear)*12 +surveymonth -birthmonth
  if(!is.na(CurrentMonth)){
    print(CurrentMonth)
    return(CurrentMonth)
  } else{
    return(as.numeric(NA))
  }
}

ComputeZ <- function(CurrentMonth,gender,height){
  if(!(is.na(CurrentMonth) | is.na(gender) | is.na(height))){
    if(gender==0){
      ruler = heightSD_girl
    }else{
      ruler = heightSD_boy
    }
    index <- which.min(abs(ruler$Month-CurrentMonth))
    Z <- (height - ruler$Median[index])/ruler$SD[index]
    return(Z)
  }else{
    return(as.numeric(NA))
  }

}

df_links_zscore$Zheight_S1 <- as.numeric(NA)
df_links_zscore$Zheight_S2 <- as.numeric(NA)
for (i in 1:nrow(df_links_zscore)) {
  CurrentMonth = calculateCurrentMonth(df_links_zscore$birthyear_S1[i],
                                       df_links_zscore$birthmonth_S1[i],
                                       df_links_zscore$ch_year_S1[i],
                                       df_links_zscore$ch_month_S1[i])
  df_links_zscore$Zheight_S1[i] = ComputeZ(CurrentMonth, df_links_zscore$gender_S1[i], df_links_zscore$currentheight_S1[i])
  
  CurrentMonth = calculateCurrentMonth(df_links_zscore$birthyear_S2[i],
                                       df_links_zscore$birthmonth_S2[i],
                                       df_links_zscore$ch_year_S2[i],
                                       df_links_zscore$ch_month_S2[i])
  df_links_zscore$Zheight_S2[i] = ComputeZ(CurrentMonth, df_links_zscore$gender_S2[i], df_links_zscore$currentheight_S2[i])
}


###
df_links.twins.Z <- df_links_zscore[which(df_links_zscore$birthyear_S1 == df_links_zscore$birthyear_S2 & df_links_zscore$birthmonth_S1 == df_links_zscore$birthmonth_S2),]
df_links.twins.clean.Z <- df_links.twins.Z |> filter( !(birthweight_S1 == birthweight_S2 & currentheight_S1 == currentheight_S2 & currentweight_S1 == currentheight_S2))

df_link.twins.DZ.Z <- df_links.twins.clean.Z |> filter(gender_S1 != gender_S2)

df_link.twins.AM.Z <- df_links.twins.clean.Z |> filter(gender_S1 == gender_S2)


df_links.fullsib.highcred.Z <- df_links_zscore[which(df_links_zscore$cred==3 & !(df_links_zscore$`1` %in% df_links.twins.Z$`1`) ), ]

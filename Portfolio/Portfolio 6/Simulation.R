setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Functions for simulation
source("func_kinsim.R")

#set.seed(601)
l.finaldf <- list()

# 5000 simulations of ace=10,10,10
for(k in 1:100){
      l.finaldf_k <- list()
      for(j in 1: 50){
            set.seed(j)
            l.finaldf_k[[j]] <- simulation_whole(50,30*k,c(10,10,10))
      }  
      l.finaldf[[k]] <- l.finaldf_k
}

# 5000 simulations of ace=34,100,5
for(k in 1:100){
      l.finaldf_k <- list()
      for(j in 1: 50){
            set.seed(j)
            l.finaldf_k[[j]] <- simulation_whole(50,30*k,c(34,100,5))
      }  
      l.finaldf[[k]] <- l.finaldf_k
}



simulation_whole(50,100,c(10,10,10))

simulation_whole <- function(nmodel = 50, npg = 500, V_ace = c(1,1,1)){
      
      # simulated dataframes
      l.sim <- list()
      
      for(i in 1: nmodel){
            r_ambi <- .5 + i/(nmodel*2)
            l.sim[[i]] <- kinsim(
                  r_all = c(r_ambi, 0.5),
                  npg_all = npg,
                  variables = 1,
                  ace_all = V_ace
            )
      }
      
      # put simulated data into the ACE model
      l.output <- list()
      
      for(i in 1:nmodel){
            r_ambi <- .5 + i/(nmodel*2)
            l_doubledata <- sep_kin(l.sim[[i]])
            l.output[[i]] <- fit_siACE(l_doubledata[[1]], l_doubledata[[2]],r_ambi)
      }
      
      # Get the estimated variance components for each model
      df_output <- data.frame("Model"= 1:nmodel, 
                              "coeAM"= as.numeric(NA),
                              "V" = as.numeric(NA),
                              "VA" = as.numeric(NA),
                              "VC" = as.numeric(NA),
                              "VE" = as.numeric(NA))
      for(i in 1:nmodel){
            r_ambi <- .5 + i/(nmodel*2)
            df_output[i,2] <- r_ambi
            df_output[i,3] <- l.output[[i]]@algebras$V$result[1]
            df_output[i,4:6] <- l.output[[i]]@algebras$US$result[1:3]
      }
      return(df_output)
}


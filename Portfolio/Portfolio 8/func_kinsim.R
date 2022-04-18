#' Simulate Biometrically informed Multivariate Data
#'
#' @description Generate paired multivariate data, given ACE parameters.
#' @importFrom stats rnorm sd
#' @param r_all Levels of relatedness; default is MZ and DZ twins c(1,.5).
#' @param npg_all Sample size per group; default is 500.
#' @param npergroup_all Vector of sample sizes by group; default repeats \code{npg_all} for all groups
#' @param variables Number of variables to generate; default is 2. Currently, limited to max of two variables.
#' @param mu_all Mean for each generated variable; default is 0.
#' @param mu_list List of means by variable; default repeats \code{mu_all} for all variables
#' @param r_vector Alternative, give vector of r coefficients for entire sample.
#' @param ace_all Vector of variance components for each generated variable; default is c(1,1,1).
#' @param ace_list Matrix of ACE variance components by variable, where each row is its own variable; default is to repeat \code{ace_all} for each variable.
#' @param ... Optional pass on additional inputs.
#' @param cov_a Shared variance for additive genetics (a); default is 0.
#' @param cov_c Shared variance for shared-environment (c); default is 0.
#' @param cov_e shared variance for non-shared-environment (e); default is 0.

#' @return Returns \code{data.frame} with the following:
#' \item{Ai_1}{genetic component for variable i for kin1}
#' \item{Ai_2}{genetic component for variable i for kin2}
#' \item{Ci_1}{shared-environmental component for variable i for kin1}
#' \item{Ci_2}{shared-environmental component for variable i for kin2}
#' \item{Ei_1}{non-shared-environmental component for variable i for kin1}
#' \item{Ei_2}{non-shared-environmental component for variable i for kin2}
#' \item{yi_1}{generated variable i for kin1}
#' \item{yi_2}{generated variable i for kin2}
#' \item{r}{level of relatedness for the kin pair}
#' \item{id}{id}


kinsim <- function(
     r_all=c(1,.5),
     npg_all=500,
     npergroup_all=rep(npg_all,length(r_all)),
     mu_all=0,
     variables=2,
     mu_list=rep(mu_all,variables),
     r_vector=NULL, # alternative specification, give vector of rs
     ace_all=c(1,1,1), # variance default
     ace_list=matrix(rep(ace_all,variables),byrow=TRUE,nrow=variables),
     cov_a=0, #default shared covariance for genetics across variables
     cov_c=0, #default shared variance for c across variables
     cov_e=0, #default shared variance for e across variables
     ...){
     mu <- NULL
     sA <- ace_list[,1]^0.5
     sC <- ace_list[,2]^0.5
     sE <- ace_list[,3]^0.5
     S2 <- diag(4)*-1+1
     
     datalist <- list()
     if(variables==1){
          data_v<-kinsim_internal(r = r_all,
                                  npergroup = npergroup_all,	#
                                  mu = mu_list[1],			#intercept
                                  ace = ace_list[[1]],
                                  r_vector = r_vector
          )
          data_v$A1_u<-data_v$A1
          data_v$A2_u<-data_v$A2
          data_v$C1_u<-data_v$C1
          data_v$C2_u<-data_v$C2
          data_v$E1_u<-data_v$E1
          data_v$E2_u<-data_v$E2
          data_v$y1_u<-data_v$y1
          data_v$y2_u<-data_v$y2
          
          merged.data.frame = data_v
          names(merged.data.frame)[c(1,10)]<-c("id","r")
     }
     if(variables > 2){
          stop("You have tried to generate data beyond the current limitations of this program. Maximum variables 2.")
     }
     if(is.null(r_vector)){
          id <- 1:sum(npergroup_all)
          for(i in 1:length(r_all)){
               n = npergroup_all[i]
               
               # Genetic Covariance
               sigma_a <- diag(4) + S2*r_all[i]
               sigma_a[1,3] <- cov_a
               sigma_a[3,1] <- cov_a
               sigma_a[2,4] <- cov_a
               sigma_a[4,2] <- cov_a
               sigma_a[1,4] <- cov_a*r_all[i]
               sigma_a[4,1] <- cov_a*r_all[i]
               sigma_a[3,2] <- cov_a*r_all[i]
               sigma_a[2,3] <- cov_a*r_all[i]
               A.r <- rmvn(n,
                           sigma = sigma_a)
               
               A.r[,1:2]<- A.r[,1:2]*sA[1]
               A.r[,3:4]<- A.r[,3:4]*sA[2]
               
               # Shared C Covariance
               sigma_c<-diag(4)+S2*1
               sigma_c[1,3]<-cov_c
               sigma_c[3,1]<-cov_c
               sigma_c[2,4]<-cov_c
               sigma_c[4,2]<-cov_c
               sigma_c[1,4]<-cov_c*1
               sigma_c[4,1]<-cov_c*1
               sigma_c[3,2]<-cov_c*1
               sigma_c[2,3]<-cov_c*1
               C.r <- rmvn(n,
                           sigma = sigma_c)
               C.r[,1:2]<- C.r[,1:2]*sC[1]
               C.r[,3:4]<- C.r[,3:4]*sC[2]
               
               # Shared E Covariance
               sigma_e <- diag(4)+S2*0
               sigma_e[1,3] <- cov_e
               sigma_e[3,1] <- cov_e
               sigma_e[2,4] <- cov_e
               sigma_e[4,2] <- cov_e
               E.r <- rmvn(n,
                           sigma = sigma_e)
               E.r[,1:2]<- E.r[,1:2]*sE[1]
               E.r[,3:4]<- E.r[,3:4]*sE[2]
               
               # total score
               y.r <-  A.r + C.r + E.r
               
               
               y.r[,1:2] <- y.r[,1:2] + mu_list[1]
               y.r[,3:4] <- y.r[,3:4] + mu_list[2]
               r_ <- rep(r_all[i],
                         n)
               
               data.r<-data.frame(A.r,C.r,E.r,y.r,r_)
               names(data.r)<-c("A1_1","A1_2",
                                "A2_1","A2_2",
                                "C1_1","C1_2",
                                "C2_1","C2_2",
                                "E1_1","E1_2",
                                "E2_1","E2_2",
                                "y1_1","y1_2",
                                "y2_1","y2_2",
                                "r")
               
               datalist[[i]] <- data.r
               names(datalist)[i] <- paste0("datar",r_all[i])
          }
          merged.data.frame <- Reduce(function(...) merge(..., all=T), datalist)
          merged.data.frame$id <- id
     }else{
          id=1:length(r_vector)
          data_vector <- data.frame(id,
                                    r_vector,
                                    matrix(rep(as.numeric(NA),
                                               length(id)*4),
                                           nrow=length(id),
                                           ncol=4))
          
          names(data_vector) <- c("id","r",
                                  "A1_1","A1_2",
                                  "A2_1","A2_2")
          
          unique_r= matrix(unique(r_vector))
          
          for(i in 1:length(unique_r)){
               n <- length(r_vector[r_vector==unique_r[i]])
               
               # Genetic Covariance
               sigma_a<-diag(4)+S2*unique_r[i]
               sigma_a[1,3]<-cov_a
               sigma_a[3,1]<-cov_a
               sigma_a[2,4]<-cov_a
               sigma_a[4,2]<-cov_a
               sigma_a[1,4]<-cov_a*unique_r[i]
               sigma_a[4,1]<-cov_a*unique_r[i]
               sigma_a[3,2]<-cov_a*unique_r[i]
               sigma_a[2,3]<-cov_a*unique_r[i]
               A.r <- rmvn(n,
                           sigma = sigma_a)
               data_vector$A1_1[data_vector$r_vector==unique_r[i]] <- A.r[,1]*sA[1]
               data_vector$A1_2[data_vector$r_vector==unique_r[i]] <- A.r[,2]*sA[1]
               data_vector$A2_1[data_vector$r_vector==unique_r[i]] <- A.r[,3]*sA[2]
               data_vector$A2_2[data_vector$r_vector==unique_r[i]] <- A.r[,4]*sA[2]
               A.r[,1:2]<- A.r[,1:2]
               A.r[,3:4]<- A.r[,3:4]*sA[2]
          }
          n <- length(r_vector)
          A.r <- matrix(c(data_vector$A1_1,
                          data_vector$A1_2,
                          data_vector$A2_1,
                          data_vector$A2_2),
                        ncol=4,
                        nrow=n)
          # Shared C Covariance
          sigma_c<-diag(4)+S2*1
          sigma_c[1,3]<-cov_c;sigma_c[3,1]<-cov_c;sigma_c[2,4]<-cov_c;sigma_c[4,2]<-cov_c
          sigma_c[1,4]<-cov_c*1;sigma_c[4,1]<-cov_c*1;sigma_c[3,2]<-cov_c*1;sigma_c[2,3]<-cov_c*1
          C.r <- rmvn(n,sigma=sigma_c)
          C.r[,1:2]<- C.r[,1:2]*sC[1]; C.r[,3:4]<- C.r[,3:4]*sC[2]
          
          # Shared E Covariance
          sigma_e<-diag(4)+S2*0
          sigma_e[1,3]<-cov_e;sigma_e[3,1]<-cov_e;sigma_e[2,4]<-cov_e;sigma_e[4,2]<-cov_e
          E.r <- rmvn(n,sigma=sigma_e)
          E.r[,1:2]<- E.r[,1:2]*sE[1]; E.r[,3:4]<- E.r[,3:4]*sE[2]
          
          
          y.r <- A.r
          y.r[,1:2]<-A.r[,1:2]*ace_list[1,1] + C.r[,1:2]*ace_list[1,2] + E.r[,1:2]*ace_list[1,3]
          y.r[,3:4]<-A.r[,3:4]*ace_list[2,1] + C.r[,3:4]*ace_list[2,2] + E.r[,3:4]*ace_list[2,3]
          y.r[,1:2]<-y.r[,1:2]+mu_list[1]
          y.r[,3:4]<-y.r[,3:4]+mu_list[2]
          y.r <- mu + A.r + C.r + E.r
          data.r<-data.frame(A.r,C.r,E.r,y.r,r_vector,id)
          names(data.r)<-c("A1_1","A1_2","A2_1","A2_2","C1_1","C1_2","C2_1","C2_2","E1_1","E1_2","E2_1","E2_2","y1_1","y1_2","y2_1","y2_2","r","id")
          
          
          datalist[[i]] <- data.r
          names(datalist)[i]<-paste0("datar",r_all[i])
          merged.data.frame = data.r
     }
     return(merged.data.frame)
}


rmvn <- function(n,sigma) {
     Sh <- with(svd(sigma),
                v%*%diag(sqrt(d))%*%t(u))
     matrix(stats::rnorm(ncol(sigma)*n),
            ncol = ncol(sigma))%*%Sh
}

kinsim_internal <- function(
     r=c(1,.5),
     npg=100,
     npergroup=rep(npg,length(r)),
     mu=0,
     ace=c(1,1,1),
     r_vector=NULL,
     ...){
     
     sA <- ace[1]^0.5
     sC <- ace[2]^0.5
     sE <- ace[3]^0.5
     
     S2 <- matrix(c(0,1,
                    1,0),2)
     datalist <- list()
     
     if(is.null(r_vector)){
          
          id <- 1:sum(npergroup)
          
          for(i in 1:length(r)){
               
               n = npergroup[i]
               
               A.r <- sA*rmvn(n,
                              sigma = diag(2) + S2*r[i])
               C.r <- stats::rnorm(n,
                                   sd = sC)
               C.r <- cbind(C.r,
                            C.r)
               E.r <- cbind(stats::rnorm(n,
                                         sd = sE),
                            stats::rnorm(n,
                                         sd = sE))
               
               y.r <- mu + A.r + C.r + E.r
               
               
               r_ <- rep(r[i],n)
               
               data.r <- data.frame(A.r,C.r,E.r,y.r,r_)
               names(data.r) <- c("A1","A2","C1","C2","E1","E2","y1","y2","r")
               datalist[[i]] <- data.r
               names(datalist)[i] <- paste0("datar",r[i])
          }
          merged.data.frame <- Reduce(function(...) merge(..., all=T), datalist)
          merged.data.frame$id <- id
     }else{
          id=1:length(r_vector)
          data_vector=data.frame(id,r_vector)
          data_vector$A.r1<-as.numeric(NA)
          data_vector$A.r2<-as.numeric(NA)
          unique_r= matrix(unique(r_vector))
          for(i in 1:length(unique_r)){
               n <- length(r_vector[r_vector==unique_r[i]])
               A.rz <- sA*rmvn(n,
                               sigma=diag(2)+S2*unique_r[i])
               data_vector$A.r1[data_vector$r_vector==unique_r[i]] <- A.rz[,1]
               data_vector$A.r2[data_vector$r_vector==unique_r[i]] <- A.rz[,2]
          }
          n=length(r_vector)
          A.r <- matrix(c(data_vector$A.r1,
                          data_vector$A.r2),ncol=2)
          C.r <- stats::rnorm(n,sd=sC)
          C.r <- cbind(C.r,C.r)
          E.r <- cbind(stats::rnorm(n,
                                    sd = sE),
                       stats::rnorm(n,
                                    sd = sE))
          
          y.r <- mu + A.r + C.r + E.r
          
          data.r <- data.frame(id,A.r,C.r,E.r,y.r,r_vector)
          names(data.r) <- c("id","A1","A2","C1","C2","E1","E2","y1","y2","r")
          datalist[[i]] <- data.r
          names(datalist)[i]<- paste0("datar",r[i])
          
          merged.data.frame <- data.r
     }
     
     return(merged.data.frame)
}

### A wrapper to create different sample sizes for each group

diff_size <- function(Ngroup1, Ngroup2, rel=c(1,.5), ace=c(1,1,1), mu = 0){
        if(Ngroup1 == Ngroup2){
                df_final <- kinsim(r_all = rel,
                                   npg_all = Ngroup1,
                                   ace_all = ace,
                                   variable = 1,
                                   mu_all = mu)
        }
        if(Ngroup1 > Ngroup2){
                N1 <- Ngroup2
                N2 <- Ngroup1-Ngroup2
                df_core <- kinsim(r_all = rel,
                                  npg_all = N1,
                                  ace_all = ace,
                                  variable = 1,
                                  mu_all = mu)
                df_supp <- kinsim(r_all = rel,
                                  npg_all = N2,
                                  ace_all = ace,
                                  variable = 1,
                                  mu_all = mu)
                two_r <- sort(unique(df_supp[,17]),decreasing = TRUE)
                df_att <- df_supp[which(df_supp[,17]==two_r[1]),]
                df_final <- rbind(df_core, df_att)
        }
        if(Ngroup2 > Ngroup1){
                N1 <- Ngroup1
                N2 <- Ngroup2-Ngroup1
                df_core <- kinsim(r_all = rel,
                                  npg_all = N1,
                                  ace_all = ace,
                                  variable = 1,
                                  mu_all = mu)
                df_supp <- kinsim(r_all = rel,
                                  npg_all = N2,
                                  ace_all = ace,
                                  variable = 1,
                                  mu_all = mu)
                two_r <- sort(unique(df_supp[,17]),decreasing = TRUE)
                df_att <- df_supp[which(df_supp[,17]==two_r[2]),]
                df_final <- rbind(df_core, df_att)
        }
        return(df_final)
        
        
}


sep_kin <- function(data){
     two_r <- sort(unique(data[,17]),decreasing = TRUE)
     data_one <- data[which(data[,17]==two_r[1]),]
     data_two <- data[which(data[,17]==two_r[2]),]
     
     data_one <- data_one[,13:14]
     data_two <- data_two[,13:14]
     
     l_twodata <- list(data_one,data_two)
     return(l_twodata)
}



fit_siACE <- function(data_one, data_two, coe_am, elbound = FALSE){
     # Load Libraries & Options
     require(OpenMx)
     #require(psych)
     #require(polycor)
     # source("miFunctions.R")
     # # Create Output
     # filename <- "oneACEc"
     # sink(paste(filename,".Ro",sep=""), append=FALSE, split=TRUE)
     
     # ----------------------------------------------------------------------------------------------------------------------
     # PREPARE DATA
     # Load Data
     mzData    <- data_one
     dzData    <- data_two
     xxx <- mxMatrix(type = "Full", nrow = 1, ncol = 1,free = FALSE, values = coe_am, name = "coeAM")
     #coeAM <- coe_am
     
     # covMZ <- cov(mzData, use = "pairwise")
     # covDZ <- cov(dzData, use = "pairwise")
     # # 
     # mean(rbind(mzData,dzData)[,1], na.rm = TRUE)

     nv <- 1
     ntv <- 2
     selVars   <- c('y1_1','y1_2')
     
     #start values
     svBe <- .01
     svMu <- 0
     svVa <- .2
     svVe <- .5
     
     #variance matrix
     covA <- mxMatrix(type = "Symm", nrow = nv, ncol = nv,free = TRUE, values = svVa, label = "VA11", name = "VA")
     covC <- mxMatrix(type = "Symm", nrow = nv, ncol = nv,free = TRUE, values = svVa, label = "VC11", name = "VC")
     
     if(elbound == TRUE){
             covE <- mxMatrix(type = "Symm", nrow = nv, ncol = nv,free = TRUE, values = svVe, lbound = .001, label = "VE11", name = "VE")
     }else{
             covE <- mxMatrix(type = "Symm", nrow = nv, ncol = nv,free = TRUE, values = svVe, label = "VE11", name = "VE")
     }
     
     
     #expected variance matrix
     covP <- mxAlgebra(expression = VA+VC+VE, name = "V")
     covMZ <- mxAlgebra(expression = coeAM*VA+VC, name = "cMZ")
     covDZ <- mxAlgebra(expression = 0.5%x%VA+VC, name = "cDZ")
     expCovMz <- mxAlgebra(expression = rbind(cbind(V,cMZ), cbind(t(cMZ),V)), name = "expCovMz")
     expCovDz <- mxAlgebra(expression = rbind(cbind(V,cDZ), cbind(t(cDZ),V)), name = "expCovDz")
     
     #create data
     dataMZ       <- mxData( observed=mzData, type="raw" )
     dataDZ       <- mxData( observed=dzData, type="raw" )
     
     # Mean Matrix
     intercept <- mxMatrix(type = "Full", nrow= 1 , ncol = ntv, free = TRUE, values = 0, labels = "interC", name = "intercept")
     expMean <- mxAlgebra(expression = 1*intercept , name = "expMean")
     
     # Create expectation objects
     expMZ <- mxExpectationNormal(covariance = "expCovMz", means ="expMean", dimnames = selVars)
     expDZ <- mxExpectationNormal(covariance = "expCovDz", means ="expMean", dimnames = selVars)
     funML <- mxFitFunctionML()
     
     #Create models
     pars <- list(intercept, covA, covC, covE, covP)
     modelMZ <- mxModel(pars, expMean,covMZ,expCovMz,dataMZ,expMZ,funML,xxx,name = "MZ")
     #MZfit <- mxRun(modelMZ, intervals = TRUE)
     #summary(MZfit)
     modelDZ <- mxModel(pars, expMean,covDZ,expCovDz,dataDZ,expDZ,funML,name = "DZ")
     #DZfit <- mxRun(modelDZ, intervals = TRUE)
     #summary(DZfit)
     
     multi <- mxFitFunctionMultigroup(c("MZ","DZ"))
     
     
     #Algebra for Variance components
     rowUS <- rep("US",nv)
     colUS <- rep(c("VA","VC","VE","SA","SC","SE"),each = nv)
     estUS <- mxAlgebra(expression = cbind(VA,VC,VE,VA/V,VC/V,VE/V), name = "US", dimnames = list(rowUS,colUS))
     
     #CI
     ciACE <- mxCI("US[1,1:6]")
     modelACE <- mxModel("oneACEvc_1cov", pars, modelMZ, modelDZ, multi,estUS,ciACE)
     fitACE <- mxRun(modelACE, intervals = TRUE, silent = TRUE)
     sumACE <-summary(fitACE)
     #sumACE
     return(fitACE)
}




#### Functions to do the simulation:
#### Alert: The nmodel is the parameter to determine the steps from .50 to 1.00. Not the numbers of groups of models being run. 
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


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

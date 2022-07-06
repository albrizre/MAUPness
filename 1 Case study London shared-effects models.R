library(spatstat)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)
library(nimble)
library(ggplot2)
# Set your working directory
setwd("")
source("data_build_nimble.R")
source("code_1_covariate_London.R")

# Load data

load(file="Data/data_london.rda")

# Build dataset for nimble
data_aux=data_london
vars_names=c("over_70_prop","child_poverty_prop","Hypertension","Obesity","Diabetes","Asthma","insecure_proportion","proportion_at_risk_jobs","Expected")
data_nimble=data_build_nimble(data_aux,vars_names)

# Zones

datos=data_london
source("Generate zones.R")

# Create Results directory

if (!dir.exists("Results")){
  dir.create("Results")
}

# For each combination of zones and covariate in vars_names

for (i in 1:nrow(zones)){
  for (var_name in c("over_70_prop","child_poverty_prop","Hypertension","Obesity","Diabetes","Asthma","insecure_proportion","proportion_at_risk_jobs")){
    
    cat(paste0(as.character(zones[i,])," ",var_name,collapse = " "),"\n")

    if (!file.exists(paste0("Results/mcmc.output_",var_name,
                            "_",paste(as.character(zones[i,]),collapse = "_"),".rda"))){
      
      # Prepare data
      data <- list(y = c(datos$covid_19_deaths[datos$zone=="District"],
                         datos$covid_19_deaths[datos$zone=="MSOA"]))
      data_aux=data_nimble[datos$zone%in%as.character(zones[i,]),
                           which(colnames(data_nimble)%in%sort(as.vector(outer(c("0",c(var_name,"Expected")), 
                                                                               zones[i,], paste, sep="_"))))]
      # Move Expected columns
      which_expected=grep("Expected",colnames(data_aux))
      if (which_expected[1]==3){
        data_aux=data_aux[,c(1,2,5,6,3,4)]
      }
     
      constants <- list(
        x = data_aux,
        N1 = sum(data_aux[,1]==1),
        N = nrow(data_aux),
        V = length(vars_names))
      
      # Fit model
      
      inits <- function() list(beta01 = 0, beta02 = 0, 
                               beta1 = 0, 
                               delta1 = 1)
      mcmc.output <- nimbleMCMC(code_1_covariate_London, data = data, inits = inits, constants = constants,
                                monitors = c("beta01", "beta02", 
                                             "beta1", 
                                             "delta1",
                                             "lambda"), thin = 10,
                                niter = 120000, nburnin = 40000, nchains = 1, 
                                summary = TRUE, WAIC = TRUE)
      
      # Save model
      
      save(mcmc.output, file=paste0("Results/mcmc.output_",var_name,
                                    "_",paste(as.character(zones[i,]),collapse = "_"),".rda"))
    }
   
  }
}


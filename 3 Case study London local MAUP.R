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
source("delta_case_deletion_London.R")

# Load data

load("Data/data_london.rda")
load("Data/districts.rda")
load("Data/MSOAs.rda")

# Var names - var labels
vars_names=c("over_70_prop","child_poverty_prop","Hypertension","Obesity","Diabetes","Asthma",
             "insecure_proportion","proportion_at_risk_jobs")
var_labels=c("Aged 70+","Child poverty","Hypertension","Obesity","Diabetes","Asthma","Insecure job","At risk job")
names(var_labels)=c("over_70_prop","child_poverty_prop","Hypertension","Obesity","Diabetes","Asthma",
                    "insecure_proportion","proportion_at_risk_jobs")

# Zones

datos=data_london
source("Generate zones.R")

# Create Results/Influences directory

if (!dir.exists("Results/Influences")){
  dir.create("Results/Influences")
}

set.seed(1234)
for (i in 1:nrow(zones)){
  
  regions1=MSOAs
  regions2=districts

  for (var_name in c("over_70_prop","child_poverty_prop","Hypertension","Obesity","Diabetes","Asthma",
                     "insecure_proportion","proportion_at_risk_jobs")){
    
    data1=datos[datos$zone==zones[i,2],]
    data2=datos[datos$zone==zones[i,1],]
    
    # Load MCMC output
    
    load(paste0("Results/mcmc.output_",var_name,
                "_",paste(as.character(zones[i,]),collapse = "_"),".rda"))
  
    # Extract parameter estimates
    beta01=mcmc.output$summary[1,1]
    beta02=mcmc.output$summary[2,1]
    beta_all=mcmc.output$summary[3,1]
    delta_all=mcmc.output$summary[4,1]
    if (delta_all<1){
      delta_all_correct=1/delta_all
    } else{
      delta_all_correct=delta_all
    }
   
    # Define L
    L=round(0.8*nrow(mcmc.output$samples))
    
    # Calculate aux_correction_offset
    aux_correction_offset=aux_correction_offset_function(mcmc.output,data1,data2)
    
    # Individual influence on delta (single observation)
    if (!file.exists(paste0("Results/Influences/influence_deltas_single_",var_name,"_",paste(as.character(zones[i,]),collapse = "_"),".rda"))){
      influence_deltas_single=c()
      influence_deltas_single_p=c()
      for (e in 1:(length(regions2)+length(regions1))){
        print(e)
        aux=delta_case_deletion_single_obs(mcmc.output,L,data1,data2,regions1,regions2,e,aux_correction_offset)[[2]]
        influence_deltas_single=c(influence_deltas_single,aux["Mean"]) 
        influence_deltas_single_p=c(influence_deltas_single_p,aux["P(MAUPness_i<MAUPness)"]) 
      }
      save(influence_deltas_single,file = paste0("Results/Influences/influence_deltas_single_",var_name,"_",paste(as.character(zones[i,]),collapse = "_"),".rda"))
      save(influence_deltas_single_p,file = paste0("Results/Influences/influence_deltas_single_p_",var_name,"_",paste(as.character(zones[i,]),collapse = "_"),".rda"))
    } 
    if (length(influence_deltas_single<1)>0){
      influence_deltas_single[influence_deltas_single<1]=1/influence_deltas_single[influence_deltas_single<1]
    }
  }
}


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

# Covariates

vars_names=c("over_70_prop","child_poverty_prop","Hypertension","Obesity","Diabetes","Asthma",
             "insecure_proportion","proportion_at_risk_jobs")
var_labels=c("Aged 70+","Child poverty","Hypertension","Obesity","Diabetes","Asthma","Insecure job","At risk job")
names(var_labels)=c("over_70_prop","child_poverty_prop","Hypertension","Obesity","Diabetes","Asthma",
                    "insecure_proportion","proportion_at_risk_jobs")

# Zones

datos=data_london
source("Generate zones.R")

# Load MCMC outputs and save results

df_results=c()
for (i in 1:nrow(zones)){
  for (var_name in vars_names){
    
    print(paste0(i, " ",var_name))
    
    load(paste0("Results/mcmc.output_",var_name,
                "_",paste(as.character(zones[i,]),collapse = "_"),".rda"))
    
    # Index
    
    delta1=mcmc.output$summary[which(rownames(mcmc.output$summary)=="delta1"),2]
    delta1_lower=mcmc.output$summary[which(rownames(mcmc.output$summary)=="delta1"),4]
    delta1_upper=mcmc.output$summary[which(rownames(mcmc.output$summary)=="delta1"),5]
    MAUP_index=max(c(delta1,1/delta1))
    if (delta1>1){
      Lower=delta1_lower
      Upper=delta1_upper
    } else{
      Lower=1/delta1_upper
      Upper=1/delta1_lower
    }
    df_results=rbind(df_results,data.frame(MAUP_index=MAUP_index,Lower=Lower,Upper=Upper,covariate=var_labels[var_name],
                           AG1=paste0(zones[i,1]),AG2=paste0(zones[i,2])))
    
  }
}
save(df_results,file=paste0("Results/MAUpness_summary_intervals_df_results.rda"))


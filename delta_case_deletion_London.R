aux_correction_offset_function <- function(mcmc.output,data1,data2){
  lambdas=mcmc.output$samples[,grep("lambda",colnames(mcmc.output$samples))]
  ci=c()
  for (i in 1:length(c(data2$covid_19_deaths,data1$covid_19_deaths))){
    if (i<=nrow(data2)){
      ci=c(ci,sum(data2$covid_19_deaths[-i])/
             sum(data1$Expected[-i]))
    } else{
      ci=c(ci,sum(data1$covid_19_deaths[-(i-nrow(data2))])/
             sum(data1$Expected[-i]))
    }
  }
  lambdas_ci=as.numeric(t(sapply(1:ncol(lambdas),function(k) lambdas[,k]*ci[k])))
  lambdas=as.numeric(t(mcmc.output$samples[,grep("lambda",colnames(mcmc.output$samples))]))
 
  observed=matrix(rep(c(data2$covid_19_deaths,data1$covid_19_deaths),nrow(mcmc.output$samples)),nrow=nrow(mcmc.output$samples),byrow=T)
  observed=as.numeric(t(observed))
  density_values=matrix(dpois(observed,lambdas),nrow=nrow(mcmc.output$samples),byrow = T)
  density_values_ci=matrix(dpois(observed,lambdas_ci),nrow=nrow(mcmc.output$samples),byrow = T)
  
  output=list()
  output[["density_values"]]=density_values
  output[["density_values_ci"]]=density_values_ci
  output[["ratio"]]=matrix(density_values_ci/density_values,nrow=nrow(mcmc.output$samples))
  return(output)
}

delta_case_deletion_single_obs <- function(mcmc.output,L,data1,data2,regions1,regions2,e,aux_correction_offset){
  # Extract lambda and delta simulations
  lambdas=mcmc.output$samples[,grep("lambda",colnames(mcmc.output$samples))]
  deltas=mcmc.output$samples[,grep("delta",colnames(mcmc.output$samples))]
  delta_all=mcmc.output$summary[4,1]
  if (delta_all<1){
    delta_all=1/delta_all
  }
  if (e<=length(regions2)){
    observed_y=c(data2$covid_19_deaths[e])
  } else{
    observed_y=c(data1$covid_19_deaths[e-length(regions2)])
  }
  # Calculate importance weights
  # imp_weight=apply(lambdas,1,function(k) 1/prod(dpois(observed_y,k[e]))) # without offset correction
  imp_weight=1/apply(aux_correction_offset[["ratio"]],1,prod)
  imp_weight=imp_weight*(1/aux_correction_offset[["density_values_ci"]][,e])
  # Normalize (probabilities)
  probs=imp_weight/sum(imp_weight)
  # Alternative calculation if there are cancellation problems
  if (sum(is.na(probs))==0){
    # Sample from delta with probabilities proportional to imp_weight (probs)
    deltas_case_deletion=sample(deltas,L,replace = F,prob = probs)
    # Inverse values for p computation
    deltas_case_deletion_aux=deltas_case_deletion
    deltas_case_deletion_aux[deltas_case_deletion_aux<1]=1/deltas_case_deletion_aux[deltas_case_deletion_aux<1]
    # Create output
    output=list()
    output[[1]]=deltas_case_deletion
    output[[2]]=c(mean(deltas_case_deletion),
                  median(deltas_case_deletion),
                  quantile(deltas_case_deletion,0.025),
                  quantile(deltas_case_deletion,0.975),
                  ks.test(deltas,deltas_case_deletion)$p.value,
                  mean(deltas_case_deletion_aux < delta_all))
    names(output[[2]])=c("Mean","Median","95%CI_low","95%CI_upp","ks","P(MAUPness_i<MAUPness)")
  } else{
    # probs=1/apply(sapply(1:nrow(lambdas),function(k) apply(lambdas,1,function(s) prod(dpois(observed_y,c(lambdas[k,e],lambdas[k,d+length(regions2)]))/
    probs=1/sapply(1:nrow(lambdas),function(k) sum(apply(lambdas,1,function(s) prod(dpois(observed_y,c(lambdas[k,e],lambdas[k,d+length(regions2)]))/
                                                                                      dpois(observed_y,c(s[e],s[d+length(regions2)]))))))
    # Sample from delta with probabilities proportional to imp_weight (probs)
    deltas_case_deletion=sample(deltas,L,replace = T,prob = probs)
    # Inverse values for p computation
    deltas_case_deletion_aux=deltas_case_deletion
    deltas_case_deletion_aux[deltas_case_deletion_aux<1]=1/deltas_case_deletion_aux[deltas_case_deletion_aux<1]
    # Create output
    output=list()
    output[[1]]=deltas_case_deletion
    output[[2]]=c(mean(deltas_case_deletion),
                  median(deltas_case_deletion),
                  quantile(deltas_case_deletion,0.025),
                  quantile(deltas_case_deletion,0.975),
                  ks.test(deltas,deltas_case_deletion)$p.value,
                  mean(deltas_case_deletion_aux < delta_all))
    # mean(abs(deltas_case_deletion_aux-1) < abs(delta_all-1))
    names(output[[2]])=c("Mean","Median","95%CI_low","95%CI_upp","ks","P(MAUPness_i<MAUPness)")
  }
  return(output)
}

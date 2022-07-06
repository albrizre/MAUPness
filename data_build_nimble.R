data_build_nimble <- function(data,vars_names){
  
  # Sort vars_names
  vars_names=sort(vars_names)
  
  # Sort the data, just in case
  data=data[order(data$zone),]
  
  # Positions 
  positions=c(which(!duplicated(data$zone)),nrow(data)+1)
  
  # Create object to save the columns
  data_nimble=c()
  
  # Constant terms columns
  for (i in 1:length(unique(data$zone))){
    # Auxiliary vector
    aux=rep(0,nrow(data))
    aux[positions[i]:(positions[i+1]-1)]=1
    # Add column
    data_nimble=cbind(data_nimble,aux)
  }
  
  # Covariate terms columns
  for (var_name in vars_names){
    for (i in 1:length(unique(data$zone))){
      # Auxiliary vector
      aux=rep(0,nrow(data))
      aux[positions[i]:(positions[i+1]-1)]=data[which(data$zone==unique(data$zone)[i]),var_name]
      # Add column
      data_nimble=cbind(data_nimble,aux)
    }
  }
  
  # Columns names
  colnames(data_nimble)=sort(as.vector(outer(c("0",vars_names), unique(data$zone), paste, sep="_")))
 
  data_nimble=as.data.frame(data_nimble)
  return(data_nimble)
}

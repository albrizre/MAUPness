zones=expand.grid(unique(datos$zone),unique(datos$zone))
zones=zones[-which(zones$Var1==zones$Var2),]
zones=zones[-which(as.character(zones$Var1)>as.character(zones$Var2)),]
zones=zones[order(zones$Var1,zones$Var2),]
zones=as.matrix(zones)
rownames(zones)=NULL
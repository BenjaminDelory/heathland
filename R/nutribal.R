nutribal<-cmpfun(function(x, data=import_nutrients(), sqa=list(grazing=1, mowing=5, burning=5, choppering=7), 
                          nutrient="N", init=0){
  
  #x is a vector for a possible management scenario
  
  if (is.list(data)==FALSE){stop("data must be a list containing model parameters")}
  if ("nutrients" %in% class(data)){} else {stop("data must be a nutrients object")}
  if (is.list(sqa)==FALSE){stop("sqa must be a list")}
  if (nutrient=="N"|nutrient=="P"|nutrient=="K"|nutrient=="Ca"|nutrient=="Mg"){} else {stop("nutrient must be either N, P, K, Ca, or Mg")}
  if (is.numeric(init)==TRUE & length(init)==1 & init>=0) {} else {stop("init must be a single positive numeric value")}

  n<-length(x) #Number of simulated years
  results<-rep(NA, length.out=n+1) #Create vector to store results
  results[1]<-init
  
  meanvalues<-Reduce('+', data)/length(data)
  
  for (i in 1:n){
    
    #Store nutrient data for a particular management
    
    if (x[i]==0){
      datanutri1<-meanvalues[,nutrient]
      datanutri1[c("Esheep", "R", "Lfirst", "Dash")]<-0}
    if (x[i]==1){
      datanutri<-data$grazing[,nutrient]
      management<-"grazing"}
    if (x[i]==2){
      datanutri<-data$mowing[,nutrient]
      management<-"mowing"}
    if (x[i]==3){
      datanutri<-data$burning[,nutrient]
      management<-"burning"}
    if (x[i]==4){
      datanutri<-data$choppering[,nutrient]
      management<-"choppering"}
    
    #Calculate nutrient balance
    
    if (sum(x)==0){balance<-datanutri1["Datm"]-datanutri1["Lcontrol"]}
    
    else{
    
    if (x[i]>0) {
      t<-1
      balance<-(datanutri["Datm"]+datanutri["Esheep"])-(datanutri["R"]+datanutri["Lfirst"]-datanutri["Dash"])}
    
    if (x[i]==0){
      t<-t+1
      if (t<sqa[[management]]){L<-(t*(datanutri["Lcontrol"]-datanutri["Lfirst"])/(sqa[[management]]-1))+((sqa[[management]]*datanutri["Lfirst"]-datanutri["Lcontrol"])/(sqa[[management]]-1))} else {L<-datanutri["Lcontrol"]}
      balance<-(datanutri1["Datm"]+datanutri1["Esheep"])-(datanutri1["R"]+L-datanutri1["Dash"])}}
    
    results[i+1]<-balance}
  
  results<-cumsum(results) #Show cumulative nutrient balance
  
  return(results)})
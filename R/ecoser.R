ecoser<-cmpfun(function(x, data=import_services(), sqa=list(grazing=1, mowing=5, burning=5, choppering=7),
                        service="waterrecharge", init=0){
  
  #x is a vector for a possible management scenario
  
  if (is.list(data)==FALSE){stop("data must be a list containing model parameters")}
  if ("services" %in% class(data)){} else {stop("data must be a services object")}
  if (is.list(sqa)==FALSE){stop("sqa must be a list")}
  if (is.numeric(init)==TRUE & length(init)==1 & init>=0) {} else {stop("init must be a single positive numeric value")}
  if (service=="waterrecharge"|service=="carbon"|service=="cost"|service=="waterquality"|service=="appreciation"){} else {stop("service must be either waterrecharge, waterquality, carbon, cost, or appreciation")}
  
  #Check scenario
  index<-which(x>0)
  
  for (i in 1:index[length(index)-1]){
    
    if (x[i]==1 & sqa[["grazing"]]>1){
      if (identical(x[(i+1):(i+sqa[["grazing"]]-1)],rep(0, sqa[["grazing"]]-1))==FALSE){stop(paste("Invalid scenario (sqa for grazing = ", sqa[["grazing"]], "). Management allowed if status quo ante is reached.", sep=""))}}
    
    if (x[i]==2 & sqa[["mowing"]]>1){
      if (identical(x[(i+1):(i+sqa[["mowing"]]-1)],rep(0, sqa[["mowing"]]-1))==FALSE){stop(paste("Invalid scenario (sqa for mowing = ", sqa[["mowing"]], "). Management allowed if status quo ante is reached.", sep=""))}}
    
    if (x[i]==3 & sqa[["burning"]]>1){
      if (identical(x[(i+1):(i+sqa[["burning"]]-1)],rep(0, sqa[["burning"]]-1))==FALSE){stop(paste("Invalid scenario (sqa for burning = ", sqa[["burning"]], "). Management allowed if status quo ante is reached.", sep=""))}}
    
    if (x[i]==4 & sqa[["choppering"]]>1){
      if (identical(x[(i+1):(i+sqa[["choppering"]]-1)],rep(0, sqa[["choppering"]]-1))==FALSE){stop(paste("Invalid scenario (sqa for choppering = ", sqa[["choppering"]], "). Management allowed if status quo ante is reached.", sep=""))}}}
  
  #Compute ecosystem services
  n<-length(x) #Number of simulated years
  
  if (service=="waterrecharge"){
    
    results<-rep(NA, length.out=n+1) #Create vector to store results
    results[1]<-init
  
      for (i in 1:n){
        
        #Store waterrecharge recharge data for a particular management
        
        if (x[i]==0){dataser1<-mean(data$waterrecharge["control",])}
        if (x[i]==1){
          dataser<-rep(mean(data$waterrecharge["control",]), 2)
          management<-"grazing"}
        if (x[i]==2){
          dataser<-data$waterrecharge[,"mowing"]
          management<-"mowing"}
        if (x[i]==3){
          dataser<-data$waterrecharge[,"burning"]
          management<-"burning"}
        if (x[i]==4){
          dataser<-data$waterrecharge[,"choppering"]
          management<-"choppering"}
        
        #Calculate waterrecharge recharge
        
        if (sum(x)==0){recharge<-dataser1}
        
        else{
        
        if (x[i]>0) {
          t<-1
          recharge<-dataser[2]}
        
        if (x[i]==0){
          t<-t+1
          if (t<sqa[[management]]){recharge<-(t*(dataser[1]-dataser[2])/(sqa[[management]]-1))+((sqa[[management]]*dataser[2]-dataser[1])/(sqa[[management]]-1))} else {recharge<-dataser1}}}
        
        results[i+1]<-recharge}}
  
  if (service=="carbon"){
    
    results<-rep(NA, length.out=n)
    
    for (i in 1:n){
      if (x[i]==1){management<-"grazing"}
      if (x[i]==2){management<-"mowing"}
      if (x[i]==3){management<-"burning"}
      if (x[i]==4){management<-"choppering"}
      if (x[i]>0) {results[i]<-data$carbon[1,management]} else {results[i]<-0}}}
  
  if (service=="cost"){
    
    results<-rep(NA, length.out=n)
    
    for (i in 1:n){
      if (x[i]==1){management<-"grazing"}
      if (x[i]==2){management<-"mowing"}
      if (x[i]==3){management<-"burning"}
      if (x[i]==4){management<-"choppering"}
      if (x[i]>0) {results[i]<-data$costs[1,management]} else {results[i]<-0}}}
  
  if (service=="waterquality"){
    
    results<-rep(NA, length.out=n+1) #Create vector to store results
    results[1]<-init
    
    for (i in 1:n){
      
      #Store waterrecharge quality data for a particular management
      
      if (x[i]==0){dataser1<-mean(data$waterquality["Lcontrol",])}
      if (x[i]==1){
        dataser<-data$waterquality[,"grazing"]
        management<-"grazing"}
      if (x[i]==2){
        dataser<-data$waterquality[,"mowing"]
        management<-"mowing"}
      if (x[i]==3){
        dataser<-data$waterquality[,"burning"]
        management<-"burning"}
      if (x[i]==4){
        dataser<-data$waterquality[,"choppering"]
        management<-"choppering"}
      
      #Calculate waterrecharge quality
      
      if (sum(x)==0){quality<-dataser1}
      
      else{
        
        if (x[i]>0) {
          t<-1
          quality<-dataser[2]}
        
        if (x[i]==0){
          t<-t+1
          if (t<sqa[[management]]){quality<-(t*(dataser[1]-dataser[2])/(sqa[[management]]-1))+((sqa[[management]]*dataser[2]-dataser[1])/(sqa[[management]]-1))} else {quality<-dataser1}}}
      
      results[i+1]<-quality}}
  
  if (service=="appreciation"){
    
    results<-rep(NA, length.out=n)
    
    for (i in 1:n){
      
      if (sum(x)==0){appreciation<-2}
      
      else{
        
        if (x[i]>0){
          if (x[i]==1){management<-"grazing"}
          if (x[i]==2){management<-"mowing"}
          if (x[i]==3){management<-"burning"}
          if (x[i]==4){management<-"choppering"}
          t<-1
          appreciation<-data$appreciation[t,management]}
        
        if (x[i]==0){
          t<-t+1
          if (t<=5){appreciation<-data$appreciation[t,management]} else {appreciation<-2}}}
      
      results[i]<-appreciation}}
  
  results<-cumsum(results) #Show cumulative results
  
  return(results)})
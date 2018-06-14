nutribal<-function(data, management, freq=NULL, time=NULL, n){
  
  #Error interceptions
  if (is.list(data)==FALSE){stop("data must be a list containing model parameters")}
  if ("nutrients" %in% class(data)){} else {stop("data must be a nutrients object")}
  if (is.character(management)==FALSE){stop("management must be character")}
  if (management=="grazing"|management=="mowing"|management=="burning"|management=="choppering"|management=="sodcutting"|management=="none"){} else {stop("Unknown management")}
  if (is.null(freq)==FALSE){if (is.numeric(freq)==TRUE & length(freq)==1){} else {stop("freq must be a single numeric value")}}
  if (is.null(time)==FALSE){if (is.numeric(time)==TRUE & length(time)==1){} else {stop("time must be a single numeric value")}}
  if (is.numeric(n)==TRUE & length(n)==1){} else {stop("n must be a single numeric value")}
  
  if (management=="mowing"|management=="burning"|management=="choppering"|management=="sodcutting") {
    if (is.null(freq)==TRUE){stop("management frequency must be provided")}
    if (is.null(time)==TRUE){stop("time needed to reach status quo ante after management must be provided")}}
  
  if (management=="none"){
    
    .data<-Reduce('+', data)/length(data) #Calculate mean values of Datm and Lcontrol for no management scenario
    
    #Calculate balance for all the years and all chemical elements
    
    balance<-.data["Datm",]-.data["Lcontrol",]
    balance.allyears<-matrix(balance, ncol=5, nrow=n, byrow=T)
    results<-apply(balance.allyears, 2, cumsum)
    colnames(results)<-c("N", "P", "K", "Ca", "Mg")
    results<-data.frame(year=c(1:n), as.data.frame(results))}
  
  if (management=="grazing"){
    
    #Calculate Lincrease for the first year
    #By default, time=1, freq=1, and Lincrease=0 for grazing
    Lincrease<-0
    time<-freq<-1
    
    #Calculate balance for all the years and all chemical elements
    
    balance<-(data$grazing["Datm",]+data$grazing["Esheep",]-data$grazing["Lcontrol",])-(data$grazing["R",]+Lincrease-data$grazing["Dash",])
    balance.allyears<-matrix(balance, ncol=5, nrow=n, byrow=T)
    results<-apply(balance.allyears, 2, cumsum)
    colnames(results)<-c("N", "P", "K", "Ca", "Mg")
    results<-data.frame(year=c(1:n), as.data.frame(results))}
  
  
  if (management=="mowing"){
    
    if (freq<time){stop("management frequency is lower than the time needed to reach status quo ante after management")}
    
    yearsaftermanagement<-seq(from=0, to=n, by=freq)+1 #Find all first years after management
    if (length(which(yearsaftermanagement>n))>0) {yearsaftermanagement<-yearsaftermanagement[-which(yearsaftermanagement>n)]}
    
    Lincrease<-matrix(ncol=5, nrow=n) #Store Lincrease for each year
    R<-matrix(ncol=5, nrow=n) #Store R for each year
    
    Lincrease.first<-data$mowing["Lfirst",]-data$mowing["Lcontrol",] #Increase in leaching one year after management
    
    Ltot<-matrix(ncol=5, nrow=time) #Total leaching until status quo ante
    for (i in 1:time) {Ltot[i,]<--(Lincrease.first/(time-1))*i+((time*data$mowing["Lfirst",]-data$mowing["Lcontrol",])/(time-1))}
    
    Lincrease.untilstatusquoante<-Ltot-matrix(data$mowing["Lcontrol",], ncol=5, nrow=time, byrow=T) #Lincrease until status quo ante
    
    t<-0 #Initialisation
    
    for (i in 1:n){
    
      if (i==1){
        t<-t+1
        Lincrease[i,]<-Lincrease.untilstatusquoante[t,]
        R[i,]<-data$mowing["R",]}
        
      else {
        
        if (i %in% yearsaftermanagement) {
          t<-1
          Lincrease[i,]<-Lincrease.untilstatusquoante[t,]
          R[i,]<-data$mowing["R",]}
        
        else {
          t<-t+1
          if (t<=nrow(Lincrease.untilstatusquoante)) {Lincrease[i,]<-Lincrease.untilstatusquoante[t,]}
          if (t>nrow(Lincrease.untilstatusquoante)) {Lincrease[i,]<-c(0,0,0,0,0)}
          R[i,]<-c(0,0,0,0,0)}}}
    
    balance.allyears<-(matrix(data$mowing["Datm",], ncol=5, nrow=n, byrow=T)+matrix(data$mowing["Esheep",], ncol=5, nrow=n, byrow=T)-matrix(data$mowing["Lcontrol",], ncol=5, nrow=n, byrow=T))-(R+Lincrease-matrix(data$mowing["Dash",], ncol=5, nrow=n, byrow=T))
    results<-apply(balance.allyears, 2, cumsum)
    colnames(results)<-c("N", "P", "K", "Ca", "Mg")
    results<-data.frame(year=c(1:n), as.data.frame(results))}
  
  
  if (management=="burning"){
    
    if (freq<time){stop("management frequency is lower than the time needed to reach status quo ante after management")}
    
    yearsaftermanagement<-seq(from=0, to=n, by=freq)+1 #Find all first years after management
    if (length(which(yearsaftermanagement>n))>0) {yearsaftermanagement<-yearsaftermanagement[-which(yearsaftermanagement>n)]}
    
    Lincrease<-matrix(ncol=5, nrow=n) #Store Lincrease for each year
    R<-matrix(ncol=5, nrow=n) #Store R for each year
    Dash<-matrix(ncol=5, nrow=n) #Store Dash for each year
    
    Lincrease.first<-data$burning["Lfirst",]-data$burning["Lcontrol",] #Increase in leaching one year after management
    
    Ltot<-matrix(ncol=5, nrow=time) #Total leaching until status quo ante
    for (i in 1:time) {Ltot[i,]<--(Lincrease.first/(time-1))*i+((time*data$burning["Lfirst",]-data$burning["Lcontrol",])/(time-1))}
    
    Lincrease.untilstatusquoante<-Ltot-matrix(data$burning["Lcontrol",], ncol=5, nrow=time, byrow=T) #Lincrease until status quo ante
    
    t<-0 #Initialisation
    
    for (i in 1:n){
      
      if (i==1){
        t<-t+1
        Lincrease[i,]<-Lincrease.untilstatusquoante[t,]
        R[i,]<-data$burning["R",]
        Dash[i,]<-data$burning["Dash",]}
      
      else {
        
        if (i %in% yearsaftermanagement) {
          t<-1
          Lincrease[i,]<-Lincrease.untilstatusquoante[t,]
          R[i,]<-data$burning["R",]
          Dash[i,]<-data$burning["Dash",]}
        
        else {
          t<-t+1
          if (t<=nrow(Lincrease.untilstatusquoante)) {Lincrease[i,]<-Lincrease.untilstatusquoante[t,]}
          if (t>nrow(Lincrease.untilstatusquoante)) {Lincrease[i,]<-c(0,0,0,0,0)}
          R[i,]<-c(0,0,0,0,0)
          Dash[i,]<-c(0,0,0,0,0)}}}
    
    balance.allyears<-(matrix(data$burning["Datm",], ncol=5, nrow=n, byrow=T)+matrix(data$burning["Esheep",], ncol=5, nrow=n, byrow=T)-matrix(data$burning["Lcontrol",], ncol=5, nrow=n, byrow=T))-(R+Lincrease-Dash)
    results<-apply(balance.allyears, 2, cumsum)
    colnames(results)<-c("N", "P", "K", "Ca", "Mg")
    results<-data.frame(year=c(1:n), as.data.frame(results))}
  

  if (management=="choppering"){
    
    if (freq<time){stop("management frequency is lower than the time needed to reach status quo ante after management")}
    
    yearsaftermanagement<-seq(from=0, to=n, by=freq)+1 #Find all first years after management
    if (length(which(yearsaftermanagement>n))>0) {yearsaftermanagement<-yearsaftermanagement[-which(yearsaftermanagement>n)]}
    
    Lincrease<-matrix(ncol=5, nrow=n) #Store Lincrease for each year
    R<-matrix(ncol=5, nrow=n) #Store R for each year
    
    Lincrease.first<-data$choppering["Lfirst",]-data$choppering["Lcontrol",] #Increase in leaching one year after management
    
    Ltot<-matrix(ncol=5, nrow=time) #Total leaching until status quo ante
    for (i in 1:time) {Ltot[i,]<--(Lincrease.first/(time-1))*i+((time*data$choppering["Lfirst",]-data$choppering["Lcontrol",])/(time-1))}
    
    Lincrease.untilstatusquoante<-Ltot-matrix(data$choppering["Lcontrol",], ncol=5, nrow=time, byrow=T) #Lincrease until status quo ante
    
    t<-0 #Initialisation
    
    for (i in 1:n){
      
      if (i==1){
        t<-t+1
        Lincrease[i,]<-Lincrease.untilstatusquoante[t,]
        R[i,]<-data$choppering["R",]}
      
      else {
        
        if (i %in% yearsaftermanagement) {
          t<-1
          Lincrease[i,]<-Lincrease.untilstatusquoante[t,]
          R[i,]<-data$choppering["R",]}
        
        else {
          t<-t+1
          if (t<=nrow(Lincrease.untilstatusquoante)) {Lincrease[i,]<-Lincrease.untilstatusquoante[t,]}
          if (t>nrow(Lincrease.untilstatusquoante)) {Lincrease[i,]<-c(0,0,0,0,0)}
          R[i,]<-c(0,0,0,0,0)}}}
    
    balance.allyears<-(matrix(data$choppering["Datm",], ncol=5, nrow=n, byrow=T)+matrix(data$choppering["Esheep",], ncol=5, nrow=n, byrow=T)-matrix(data$choppering["Lcontrol",], ncol=5, nrow=n, byrow=T))-(R+Lincrease-matrix(data$choppering["Dash",], ncol=5, nrow=n, byrow=T))
    results<-apply(balance.allyears, 2, cumsum)
    colnames(results)<-c("N", "P", "K", "Ca", "Mg")
    results<-data.frame(year=c(1:n), as.data.frame(results))}
  
  
  if (management=="sodcutting"){
    
    if (freq<time){stop("management frequency is lower than the time needed to reach status quo ante after management")}
    
    yearsaftermanagement<-seq(from=0, to=n, by=freq)+1 #Find all first years after management
    if (length(which(yearsaftermanagement>n))>0) {yearsaftermanagement<-yearsaftermanagement[-which(yearsaftermanagement>n)]}
    
    Lincrease<-matrix(ncol=5, nrow=n) #Store Lincrease for each year
    R<-matrix(ncol=5, nrow=n) #Store R for each year
    
    Lincrease.first<-data$sodcutting["Lfirst",]-data$sodcutting["Lcontrol",] #Increase in leaching one year after management
    
    Ltot<-matrix(ncol=5, nrow=time) #Total leaching until status quo ante
    for (i in 1:time) {Ltot[i,]<--(Lincrease.first/(time-1))*i+((time*data$sodcutting["Lfirst",]-data$sodcutting["Lcontrol",])/(time-1))}
    
    Lincrease.untilstatusquoante<-Ltot-matrix(data$sodcutting["Lcontrol",], ncol=5, nrow=time, byrow=T) #Lincrease until status quo ante
    
    t<-0 #Initialisation
    
    for (i in 1:n){
      
      if (i==1){
        t<-t+1
        Lincrease[i,]<-Lincrease.untilstatusquoante[t,]
        R[i,]<-data$sodcutting["R",]}
      
      else {
        
        if (i %in% yearsaftermanagement) {
          t<-1
          Lincrease[i,]<-Lincrease.untilstatusquoante[t,]
          R[i,]<-data$sodcutting["R",]}
        
        else {
          t<-t+1
          if (t<=nrow(Lincrease.untilstatusquoante)) {Lincrease[i,]<-Lincrease.untilstatusquoante[t,]}
          if (t>nrow(Lincrease.untilstatusquoante)) {Lincrease[i,]<-c(0,0,0,0,0)}
          R[i,]<-c(0,0,0,0,0)}}}
    
    balance.allyears<-(matrix(data$sodcutting["Datm",], ncol=5, nrow=n, byrow=T)+matrix(data$sodcutting["Esheep",], ncol=5, nrow=n, byrow=T)-matrix(data$sodcutting["Lcontrol",], ncol=5, nrow=n, byrow=T))-(R+Lincrease-matrix(data$sodcutting["Dash",], ncol=5, nrow=n, byrow=T))
    results<-apply(balance.allyears, 2, cumsum)
    colnames(results)<-c("N", "P", "K", "Ca", "Mg")
    results<-data.frame(year=c(1:n), as.data.frame(results))}
  
  return(results)}
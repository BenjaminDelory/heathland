ecoser<-function(data, management, freq=NULL, time=NULL, n){
  
  #Error interceptions
  if (is.list(data)==FALSE){stop("data must be a list containing model parameters")}
  if ("services" %in% class(data)){} else {stop("data must be a services object")}
  if (is.character(management)==FALSE){stop("management must be character")}
  if (management=="grazing"|management=="mowing"|management=="burning"|management=="choppering"|management=="sodcutting"|management=="none"){} else {stop("Unknown management")}
  if (is.null(freq)==FALSE){if (is.numeric(freq)==TRUE & length(freq)==1){} else {stop("freq must be a single numeric value")}}
  if (is.null(time)==FALSE){if (is.numeric(time)==TRUE & length(time)==1){} else {stop("time must be a single numeric value")}}
  if (is.numeric(n)==TRUE & length(n)==1){} else {stop("n must be a single numeric value")}
  
  if (management=="mowing"|management=="burning"|management=="choppering"|management=="sodcutting") {
    if (is.null(freq)==TRUE){stop("management frequency must be provided")}
    if (is.null(time)==TRUE){stop("time needed to reach status quo ante after management must be provided")}}
  
  if (management=="grazing"){
    #We assume that there is no difference between control and managed plots
    results<-as.matrix(data.frame(year=c(1:n), groundwater=rep(0, n)))}
  
  if (management!="grazing"){
    
    if (freq<time){stop("management frequency is lower than the time needed to reach status quo ante after management")}
  
    yearsaftermanagement<-seq(from=0, to=n, by=freq)+1 #Find all first years after management
    if (length(which(yearsaftermanagement>n))>0) {yearsaftermanagement<-yearsaftermanagement[-which(yearsaftermanagement>n)]}
  
    L<-matrix(ncol=1, nrow=n)} #Store groundwater recharge for each year
  
  if (management=="mowing"){

    Ltot<-matrix(ncol=1, nrow=time) #Total leaching until status quo ante
    for (i in 1:time) {Ltot[i,]<--((data$groundwater["managed","mowing"]-data$groundwater["control","mowing"])/(time-1))*i+((time*data$groundwater["managed","mowing"]-data$groundwater["control","mowing"])/(time-1))}
    
    t<-0 #Initialisation
    
    for (i in 1:n){
    
      if (i==1){
        t<-t+1
        L[i,]<-Ltot[t,]}
        
      else {
        
        if (i %in% yearsaftermanagement) {
          t<-1
          L[i,]<-Ltot[t,]}
        
        else {
          t<-t+1
          if (t<=time) {L[i,]<-Ltot[t,]}
          if (t>time) {L[i,]<-data$groundwater["control","mowing"]}}}}
    
    results.control<-cumsum(matrix(data$groundwater["control", "mowing"], ncol=1, nrow=n))
    results.managed<-cumsum(L)
    results<-results.managed-results.control
    results<-data.frame(year=c(1:n), groundwater=results)}
  
  if (management=="burning"){
    
    Ltot<-matrix(ncol=1, nrow=time) #Total leaching until status quo ante
    for (i in 1:time) {Ltot[i,]<--((data$groundwater["managed","burning"]-data$groundwater["control","burning"])/(time-1))*i+((time*data$groundwater["managed","burning"]-data$groundwater["control","burning"])/(time-1))}
    
    t<-0 #Initialisation
    
    for (i in 1:n){
      
      if (i==1){
        t<-t+1
        L[i,]<-Ltot[t,]}
      
      else {
        
        if (i %in% yearsaftermanagement) {
          t<-1
          L[i,]<-Ltot[t,]}
        
        else {
          t<-t+1
          if (t<=time) {L[i,]<-Ltot[t,]}
          if (t>time) {L[i,]<-data$groundwater["control","burning"]}}}}
    
    results.control<-cumsum(matrix(data$groundwater["control", "burning"], ncol=1, nrow=n))
    results.managed<-cumsum(L)
    results<-results.managed-results.control
    results<-data.frame(year=c(1:n), groundwater=results)}
  
  if (management=="choppering"){
    
    Ltot<-matrix(ncol=1, nrow=time) #Total leaching until status quo ante
    for (i in 1:time) {Ltot[i,]<--((data$groundwater["managed","choppering"]-data$groundwater["control","choppering"])/(time-1))*i+((time*data$groundwater["managed","choppering"]-data$groundwater["control","choppering"])/(time-1))}
    
    t<-0 #Initialisation
    
    for (i in 1:n){
      
      if (i==1){
        t<-t+1
        L[i,]<-Ltot[t,]}
      
      else {
        
        if (i %in% yearsaftermanagement) {
          t<-1
          L[i,]<-Ltot[t,]}
        
        else {
          t<-t+1
          if (t<=time) {L[i,]<-Ltot[t,]}
          if (t>time) {L[i,]<-data$groundwater["control","choppering"]}}}}
    
    results.control<-cumsum(matrix(data$groundwater["control", "choppering"], ncol=1, nrow=n))
    results.managed<-cumsum(L)
    results<-results.managed-results.control
    results<-data.frame(year=c(1:n), groundwater=results)}
  
  if (management=="sodcutting"){
    
    Ltot<-matrix(ncol=1, nrow=time) #Total leaching until status quo ante
    for (i in 1:time) {Ltot[i,]<--((data$groundwater["managed","sodcutting"]-data$groundwater["control","sodcutting"])/(time-1))*i+((time*data$groundwater["managed","sodcutting"]-data$groundwater["control","sodcutting"])/(time-1))}
    
    t<-0 #Initialisation
    
    for (i in 1:n){
      
      if (i==1){
        t<-t+1
        L[i,]<-Ltot[t,]}
      
      else {
        
        if (i %in% yearsaftermanagement) {
          t<-1
          L[i,]<-Ltot[t,]}
        
        else {
          t<-t+1
          if (t<=time) {L[i,]<-Ltot[t,]}
          if (t>time) {L[i,]<-data$groundwater["control","sodcutting"]}}}}
    
    results.control<-cumsum(matrix(data$groundwater["control", "sodcutting"], ncol=1, nrow=n))
    results.managed<-cumsum(L)
    results<-results.managed-results.control
    results<-data.frame(year=c(1:n), groundwater=results)}
  
  return(results)}
nutribal<-cmpfun(function(x, data=import_nutrients(), sqa=list(grazing=1, mowing=5, burning=5, choppering=7), 
                          bioleft=list(mowing=5133.63, burning=1888.96), nutrient="N", Datm=NULL, 
                          gompertz=list(a=20148.38, b=8.48362, c=6.647557), sheep.pressure=0.5,
                          plot.biomass=FALSE, cumulative=TRUE, ...){
  
  #x is a vector for a possible management scenario
  
  if (is.numeric(x)==FALSE){stop("x must be a vector of positive numeric values")}
  if (is.list(data)==FALSE){stop("data must be a list containing model parameters")}
  if ("nutrients" %in% class(data)){} else {stop("data must be a nutrients object")}
  if (is.list(sqa)==FALSE){stop("sqa must be a list")}
  if (nutrient=="N"|nutrient=="P"|nutrient=="K"|nutrient=="Ca"|nutrient=="Mg"){} else {stop("nutrient must be either N, P, K, Ca, or Mg")}
  if (is.null(Datm)==FALSE){if (is.numeric(Datm)==TRUE & length(Datm)==1 & Datm>=0) {} else {stop("Datm must be a single positive numeric value")}}
  if (is.list(gompertz)==FALSE){stop("gompertz must be a list containing model parameters")}
  if (is.list(bioleft)==FALSE){stop("bioleft must be a list containing model parameters")}
  if (is.numeric(sheep.pressure)==FALSE|sheep.pressure<0|sheep.pressure>1){stop("sheep.pressure must be a numeric value comprised between 0 and 1")}
  if (is.logical(plot.biomass)==FALSE){stop("plot.biomass must be a logical value")}
  if (is.logical(cumulative)==FALSE){stop("cumulative must be a logical value")}
  if (x[1]!=4){stop("A scenario must start with choppering")}
  
  #Set model parameters
  path<-system.file("extdata", package="heathland")
  nutrientcontent<-as.data.frame(read.table(paste(path, "nutrientcontent.txt", sep="/"), header=TRUE))
  
  if (nutrient=="N"){model<-lm(N/100~Year, nutrientcontent)} #Fit linear model
  if (nutrient=="P"){model<-lm(P/100~Year, nutrientcontent)} #Fit linear model
  if (nutrient=="K"){model<-lm(K/100~Year, nutrientcontent)} #Fit linear model
  if (nutrient=="Ca"){model<-lm(Ca/100~Year, nutrientcontent)} #Fit linear model
  if (nutrient=="Mg"){model<-lm(Mg/100~Year, nutrientcontent)} #Fit linear model
  
  a.nutri<-coef(model)[2] #Slope
  b.nutri<-coef(model)[1] #Intercept
  
  #Check scenario
  index<-which(x>0)
  
  if (length(index)>1){
  
  for (i in 1:index[length(index)-1]){
    
    if (x[i]==1 & sqa[["grazing"]]>1){
      if (identical(x[(i+1):(i+sqa[["grazing"]]-1)],rep(0, sqa[["grazing"]]-1))==FALSE){stop(paste("Invalid scenario (sqa for grazing = ", sqa[["grazing"]], "). Management allowed if status quo ante is reached.", sep=""))}}
    
    if (x[i]==2 & sqa[["mowing"]]>1){
      if (identical(x[(i+1):(i+sqa[["mowing"]]-1)],rep(0, sqa[["mowing"]]-1))==FALSE){stop(paste("Invalid scenario (sqa for mowing = ", sqa[["mowing"]], "). Management allowed if status quo ante is reached.", sep=""))}}
    
    if (x[i]==3 & sqa[["burning"]]>1){
      if (identical(x[(i+1):(i+sqa[["burning"]]-1)],rep(0, sqa[["burning"]]-1))==FALSE){stop(paste("Invalid scenario (sqa for burning = ", sqa[["burning"]], "). Management allowed if status quo ante is reached.", sep=""))}}
    
    if (x[i]==4 & sqa[["choppering"]]>1){
      if (2 %in% x[(i+1):(i+sqa[["choppering"]]-1)]) {stop("Mowing is not allowed after choppering if status quo ante is not reached.")}
      if (3 %in% x[(i+1):(i+sqa[["choppering"]]-1)]) {stop("Burning is not allowed after choppering if status quo ante is not reached.")}}}}
  
  #Compute nutrient balance
  n<-length(x) #Number of simulated years
  results<-rep(NA, length.out=n+1) #Create vector to store nutrient balance results
  results[1]<-0 #Initiialisation value

  results.biomass<-rep(NA, length.out=n) #Create vector to store biomass value of the stand
  results.biomass[1]<-0 #We always start with heavy restoration management (choppering)

  meanvalues<-Reduce('+', data)/length(data) #Calculate mean value of model parameters
  datanutri1<-meanvalues[,nutrient] #Store mean value of model parameters for the target nutrient
  
  if (is.null(Datm)==TRUE){Datm<-meanvalues["Datm", nutrient]} #Adjust Datm
  
  for (i in 1:n){
    
    #Store nutrient data for a particular management
    
    if (x[i]==1){
      age<-age+1
      datanutri<-data$grazing[,nutrient]}
    if (x[i]==2){
      age<-age+1
      datanutri<-data$mowing[,nutrient]}
    if (x[i]==3){
      age<-age+1
      datanutri<-data$burning[,nutrient]}
    if (x[i]==4){ #We start all scenarios with a choppering
      datanutri<-data$choppering[,nutrient]
      management<-"choppering"
      age<-0}
    
    if (x[i]==0){age<-age+1}
    
    #Calculate biomass of the stand before management
    
    if (i==2){results.biomass[i]<-gompertz$a*exp(-exp(-((age-gompertz$b)/gompertz$c)))}
    
    if (i>2){results.biomass[i]<-results.biomass[i-1]+((1/gompertz$c)*results.biomass[i-1]*log(gompertz$a/results.biomass[i-1]))}
  
    #Calculate nutrient balance model parameters
    
    if (x[i]==1){
      
      management<-"grazing"
      
      if (i<=10){
        bioexport<-(2127/10)*i*sheep.pressure
        R<-(datanutri["R"]/10)*i*sheep.pressure}
      else {
        bioexport<-2127*sheep.pressure
        R<-datanutri["R"]*sheep.pressure}
      
      results.biomass[i]<-results.biomass[i]-bioexport #biomass left after grazing
      if (results.biomass[i]<0){
        results.biomass<-rep(NA, length.out=n)
        results<-rep(NA, length.out=n)
        warning("Negative plot biomass values have been reached. NA is returned.")
        break}

      Esheep<-datanutri["Esheep"]*sheep.pressure
      Dash<-0}
    
    if (x[i]==2){
      
      management<-"mowing"
      if ((results.biomass[i]-bioleft$mowing)<0){
        results.biomass<-rep(NA, length.out=n)
        results<-rep(NA, length.out=n)
        warning("Biomass left after mowing is greater than biomass before management. NA is returned.")
        break}
      R<-(results.biomass[i]-bioleft$mowing)*(a.nutri*age+b.nutri)
      results.biomass[i]<-bioleft$mowing
      Esheep<-0
      Dash<-0
      age<-0}
    
    if (x[i]==3){
      
      management<-"burning"
      if ((results.biomass[i]-bioleft$burning)<0){
        results.biomass<-rep(NA, length.out=n)
        results<-rep(NA, length.out=n)
        warning("Biomass left after burning is greater than biomass before management. NA is returned.")
        break}
      R<-(results.biomass[i]-bioleft$burning)*(a.nutri*age+b.nutri)
      Dash<-(datanutri["Dash"]/(11806*(a.nutri*10+b.nutri)))*(results.biomass[i]*(a.nutri*age+b.nutri))
      results.biomass[i]<-bioleft$burning
      Esheep<-0
      age<-0}
    
    if (x[i]==4){
      R<-datanutri["R"]
      Esheep<-0
      Dash<-0}
    
    #Calculate nutrient balance
    if (sum(x)==0){balance<-Datm-datanutri1["Lcontrol"]}
    
    else{
    
    if (x[i]>0) {
      t<-1
      balance<-(Datm+Esheep)-(R+datanutri["Lfirst"]-Dash)}
    
    if (x[i]==0){
      t<-t+1
      if (t<sqa[[management]]){L<-(t*(datanutri["Lcontrol"]-datanutri["Lfirst"])/(sqa[[management]]-1))+((sqa[[management]]*datanutri["Lfirst"]-datanutri["Lcontrol"])/(sqa[[management]]-1))} else {L<-datanutri1["Lcontrol"]}
      balance<-Datm-L}}
  
    results[i+1]<-balance}
  
  if (NA %in% results) {} else {results<-c(0, results[3:length(results)])}

  if (cumulative==TRUE) {results<-cumsum(results)} #Show cumulative nutrient balance
  
  if (plot.biomass==TRUE){plot(c(0:(n-1)), results.biomass, type="l", ylab="Aboveground biomass (kg/ha)", xlab="Time (years)", las=1,...)}
  
  return(results)})
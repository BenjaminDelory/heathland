freq_sc<-function(x){
  freq0<-sum(x==0)
  freq1<-sum(x==1)
  freq2<-sum(x==2)
  freq3<-sum(x==3)
  freq4<-sum(x==4)
  result<-c(freq0, freq1, freq2, freq3, freq4)
  return(result)}

is.whole<-function(x){is.numeric(x) && floor(x)==x}

resample <- function(x, ...) {x[sample.int(length(x), ...)]}

create.scenario<-function(x, constraints){

      scenario<-rep(0, sum(x))
      
      if (x[5]>0){ #if there is choppering
      
          pos.choppering<-seq(from=0, by=constraints["choppering","mowing"]+constraints["mowing","choppering"]+constraints["mowing","mowing"], length.out=x[5])+1
          scenario[pos.choppering]<-4
          
          if (x[3]+x[4]!=0){
            
              pos.mow.burn1<-pos.choppering-constraints["mowing", "choppering"]
              pos.mow.burn1<-pos.mow.burn1[pos.mow.burn1>0]
              pos.mow.burn2<-pos.choppering+constraints["choppering", "mowing"]
              pos.mow.burn2<-pos.mow.burn2[pos.mow.burn2<sum(x)]
              pos.mow.burn.rest<-seq(max(pos.mow.burn2)+constraints["mowing","mowing"], to=sum(x), by=constraints["mowing","mowing"])
              pos.mow.burn<-sort(c(pos.mow.burn1, pos.mow.burn2, pos.mow.burn.rest))
              if (x[3]+x[4]<=length(pos.mow.burn)){
                index<-pos.mow.burn[1:(x[3]+x[4])]
                if (x[3]+x[4]>1) {scenario[index]<-sample(c(rep(2, x[3]), rep(3, x[4])))}
                if (x[3]+x[4]==1) {scenario[index]<-c(rep(2, x[3]), rep(3, x[4]))}} 
              else {scenario<-rep(NA, sum(x))}}
              
          if (FALSE %in% is.na(scenario)){
          
              t<-x[2] #Number of grazing
              for (i in 1:length(scenario)){
                
                if (scenario[i]!=0){
                  pos.m<-i
                  m<-scenario[i]}
                
                if (scenario[i]==0 & i-pos.m>=constraints[m+1,"grazing"]){
                  scenario[i]<-1
                  t<-t-1}
                
                if (t==0){break}}}}
      
      else{ #if there is no choppering
        
        sc1<-c(rep(1, x[2]),rep(2, x[3]),rep(3, x[4]),rep(4, x[5]))
        l0<-length(sc1)
        if (l0!=0){
          scenario<-c()
          for (i in 1:l0){
            m<-resample(sc1, 1)
            sc1<-sc1[-which(sc1==m)[1]]
            if (i==1) {scenario<-c(scenario,m)} else {scenario<-c(scenario, c(rep(0, constraints[scenario[length(scenario)]+1,m+1]-1), m))}}
          scenario<-c(scenario, rep(0,sum(x)-length(scenario)))}}
      
      #Check if valid scenario
      
      if (identical(as.vector(x), freq_sc(scenario))==FALSE){scenario<-rep(NA, sum(x))}
  
  return(scenario)}

#Functions to filter scenarios in a matrix

filtertest<-function(x, constraints1=constraints){
  
  #x is a line of matrix of new scenarios
  
  index.mowing<-which(x==2)
  index.burning<-which(x==3)
  
  result<-0
  
  #Test 2-2
  
  if (length(index.mowing)>1 & result==0){
    diff1<-diff(index.mowing)
    if (sum(diff1<constraints1["mowing", "mowing"])>0){result<-1}}
  
  #Test 3-3
  
  if (length(index.burning)>1 & result==0){
    diff1<-diff(index.burning)
    if (sum(diff1<constraints1["burning", "burning"])>0){result<-1}}
  
  #Test 2-3
  
  if (length(index.mowing)>0 & length(index.burning)>0 & result==0){
    for (j in 1:length(index.mowing)){
      diff1<-index.burning-index.mowing[j]
      diff1<-diff1[diff1>=0]
      if (sum(diff1<constraints1["mowing", "burning"])>0){
        result<-1
        break}}}
  
  #Test 3-2
  
  if (length(index.mowing)>0 & length(index.burning)>0 & result==0){
    for (j in 1:length(index.burning)){
      diff1<-index.mowing-index.burning[j]
      diff1<-diff1[diff1>=0]
      if (sum(diff1<constraints1["burning", "mowing"])>0){
        result<-1
        break}}}
  
  return(result)}

#########

filter<-function(x){
  
  #x is a matrix of new scenarios
  
  #Find lines with problemetic scenarios
  test<-apply(x, 1, filtertest)
  
  #Remove lines
  if (1 %in% test) {x<-x[-which(test==1),]}
  
  return(x)}






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

create.scenario<-function(x, constraints, try){
  success<-FALSE
  t<-0
  while(success==FALSE){
    t<-t+1
    sc<-c(rep(0, x[1]),rep(1, x[2]),rep(2, x[3]),rep(3, x[4]),rep(4, x[5]))
    sc1<-sc[sc>0]
    if (length(sc1)==0){
      scenario<-sc
      success<-TRUE}
    else{
      l0<-length(sc1)
      scenario<-c()
      for (i in 1:l0){
        if (length(sc1)>1) {m<-resample(sc1, 1)} else {m<-sc1[1]}
        if (4 %in% scenario & m==4) {if ((length(scenario)+constraints[scenario[length(scenario)]+1,m+1])-which(scenario==4)[sum(scenario==4)]<constraints["choppering", "choppering"]){
          if (length(sc1[sc1!=4])==0){scenario<-NA}
          if (length(sc1[sc1!=4])>0){
            m<-resample(sc1[sc1!=4], 1)}}} 
        sc1<-sc1[-which(sc1==m)[1]]
        if (i==1) {scenario<-c(scenario,m)}
        if (FALSE %in% is.na(scenario) & i>1) {scenario<-c(scenario, c(rep(0, constraints[scenario[length(scenario)]+1,m+1]-1), m))}}
      if (FALSE %in% is.na(scenario) & length(scenario)<=sum(x)){success<-TRUE}}
    if (t>try){break}}
  if (t>try){scenario<-rep(NA, sum(x))}
  scenario<-c(scenario, rep(0,sum(x)-length(scenario)))
  return(scenario)}
freq_sc<-function(x){
  n<-length(x)
  freq0<-sum(x==0)/n*100
  freq1<-sum(x==1)/n*100
  freq2<-sum(x==2)/n*100
  freq3<-sum(x==3)/n*100
  freq4<-sum(x==4)/n*100
  freq5<-sum(x==5)/n*100
  result<-c(freq0, freq1, freq2, freq3, freq4, freq5)
  return(result)}

is.whole<-function(x){is.numeric(x) && floor(x)==x}
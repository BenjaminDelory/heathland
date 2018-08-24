scenarios<-cmpfun(function(management=c("grazing", "mowing", "burning", "choppering", "sodcutting"), 
                    constraints=import_constraints(), n=20, show.progress=TRUE, filename=NULL){
  
  if (is.matrix(constraints)==FALSE){stop("constraints must be a matrix containing model parameters")}
  if ("constraints" %in% class(constraints)){} else {stop("constraints must be a constraints object")}
  if (is.character(management)==FALSE){stop("management must be character")}
  for (i in 1:length(management)){if (management[i]=="grazing"|management[i]=="mowing"|management[i]=="burning"|management[i]=="choppering"|management[i]=="sodcutting") {} else {stop("Unknown management")}}
  if (is.numeric(n)==TRUE & length(n)==1 & n>0){} else {stop("n must be a single positive numeric value")}
  if (mode(show.progress)!="logical"){stop("show.progress must be logical")}
  if (is.null(filename)==FALSE & is.character(filename)==FALSE){stop("filename must be a character string")}

  #Create possible scenarios to test
  #0 is no management
  #1 is grazing
  #2 is mowing
  #3 is burning
  #4 is choppering
  #5 is sodcutting
  
  m<-length(management)
  
  if (show.progress==TRUE) {pb<-txtProgressBar(min=2, max=n, style=3)}
  
  options(bigmemory.typecast.warning=FALSE) #Remove warnings
  
  #Create big.matrix object
  scenarios<-big.matrix(nrow=m, ncol=n, type="char", shared=TRUE)

  if ("grazing" %in% management) {
    grazing<-matrix(ncol=max(constraints["grazing", management]), nrow=m)
    colgrazing<-ncol(grazing)
    graz<-1} else {graz<-NULL}
  if ("mowing" %in% management) {
    mowing<-matrix(ncol=max(constraints["mowing", management]), nrow=m)
    colmowing<-ncol(mowing)
    mow<-2} else {mow<-NULL}
  if ("burning" %in% management) {
    burning<-matrix(ncol=max(constraints["burning", management]), nrow=m)
    colburning<-ncol(burning)
    burn<-3} else {burn<-NULL}
  if ("choppering" %in% management) {
    choppering<-matrix(ncol=max(constraints["choppering", management]), nrow=m)
    colchoppering<-ncol(choppering)
    chop<-4} else {chop<-NULL}
  if ("sodcutting" %in% management) {
    sodcutting<-matrix(ncol=max(constraints["sodcutting", management]), nrow=m)
    colsodcutting<-ncol(sodcutting)
    sod<-5} else {sod<-NULL}
  
  #Fill matrices for each management
  for (i in 1:m){
    
    if ("grazing" %in% management) {
      if(management[i]=="grazing"){grazing[i,constraints["grazing",management[i]]]<-graz}
      if(management[i]=="mowing"){grazing[i,constraints["grazing",management[i]]]<-mow}
      if(management[i]=="burning"){grazing[i,constraints["grazing",management[i]]]<-burn}
      if(management[i]=="choppering"){grazing[i,constraints["grazing",management[i]]]<-chop}
      if(management[i]=="sodcutting"){grazing[i,constraints["grazing",management[i]]]<-sod}
      if (ncol(grazing)>1) {grazing[i,1:(constraints["grazing",management[i]]-1)]<-0}}
    
    if ("mowing" %in% management) {
      if(management[i]=="grazing"){mowing[i,constraints["mowing",management[i]]]<-graz}
      if(management[i]=="mowing"){mowing[i,constraints["mowing",management[i]]]<-mow}
      if(management[i]=="burning"){mowing[i,constraints["mowing",management[i]]]<-burn}
      if(management[i]=="choppering"){mowing[i,constraints["mowing",management[i]]]<-chop}
      if(management[i]=="sodcutting"){mowing[i,constraints["mowing",management[i]]]<-sod}
      if (ncol(mowing)>1) {mowing[i,1:(constraints["mowing",management[i]]-1)]<-0}}
    
    if ("burning" %in% management) {
      if(management[i]=="grazing"){burning[i,constraints["burning",management[i]]]<-graz}
      if(management[i]=="mowing"){burning[i,constraints["burning",management[i]]]<-mow}
      if(management[i]=="burning"){burning[i,constraints["burning",management[i]]]<-burn}
      if(management[i]=="choppering"){burning[i,constraints["burning",management[i]]]<-chop}
      if(management[i]=="sodcutting"){burning[i,constraints["burning",management[i]]]<-sod}
      if (ncol(burning)>1) {burning[i,1:(constraints["burning",management[i]]-1)]<-0}}
    
    if ("choppering" %in% management) {
      if(management[i]=="grazing"){choppering[i,constraints["choppering",management[i]]]<-graz}
      if(management[i]=="mowing"){choppering[i,constraints["choppering",management[i]]]<-mow}
      if(management[i]=="burning"){choppering[i,constraints["choppering",management[i]]]<-burn}
      if(management[i]=="choppering"){choppering[i,constraints["choppering",management[i]]]<-chop}
      if(management[i]=="sodcutting"){choppering[i,constraints["choppering",management[i]]]<-sod}
      if (ncol(choppering)>1) {choppering[i,1:(constraints["choppering",management[i]]-1)]<-0}}
    
    if ("sodcutting" %in% management) {
      if(management[i]=="grazing"){sodcutting[i,constraints["sodcutting",management[i]]]<-graz}
      if(management[i]=="mowing"){sodcutting[i,constraints["sodcutting",management[i]]]<-mow}
      if(management[i]=="burning"){sodcutting[i,constraints["sodcutting",management[i]]]<-burn}
      if(management[i]=="choppering"){sodcutting[i,constraints["sodcutting",management[i]]]<-chop}
      if(management[i]=="sodcutting"){sodcutting[i,constraints["sodcutting",management[i]]]<-sod}
      if (ncol(sodcutting)>1) {sodcutting[i,1:(constraints["sodcutting",management[i]]-1)]<-0}}}

  scenarios[,1]<-c(graz, mow, burn, chop, sod) 
  l<-nrow(scenarios)
  
  for (i in 2:n){#For each simulated year
    
    if (show.progress==TRUE) {setTxtProgressBar(pb, i)}
    
    index<-mwhich(x=scenarios, cols=i, vals=NA, "eq")
    lengthindex<-length(index)
    
    if (lengthindex==0){}
    
    else{
    
    scenarios1<-big.matrix(nrow=l+lengthindex*m, ncol=n, type="char", shared=TRUE)
    scenarios1[1:l,1:n]<-scenarios[1:l,1:n]
    scenarios<-scenarios1
    remove(scenarios1)
    
    newscenarios<-matrix(nrow=lengthindex*m, ncol=n)
    newl<-0
     
    for (j in 1:lengthindex){
      
      if (scenarios[index[j],i-1]==1) {
        if (n-(i-1)-colgrazing>0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  grazing,
                                                  matrix(NA, ncol=n-(i-1)-colgrazing, nrow=m))}
        if (n-(i-1)-colgrazing==0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  grazing)}
        if (n-(i-1)-colgrazing<0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                         grazing[,1:(n-(i-1))])}
        newl<-newl+m}
      
      if (scenarios[index[j],i-1]==2) {
        if (n-(i-1)-colmowing>0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  mowing,
                                                  matrix(NA, ncol=n-(i-1)-colmowing, nrow=m))}
        if (n-(i-1)-colmowing==0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  mowing)}
        if (n-(i-1)-colmowing<0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  mowing[,1:(n-(i-1))])}
        newl<-newl+m}
      
      if (scenarios[index[j],i-1]==3) {
        if (n-(i-1)-colburning>0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  burning,
                                                  matrix(NA, ncol=n-(i-1)-colburning, nrow=m))}
        if (n-(i-1)-colburning==0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  burning)}
        if (n-(i-1)-colburning<0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  burning[,1:(n-(i-1))])}
        newl<-newl+m}
      
      if (scenarios[index[j],i-1]==4) {
        if (n-(i-1)-colchoppering>0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  choppering,
                                                  matrix(NA, ncol=n-(i-1)-colchoppering, nrow=m))}
        if (n-(i-1)-colchoppering==0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  choppering)}
        if (n-(i-1)-colchoppering<0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  choppering[,1:(n-(i-1))])}
      
        newl<-newl+m}
      
      if (scenarios[index[j],i-1]==5) {
        if (n-(i-1)-colsodcutting>0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  sodcutting,
                                                  matrix(NA, ncol=n-(i-1)-colsodcutting, nrow=m))}
        if (n-(i-1)-colsodcutting==0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                  sodcutting)}
        if (n-(i-1)-colsodcutting<0){
          newscenarios[(newl+1):(newl+m),]<-cbind(matrix(scenarios[index[j],1:(i-1)], nrow=m, ncol=i-1, byrow=TRUE),
                                                sodcutting[,1:(n-(i-1))])}
      
        newl<-newl+m}
      
    }
    
    scenarios[(l+1):(l+nrow(newscenarios)), 1:n]<-newscenarios
    l<-l+nrow(newscenarios)
    
    mpermute(scenarios, order=c(index, c(1:l)[-index]))
    scenarios<-sub.big.matrix(scenarios, firstRow=1+lengthindex, lastRow=l,
                              firstCol=1, lastCol=n)
    l<-l-lengthindex}}
  
  if (is.null(filename)==FALSE){write.big.matrix(scenarios, filename=filename, row.names=F, col.names=F, sep=",")}
  
  message(paste("Total number of possible scenarios: ", l, sep=""))
  
  return(scenarios)})
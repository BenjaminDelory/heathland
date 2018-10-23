scenarios<-cmpfun(function(management=c("none", "grazing", "mowing", "burning", "choppering"), 
                    constraints=import_constraints(), n=100, filename=NULL, try=100){
  
  if (is.matrix(constraints)==FALSE){stop("constraints must be a matrix containing model parameters")}
  if ("constraints" %in% class(constraints)){} else {stop("constraints must be a constraints object")}
  if (is.character(management)==FALSE){stop("management must be character")}
  for (i in 1:length(management)){if (management[i]=="grazing"|management[i]=="mowing"|management[i]=="burning"|management[i]=="choppering"|management[i]=="none") {} else {stop("Unknown management")}}
  if (is.numeric(n)==TRUE & length(n)==1 & n>0){} else {stop("n must be a single positive numeric value")}
  if (is.numeric(try)==TRUE & length(try)==1 & try>0){} else {stop("try must be a single positive numeric value")}
  if (is.null(filename)==FALSE & is.character(filename)==FALSE){stop("filename must be a character string")}

  #Create possible scenarios to test
  #0 is no management
  #1 is grazing
  #2 is mowing
  #3 is burning
  #4 is choppering

  m<-length(management)
  
  #Order constraints
  constraints<-constraints[c("none", "grazing", "mowing", "burning", "choppering"),
                           c("none", "grazing", "mowing", "burning", "choppering")]
  
  #Create all management frequency distributions
  C1 <- t(restrictedparts(n, m, include.zero=TRUE))
  perm<-permn(management)
  C<-vector("list", length(perm))
  allmanagement<-c("none","grazing", "mowing", "burning", "choppering")
  for (i in 1:length(perm)){
    C[[i]]<-C1
    colnames(C[[i]])<-perm[[i]]
    if (m<5){
      C[[i]]<-cbind(C[[i]], matrix(0, ncol=5-m, nrow=nrow(C[[i]])))
      colnames(C[[i]])[(m+1):5]<-allmanagement[-match(management, allmanagement)]}
    C[[i]]<-C[[i]][,c("none", "grazing", "mowing", "burning", "choppering")]}
  C<-uniquecombs(do.call(rbind, C)) #Remove duplicated lines
  
  #First filter: for each management, remove combinations for which the frequency is too high based on existing constraints
  for (i in 1:m){
    index<-which(C[,i]>floor((n-1)/constraints[management[i], management[i]])+1)
    if (length(index)>0) {C<-C[-index,]}}
  
  #Second filter
  C<-C[-which(apply(C, 1, function(x){x[2]*min(constraints["grazing", 2:5])+x[3]*min(constraints["mowing", 2:5])+x[4]*min(constraints["burning", 2:5])+x[5]*min(constraints["choppering", 2:5])})>n),]
  
  #Create one potential scenario for each frequency distribution
  scenarios<-as.big.matrix(t(apply(C, 1, create.scenario, constraints=constraints, try=try)))
  
  #Remove lines with NA values
  scenarios<-as.big.matrix(scenarios[-mwhich(scenarios, cols=1, vals=NA, comps="eq"),])
  
  #Export results
  if (is.null(filename)==FALSE){write.big.matrix(scenarios, filename=filename, row.names=F, col.names=F, sep=",")}
  
  message(paste("Total number of scenarios: ", nrow(scenarios), sep=""))
  
  return(scenarios)})
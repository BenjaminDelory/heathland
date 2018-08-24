nuts<-cmpfun(function(scenarios, data=import_nutrients(), constraints=import_constraints(), 
                      sqa=list(grazing=1, mowing=5, burning=5, choppering=10, sodcutting=15),
                      nutrient="N", init=0){
  
  #Error interceptions
  if (is.big.matrix(scenarios)==FALSE){stop("scenarios must be a big.matrix containing possible scenarios")}
  if ("constraints" %in% class(constraints)){} else {stop("constraints must be a constraints object")}
  if (is.list(data)==FALSE){stop("data must be a list containing model parameters")}
  if ("nutrients" %in% class(data)){} else {stop("data must be a nutrients object")}
  if (is.list(sqa)==FALSE){stop("sqa must be a list")}
  if (nutrient=="N"|nutrient=="P"|nutrient=="K"|nutrient=="Ca"|nutrient=="Mg"){} else {stop("nutrient must be either N, P, K, Ca, or Mg")}
  if (is.numeric(init)==TRUE & length(init)==1 & init>=0) {} else {stop("init must be a single positive numeric value")}
  
  for (i in 1:length(sqa)){
    if (is.numeric(sqa[[i]])==TRUE & length(sqa[[i]])==1 & sqa[[i]]>0) {} else {stop("each sqa element must be a single positive numeric value")}
    if (sqa[[i]]>min(constraints[names(sqa)[i],2:5])){stop(paste("sqa for ", names(sqa)[i], " must be lower or equal to min(constraints)", sep=""))}}
  
  results<-as.big.matrix(t(apply(scenarios, 1, nutribal, data=data, sqa=sqa, nutrient=nutrient, init=init)))
  
  return(results)})
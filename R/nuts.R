nuts<-cmpfun(function(scenarios, data=import_nutrients(), sqa=list(grazing=1, mowing=5, burning=5, choppering=7),
                      nutrient="N", service="waterrecharge", init=0, ...){
  
  #Error interceptions
  if (is.big.matrix(scenarios)==FALSE){stop("scenarios must be a big.matrix containing possible scenarios")}
  if (is.list(data)==FALSE){stop("data must be a list containing model parameters")}
  if ("nutrients" %in% class(data)|"services" %in% class(data)){} else {stop("data must be a nutrients or services object")}
  if (is.list(sqa)==FALSE){stop("sqa must be a list")}
  if ("nutrients" %in% class(data)) {if (nutrient=="N"|nutrient=="P"|nutrient=="K"|nutrient=="Ca"|nutrient=="Mg"){} else {stop("nutrient must be either N, P, K, Ca, or Mg")}}
  if ("services" %in% class(data)) {if (service=="waterrecharge"|service=="carbon"|service=="cost"|service=="waterquality"|service=="appreciation"){} else {stop("service must be either waterrecharge, waterquality, carbon, cost, or appreciation")}}
  if (is.numeric(init)==TRUE & length(init)==1 & init>=0) {} else {stop("init must be a single positive numeric value")}
  
  message(paste("Total number of scenarios: ", nrow(scenarios), sep=""))
  
  if ("nutrients" %in% class(data)) {
    results<-as.big.matrix(t(apply(scenarios, 1, nutribal, data=data, sqa=sqa, nutrient=nutrient, ...)))
    message(paste("Total number of problematic scenarios: ", length(mwhich(x=results, cols=1, vals=NA, "eq")), sep=""))}
  
  if ("services" %in% class(data)) {results<-as.big.matrix(t(apply(scenarios, 1, ecoser, data=data, sqa=sqa, service=service, init=init, ...)))}
  
  return(results)})
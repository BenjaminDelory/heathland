import_scenarios<-function(path){
  
  #Error interceptions
  if (is.character(path)==FALSE){stop("path must be a character string")}
  
  #Import in a big.matrix object
  data<-read.big.matrix(filename=path, sep=",", header=FALSE, type="char", shared=TRUE)
  
  return(data)}
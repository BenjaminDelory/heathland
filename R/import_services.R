import_services<-function(path=NULL){
  
  #Error interceptions
  if (is.null(path)==FALSE & is.character(path)==FALSE){stop("If path is not NULL, it must be a character string")}
  
  #Locate extdata folder
  if (is.null(path)==TRUE){path<-system.file("extdata", package="heathland")}
  
  #create list
  data<-list(groundwater=as.matrix(read.table(paste(path, "groundwater.txt", sep="/"))))
  
  class(data)<-c("list", "services")
  
  return(data)}